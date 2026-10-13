(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2015 Frédéric Bour <frederic.bour(_)lakaban.net> Thomas
   Refis <refis.thomas(_)gmail.com> Simon Castellan <simon.castellan(_)iuwt.fr>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   The Software is provided "as is", without warranty of any kind, express or
   implied, including but not limited to the warranties of merchantability,
   fitness for a particular purpose and noninfringement. In no event shall the
   authors or copyright holders be liable for any claim, damages or other
   liability, whether in an action of contract, tort or otherwise, arising from,
   out of or in connection with the software or the use or other dealings in the
   Software.

   )* }}} *)

open Import
open Fiber.O
module Std = Merlin_utils.Std
module Misc = Ocaml_utils.Misc

let empty = Mconfig_dot.empty_config

type trace = message:(unit -> string) -> verbose:(unit -> string) -> unit Fiber.t

module Process = struct
  type nonrec t =
    { pid : Pid.t
    ; prog : string
    ; command : string
    ; initial_cwd : string
    ; trace : trace
    ; stdin : Lev_fiber.Io.output Lev_fiber.Io.t
    ; stdout : Lev_fiber.Io.input Lev_fiber.Io.t
    ; session : Lev_fiber_csexp.Session.t
    ; query_mutex : Fiber.Mutex.t
    ; mutable exited : bool
    ; mutable shutdown_timer : Lev_fiber.Timer.Wheel.task option
    }

  let waitpid t =
    let* status = Lev_fiber.waitpid ~pid:(Pid.to_int t.pid) in
    t.exited <- true;
    let shutdown_timer = t.shutdown_timer in
    t.shutdown_timer <- None;
    let* () =
      match shutdown_timer with
      | None -> Fiber.return ()
      | Some timer -> Lev_fiber.Timer.Wheel.cancel timer
    in
    (match status with
     | Unix.WEXITED n ->
       (match n with
        | 0 -> ()
        | n -> Format.eprintf "%s finished with code = %d@.%!" t.prog n)
     | WSIGNALED s -> Format.eprintf "%s finished signal = %d@.%!" t.prog s
     | WSTOPPED _ -> ());
    Lev_fiber.Io.close t.stdin;
    Lev_fiber.Io.close t.stdout;
    t.trace
      ~message:(fun () -> "Merlin configuration process exited")
      ~verbose:(fun () ->
        let status =
          match status with
          | Unix.WEXITED code -> sprintf "exited with code %d" code
          | WSIGNALED signal -> sprintf "terminated by signal %d" signal
          | WSTOPPED signal -> sprintf "stopped by signal %d" signal
        in
        sprintf
          "Command: %s; pid: %d; cwd: %s; status: %s"
          t.command
          (Pid.to_int t.pid)
          t.initial_cwd
          status)
  ;;

  let await_exit t wheel =
    if t.exited
    then Fiber.return true
    else
      let* timer = Lev_fiber.Timer.Wheel.task wheel in
      if t.exited
      then
        let+ () = Lev_fiber.Timer.Wheel.cancel timer in
        true
      else (
        t.shutdown_timer <- Some timer;
        let+ (_ : [ `Ok | `Cancelled ]) = Lev_fiber.Timer.Wheel.await timer in
        t.shutdown_timer <- None;
        t.exited)
  ;;

  let send_signal t signal =
    if t.exited
    then Fiber.return ()
    else (
      match Unix.kill (Pid.to_int t.pid) signal with
      | exception Unix.Unix_error (Unix.ESRCH, _, _) -> Fiber.return ()
      | () ->
        t.trace
          ~message:(fun () -> "Signal sent to Merlin configuration process")
          ~verbose:(fun () ->
            sprintf "Command: %s; pid: %d; signal: %d" t.command (Pid.to_int t.pid) signal))
  ;;

  let start ~trace ~dir =
    let bin, args =
      (* the convention is that $OCAMLLSP_PROJECT_BUILD_SYSTEM executable has
         `ocaml-merlin` subcommand to start a merlin configuration server *)
      match Sys.getenv_opt "OCAMLLSP_PROJECT_BUILD_SYSTEM" with
      | None -> "dune", [ "ocaml-merlin"; "--no-print-directory" ]
      | Some bin -> bin, [ "ocaml-merlin" ]
    in
    match Bin.which bin with
    | None -> Fiber.return (Error (Printf.sprintf "%s binary not found" bin))
    | Some prog ->
      let command = String.concat ~sep:" " (prog :: args) in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      Unix.set_close_on_exec stdin_w;
      let pid =
        Spawn.spawn
          ~cwd:(Path dir)
          ~prog
          ~argv:(prog :: args)
          ~stdin:stdin_r
          ~stdout:stdout_w
          ()
        |> Pid.of_int
      in
      Unix.close stdin_r;
      Unix.close stdout_w;
      let blockity =
        if Sys.win32
        then `Blocking
        else (
          Unix.set_nonblock stdin_w;
          Unix.set_nonblock stdout_r;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* stdin = make stdin_w Output in
      let* stdout = make stdout_r Input in
      let session = Lev_fiber_csexp.Session.create ~socket:false stdout stdin in
      let process =
        { prog
        ; command
        ; pid
        ; initial_cwd = dir
        ; trace
        ; stdin
        ; stdout
        ; session
        ; query_mutex = Fiber.Mutex.create ()
        ; exited = false
        ; shutdown_timer = None
        }
      in
      let+ () =
        trace
          ~message:(fun () -> "Merlin configuration process started")
          ~verbose:(fun () ->
            sprintf "Command: %s; pid: %d; cwd: %s" command (Pid.to_int pid) dir)
      in
      Ok process
  ;;
end

module Dot_protocol_io =
  Merlin_dot_protocol.Make
    (Fiber)
    (struct
      include Lev_fiber_csexp.Session

      type in_chan = t
      type out_chan = t

      let read t =
        let open Fiber.O in
        let+ opt = read t in
        match opt with
        | Some r -> Result.return r
        | None -> Error "Read error"
      ;;

      let write t x = write t [ x ]
    end)

type _ process_request =
  | Halt : unit process_request
  | File :
      string
      -> (Merlin_dot_protocol.directive list, Merlin_dot_protocol.read_error) result
           process_request
  | File_configurations :
      string
      -> ( Merlin_dot_protocol.configuration Merlin_dot_protocol.Nonempty_list.t
           , Merlin_dot_protocol.configurations_error )
           result
           process_request

let process_request_path : type response. response process_request -> string = function
  | Halt -> "<halt>"
  | File path | File_configurations path -> path
;;

let execute_process_request (type response) session (request : response process_request)
  : response Fiber.t
  =
  match request with
  | Halt -> Dot_protocol_io.Commands.halt session
  | File path ->
    let* () = Dot_protocol_io.Commands.send_file session path in
    Dot_protocol_io.read session
  | File_configurations path ->
    let* request = Dot_protocol_io.Commands.send_file_configurations session path in
    Dot_protocol_io.read_configurations ~request session
;;

let process_query
      (type response)
      (process : Process.t)
      (request : response process_request)
  : response Fiber.t
  =
  let { Process.query_mutex; trace; command; pid; session; _ } = process in
  let path = process_request_path request in
  Fiber.Mutex.with_lock query_mutex ~f:(fun () ->
    let* () =
      trace
        ~message:(fun () -> "Merlin configuration request sent")
        ~verbose:(fun () ->
          sprintf "Command: %s; pid: %d; file: %s" command (Pid.to_int pid) path)
    in
    let* response = execute_process_request session request in
    let+ () =
      trace
        ~message:(fun () -> "Merlin configuration response received")
        ~verbose:(fun () ->
          sprintf "Command: %s; pid: %d; file: %s" command (Pid.to_int pid) path)
    in
    response)
;;

let prefer_dot_merlin = ref false

type db =
  { running : (string, entry) Hashtbl.t
  ; pool : Fiber.Pool.t
  ; trace : trace
  ; process_mutex : Fiber.Mutex.t
  }

and entry =
  { db : db
  ; process : Process.t
  ; mutable ref_count : int
  ; mutable stopping : bool
  }

module Entry = struct
  type t = entry

  let create db process = { db; process; ref_count = 0; stopping = false }
  let equal = ( == )
  let incr t = t.ref_count <- t.ref_count + 1

  let stop t =
    if t.stopping
    then Fiber.return ()
    else (
      t.stopping <- true;
      Hashtbl.remove t.db.running t.process.initial_cwd;
      let* () =
        t.process.trace
          ~message:(fun () -> "Stopping Merlin configuration process")
          ~verbose:(fun () ->
            sprintf
              "Command: %s; pid: %d; cwd: %s"
              t.process.command
              (Pid.to_int t.process.pid)
              t.process.initial_cwd)
      in
      let+ () = process_query t.process Halt in
      (* Do not leave a process that handled [Halt] blocked waiting for more input. *)
      Lev_fiber.Io.close t.process.stdin)
  ;;

  let destroy (t : t) =
    assert (t.ref_count > 0);
    t.ref_count <- t.ref_count - 1;
    if t.ref_count > 0 then Fiber.return () else stop t
  ;;
end

let get_process t ~dir =
  Fiber.Mutex.with_lock t.process_mutex ~f:(fun () ->
    match Hashtbl.find t.running dir with
    | Some p -> Fiber.return (Ok p)
    | None ->
      let* process = Process.start ~trace:t.trace ~dir in
      (match process with
       | Error _ as error -> Fiber.return error
       | Ok process ->
         let entry = Entry.create t process in
         Hashtbl.add_exn t.running ~key:dir ~data:entry;
         let+ () = Fiber.Pool.task t.pool ~f:(fun () -> Process.waitpid process) in
         Ok entry))
;;

type context =
  { workdir : string
  ; process_dir : string
  }

type mode =
  | Ocaml
  | Melange
  | Other of string

let mode_of_string = function
  | "ocaml" -> Ocaml
  | "melange" -> Melange
  | mode -> Other mode
;;

let mode_key = function
  | Ocaml -> "ocaml"
  | Melange -> "melange"
  | Other mode -> mode
;;

let mode_label = function
  | Ocaml -> "OCaml"
  | Melange -> "Melange"
  | Other mode -> mode
;;

type source_kind =
  | Implementation
  | Interface

type configuration_origin =
  | Plural of
      { mode : mode
      ; kind : source_kind
      ; counterpart : Uri.t option
      }
  | Legacy_file

type configuration =
  { origin : configuration_origin
  ; is_default : bool
  ; config : Mconfig.t
  }

type configuration_set = configuration Merlin_dot_protocol.Nonempty_list.t
type error = string list

let configuration_mode (configuration : configuration) =
  match configuration.origin with
  | Legacy_file -> None
  | Plural { mode; _ } -> Some mode
;;

let configuration_label configuration =
  configuration_mode configuration |> Option.value_map ~default:"legacy" ~f:mode_label
;;

let nonempty_to_list = Merlin_dot_protocol.Nonempty_list.to_list

let configuration_list (configurations : configuration_set) =
  nonempty_to_list configurations
;;

let singleton configuration = Merlin_dot_protocol.Nonempty_list.create configuration []

let map_nonempty configurations ~f =
  match nonempty_to_list configurations with
  | first :: rest -> Merlin_dot_protocol.Nonempty_list.create (f first) (List.map rest ~f)
  | [] -> invalid_arg "Merlin_config.map_nonempty"
;;

let primary (configurations : configuration_set) =
  configuration_list configurations
  |> List.find ~f:(fun (configuration : configuration) -> configuration.is_default)
  |> Option.value ~default:(List.hd_exn (configuration_list configurations))
;;

let find_mode (configurations : configuration_set) mode =
  configuration_list configurations
  |> List.find ~f:(fun (configuration : configuration) ->
    match configuration.origin with
    | Legacy_file -> false
    | Plural origin -> String.equal (mode_key origin.mode) (mode_key mode))
;;

let legacy_configuration config =
  Merlin_dot_protocol.Nonempty_list.create
    { origin = Legacy_file; is_default = true; config }
    []
;;

let protocol_error_message p = function
  | Merlin_dot_protocol.Unexpected_output msg -> msg
  | Csexp_parse_error _ ->
    Printf.sprintf
      "ocamllsp could not load its configuration from the external reader. Building your \
       project with `%s` might solve this issue."
      p.Process.prog
;;

let relative_query_path (p : Process.t) path_abs =
  (* Both [p.initial_cwd] and [path_abs] have gone through
     [canonicalize_filename] *)
  let path_rel =
    String.chop_prefix ~prefix:p.initial_cwd path_abs
    |> Option.map ~f:(fun path ->
      (* We need to remove the leading path separator after chopping. There
         is one case where no separator is left: when [initial_cwd] was the
         root of the filesystem *)
      if (not (String.is_empty path)) && path.[0] = Filename.dir_sep.[0]
      then String.drop_prefix path 1
      else path)
  in
  Option.value path_rel ~default:path_abs
;;

let query_file (p : Process.t) path = process_query p (File path)

let get_legacy_config (p : Process.t) ~workdir path_abs =
  let path = relative_query_path p path_abs in
  (* Starting with Dune 2.8.3 relative paths are prefered. However to maintain
     compatibility with 2.8 <= Dune <= 2.8.2 we always retry with an absolute
     path if using a relative one failed *)
  let+ answer =
    let* query_path = query_file p path in
    match query_path with
    | Ok [ `ERROR_MSG _ ] when not (String.equal path path_abs) -> query_file p path_abs
    | answer -> Fiber.return answer
  in
  match answer with
  | Ok directives ->
    let cfg, failures =
      Mconfig_dot.prepend_config
        ~dir:workdir
        Mconfig_dot.Configurator.Dune
        directives
        empty
    in
    Ok (Mconfig_dot.postprocess_config cfg, failures)
  | Error error -> Error [ protocol_error_message p error ]
;;

let canonical_uri path = Source_path.of_path path

let get_configurations (p : Process.t) ~workdir path_abs =
  let path = relative_query_path p path_abs in
  let query_plural () = process_query p (File_configurations path) in
  let legacy () =
    let+ result = get_legacy_config p ~workdir path_abs in
    Result.map result ~f:(fun (dot, failures) -> `Legacy (dot, failures))
  in
  let+ response =
    let* response = query_plural () in
    match response with
    | Error Merlin_dot_protocol.Unsupported -> legacy ()
    | Error (Server_error message) -> Fiber.return (Error [ message ])
    | Error (Protocol_error error) ->
      Fiber.return (Error [ protocol_error_message p error ])
    | Ok configurations -> Fiber.return (Ok (`Plural configurations))
  in
  Result.map response ~f:(function
    | `Legacy (dot, failures) -> `Legacy (dot, failures)
    | `Plural configurations ->
      let make configuration =
        let directives = Merlin_dot_protocol.configuration_directives configuration in
        let cfg, failures =
          Mconfig_dot.prepend_config
            ~dir:workdir
            Mconfig_dot.Configurator.Dune
            directives
            empty
        in
        let kind =
          match configuration.kind with
          | Merlin_dot_protocol.Implementation -> Implementation
          | Interface -> Interface
        in
        let counterpart = Option.map configuration.counterpart ~f:canonical_uri in
        ( Plural { mode = mode_of_string configuration.mode; kind; counterpart }
        , configuration.is_default
        , Mconfig_dot.postprocess_config cfg
        , failures )
      in
      `Plural (map_nonempty configurations ~f:make))
;;

let file_exists fname =
  match Unix.stat fname with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | s -> s.st_kind <> S_DIR
;;

let check_project_root_markers ~workdir ~dir markers =
  List.find_map markers ~f:(fun f ->
    let fname = Filename.concat dir f in
    if file_exists fname
    then (
      let workdir = Misc.canonicalize_filename (Option.value ~default:dir workdir) in
      Some ({ workdir; process_dir = dir }, fname))
    else None)
;;

let find_dune_project_context start_dir =
  (* The workdir is the first directory we find which contains a [dune] file. We
     need to keep track of this folder because [dune ocaml-merlin] might be
     started from a folder that is a parent of the [workdir]. Thus we cannot
     always use that starting folder as the workdir. *)
  let map_workdir dir = function
    | Some dir -> Some dir
    | None ->
      (* XXX what's ["dune-file"]? *)
      let fnames = List.map ~f:(Filename.concat dir) [ "dune"; "dune-file" ] in
      Option.some_if (List.exists ~f:file_exists fnames) dir
  in
  let rec loop ~workdir ~dir =
    match
      check_project_root_markers [ "dune-project"; "dune-workspace" ] ~workdir ~dir
    with
    | Some s -> Some s
    | None ->
      let parent = Filename.dirname dir in
      if parent <> dir
      then (
        (* Was this directory the workdir ? *)
        let workdir = map_workdir dir workdir in
        loop ~workdir ~dir:parent)
      else None
  in
  loop ~workdir:None ~dir:start_dir
;;

let find_project_context start_dir =
  match Sys.getenv_opt "OCAMLLSP_PROJECT_ROOT" with
  | Some dir ->
    let dir = Misc.canonicalize_filename dir in
    Some ({ workdir = dir; process_dir = dir }, "<merlin-config>")
  | None -> find_dune_project_context start_dir
;;

type nonrec t =
  { path : string
  ; directory : string
  ; initial : Mconfig.t
  ; mutable entry : Entry.t option
  ; db : db
  ; mutex : Fiber.Mutex.t
  }

let destroy_unlocked t =
  let* () = Fiber.return () in
  match t.entry with
  | None -> Fiber.return ()
  | Some entry ->
    t.entry <- None;
    Entry.destroy entry
;;

let destroy t = Fiber.Mutex.with_lock t.mutex ~f:(fun () -> destroy_unlocked t)

let create db path =
  let path =
    let path = Uri.to_path path in
    Source_path.canonicalize path
  in
  let directory = Filename.dirname path in
  let initial =
    let filename = Filename.basename path in
    let init = Mconfig.initial in
    { init with
      query = { init.query with filename; directory; verbosity = Mconfig.Verbosity.Smart }
    }
  in
  { path; directory; initial; db; entry = None; mutex = Fiber.Mutex.create () }
;;

let load_configurations (t : t) : (configuration_set, error) result Fiber.t =
  let use_entry entry =
    Entry.incr entry;
    t.entry <- Some entry
  in
  let* () = Fiber.return () in
  if !prefer_dot_merlin
  then
    Mconfig.get_external_config t.path t.initial
    |> legacy_configuration
    |> Result.return
    |> Fiber.return
  else (
    match find_project_context t.directory with
    | None ->
      let+ () = destroy_unlocked t in
      Result.return (legacy_configuration (Mconfig.get_external_config t.path t.initial))
    | Some (ctx, config_path) ->
      let* entry = get_process t.db ~dir:ctx.process_dir in
      (match entry with
       | Error failure ->
         let+ () = destroy_unlocked t in
         Error [ failure ]
       | Ok entry ->
         let* () =
           match t.entry with
           | None ->
             use_entry entry;
             Fiber.return ()
           | Some entry' ->
             if Entry.equal entry entry'
             then Fiber.return ()
             else
               let+ () = destroy_unlocked t in
               use_entry entry
         in
         let+ loaded = get_configurations entry.process ~workdir:ctx.workdir t.path in
         Result.map loaded ~f:(fun loaded ->
           let merge dot failures =
             let merlin =
               Mconfig.merge_merlin_config dot t.initial.merlin ~failures ~config_path
             in
             Mconfig.normalize { t.initial with merlin }
           in
           match loaded with
           | `Legacy (dot, failures) -> legacy_configuration (merge dot failures)
           | `Plural loaded ->
             let make (origin, is_default, dot, failures) =
               { origin; is_default; config = merge dot failures }
             in
             map_nonempty loaded ~f:make)))
;;

let configurations t = Fiber.Mutex.with_lock t.mutex ~f:(fun () -> load_configurations t)

let config_with_failures t failures =
  let merlin =
    Mconfig.merge_merlin_config
      empty
      t.initial.merlin
      ~failures
      ~config_path:"<merlin-config>"
  in
  Mconfig.normalize { t.initial with merlin }
;;

let config (t : t) : Mconfig.t Fiber.t =
  let+ configurations = configurations t in
  match configurations with
  | Ok configurations -> (primary configurations).config
  | Error failures -> config_with_failures t failures
;;

module DB = struct
  type t = db

  let get t uri = create t uri

  let create ~trace =
    { running = Hashtbl.create (module String)
    ; pool = Fiber.Pool.create ()
    ; trace
    ; process_mutex = Fiber.Mutex.create ()
    }
  ;;

  let run t = Fiber.Pool.run t.pool

  let stop t =
    let running =
      Hashtbl.fold t.running ~init:[] ~f:(fun ~key:_ ~data acc -> data :: acc)
    in
    match running with
    | [] -> Fiber.Pool.stop t.pool
    | running ->
      (* Give the protocol-level halt a chance to complete before escalating to
         signals. SIGKILL is necessary because Dune can remain blocked waiting
         for protocol input after SIGTERM. *)
      let* wheel = Lev_fiber.Timer.Wheel.create ~delay:0.5 in
      let signal_after_timeout signal =
        Fiber.parallel_iter running ~f:(fun entry ->
          let* exited = Process.await_exit entry.process wheel in
          if exited then Fiber.return () else Process.send_signal entry.process signal)
      in
      let shutdown () =
        let* () = Fiber.parallel_iter running ~f:Entry.stop in
        let* () = signal_after_timeout (if Sys.win32 then Sys.sigkill else Sys.sigterm) in
        let* () =
          if Sys.win32 then Fiber.return () else signal_after_timeout Sys.sigkill
        in
        Fiber.Pool.stop t.pool
      in
      Fiber.fork_and_join_unit
        (fun () -> Lev_fiber.Timer.Wheel.run wheel)
        (fun () ->
           Fiber.finalize shutdown ~finally:(fun () -> Lev_fiber.Timer.Wheel.stop wheel))
  ;;
end
