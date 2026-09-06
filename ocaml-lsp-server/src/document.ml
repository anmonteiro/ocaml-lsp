open! Import
open Fiber.O

module Kind = struct
  type t =
    | Intf
    | Impl

  let of_fname_opt p =
    match Filename.extension p with
    | ".ml" | ".eliom" | ".re" | ".mll" | ".mly" | ".mlx" -> Some Impl
    | ".mli" | ".eliomi" | ".rei" -> Some Intf
    | _ -> None
  ;;

  let unsupported uri =
    let p = Uri.to_path uri in
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:"unsupported file extension"
         ~data:(`Assoc [ "extension", `String (Filename.extension p) ])
         ())
  ;;
end

module Syntax = struct
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir
    | Cram
    | Dune
    | Mlx

  let human_name = function
    | Ocaml -> "OCaml"
    | Reason -> "Reason"
    | Ocamllex -> "OCamllex"
    | Menhir -> "Menhir/ocamlyacc"
    | Cram -> "Cram"
    | Dune -> "Dune"
    | Mlx -> "OCaml.mlx"
  ;;

  let all =
    [ "ocaml.interface", Ocaml
    ; "ocaml", Ocaml
    ; "reason", Reason
    ; "ocaml.ocamllex", Ocamllex
    ; "ocaml.menhir", Menhir
    ; "cram", Cram
    ; "dune", Dune
    ; "dune-project", Dune
    ; "dune-workspace", Dune
    ; "ocaml.mlx", Mlx
    ]
  ;;

  let of_fname =
    let of_fname_res = function
      | "dune" | "dune-workspace" | "dune-project" -> Ok Dune
      | s ->
        (match Filename.extension s with
         | ".eliomi" | ".eliom" | ".mli" | ".ml" -> Ok Ocaml
         | ".mlx" -> Ok Mlx
         | ".rei" | ".re" -> Ok Reason
         | ".mll" -> Ok Ocamllex
         | ".mly" -> Ok Menhir
         | ".t" -> Ok Cram
         | ext -> Error ext)
    in
    fun s ->
      match of_fname_res s with
      | Ok x -> x
      | Error ext ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:InvalidRequest
             ~message:(Printf.sprintf "unsupported file extension")
             ~data:(`Assoc [ "extension", `String ext ])
             ())
  ;;

  let to_language_id x = List.find_map_exn all ~f:(fun (k, v) -> Option.some_if (v = x) k)

  let markdown_name = function
    | Ocaml -> "ocaml"
    | Reason -> "reason"
    | s -> to_language_id s
  ;;

  let of_text_document (td : Text_document.t) =
    match List.Assoc.find all (Text_document.languageId td) ~equal:String.equal with
    | Some s -> s
    | None -> Text_document.documentUri td |> Uri.to_path |> of_fname
  ;;
end

let await task =
  let* cancel_token = Server.cancel_token () in
  let f () = Lev_fiber.Thread.await task in
  let without_cancellation res =
    match res with
    | Ok s -> Ok s
    | Error (`Exn exn) -> Error exn
    | Error `Cancelled ->
      let exn = Failure "unexpected cancellation" in
      let backtrace = Printexc.get_callstack 10 in
      Error { Exn_with_backtrace.exn; backtrace }
  in
  match cancel_token with
  | None -> f () |> Fiber.map ~f:without_cancellation
  | Some t ->
    let+ res, outcome =
      Fiber.Cancel.with_handler t f ~on_cancel:(fun () -> Lev_fiber.Thread.cancel task)
    in
    (match outcome with
     | Not_cancelled -> without_cancellation res
     | Cancelled () ->
       let e =
         Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" ()
       in
       raise (Jsonrpc.Response.Error.E e))
;;

module Single_pipeline : sig
  type t

  val create : Lev_fiber.Thread.t -> t

  val use
    :  ?name:string
    -> t
    -> doc:Text_document.t
    -> config:Merlin_config.t
    -> f:(Mpipeline.t -> 'a)
    -> ('a, Exn_with_backtrace.t) result Fiber.t

  val use_with_config
    :  ?name:string
    -> t
    -> doc:Text_document.t
    -> config:Mconfig.t
    -> f:(Mpipeline.t -> 'a)
    -> ('a, Exn_with_backtrace.t) result Fiber.t
end = struct
  type t = { thread : Lev_fiber.Thread.t } [@@unboxed]

  let create thread = { thread }

  let use_with_config ?name t ~doc ~config ~f =
    let make_pipeline =
      let source = Msource.make (Text_document.text doc) in
      fun () -> Mpipeline.make config source
    in
    let task =
      match
        Lev_fiber.Thread.task t.thread ~f:(fun () ->
          let start = Unix.gettimeofday () in
          let pipeline = make_pipeline () in
          let res = Mpipeline.with_pipeline pipeline (fun () -> f pipeline) in
          let stop = Unix.gettimeofday () in
          res, start, stop)
      with
      | Error `Stopped -> assert false
      | Ok task -> task
    in
    let* res = await task in
    match res with
    | Error exn -> Fiber.return (Error exn)
    | Ok (res, start, stop) ->
      let event =
        let module Event = Chrome_trace.Event in
        let dur = Event.Timestamp.of_float_seconds (stop -. start) in
        let fields =
          let name = Option.value name ~default:"unknown" in
          Event.common_fields
            ~cat:[ "merlin" ]
            ~ts:(Event.Timestamp.of_float_seconds start)
            ~name
            ()
        in
        Event.complete ~dur fields
      in
      let+ () = Metrics.report event in
      Ok res
  ;;

  let use ?name t ~doc ~config ~f =
    let* config = Merlin_config.config config in
    use_with_config ?name t ~doc ~config ~f
  ;;
end

type merlin =
  { tdoc : Text_document.t
  ; pipeline : Single_pipeline.t
  ; timer : Lev_fiber.Timer.Wheel.task
  ; merlin_config : Merlin_config.t
  ; fixed_configuration : Merlin_config.configuration option
  ; syntax : Syntax.t
  ; kind : Kind.t option
  }

module Dune = struct
  type t = Text_document.t

  let uri = Text_document.documentUri
  let text = Text_document.text
end

type t =
  | Dune of Dune.t
  | Other of
      { tdoc : Text_document.t
      ; syntax : Syntax.t
      }
  | Merlin of merlin

let text_document = function
  | Dune d -> d
  | Other d -> d.tdoc
  | Merlin m -> m.tdoc
;;

let uri t = Text_document.documentUri (text_document t)

let syntax = function
  | Dune _ -> Syntax.Dune
  | Merlin m -> m.syntax
  | Other t -> t.syntax
;;

let text t = Text_document.text (text_document t)
let source t = Msource.make (text t)
let version t = Text_document.version (text_document t)

let make_merlin wheel merlin_db pipeline tdoc syntax =
  let* timer = Lev_fiber.Timer.Wheel.task wheel in
  let uri = Text_document.documentUri tdoc in
  let path = Uri.to_path uri in
  let merlin_config = Merlin_config.DB.get merlin_db uri in
  let kind = Kind.of_fname_opt path in
  Fiber.return
    (Merlin
       { merlin_config; fixed_configuration = None; tdoc; pipeline; timer; syntax; kind })
;;

let make wheel config pipeline (doc : DidOpenTextDocumentParams.t) ~position_encoding =
  Fiber.of_thunk (fun () ->
    let tdoc = Text_document.make ~position_encoding doc in
    let syntax = Syntax.of_text_document tdoc in
    match syntax with
    | Ocaml | Reason | Mlx -> make_merlin wheel config pipeline tdoc syntax
    | Dune -> Fiber.return (Dune tdoc)
    | Ocamllex | Menhir | Cram -> Fiber.return (Other { tdoc; syntax }))
;;

let update_text ?version t changes =
  match Text_document.apply_content_changes ?version (text_document t) changes with
  | exception Text_document.Invalid_utf error ->
    Log.log ~section:"warning" (fun () ->
      let error =
        match error with
        | Malformed input ->
          [ "message", `String "malformed input"; "input", `String input ]
        | Insufficient_input -> [ "message", `String "insufficient input" ]
      in
      Log.msg
        "dropping update due to invalid utf8"
        (( "changes"
         , Json.yojson_of_list TextDocumentContentChangeEvent.yojson_of_t changes )
         :: error));
    t
  | tdoc ->
    (match t with
     | Dune _ -> Dune tdoc
     | Other o -> Other { o with tdoc }
     | Merlin t -> Merlin { t with tdoc })
;;

let dune = function
  | Dune d -> Some d
  | Other _ | Merlin _ -> None
;;

module Merlin = struct
  type t = merlin

  type configuration_context =
    { configurations : Merlin_config.configuration_set
    ; kind : Kind.t
    }

  type 'a configured_result =
    { configuration : Merlin_config.configuration
    ; result : ('a, Exn_with_backtrace.t) result
    }

  let to_doc t = Merlin t
  let source t = Msource.make (text (Merlin t))
  let timer (t : t) = t.timer

  let legacy_kind t config =
    let path = Uri.to_path (Text_document.documentUri t.tdoc) in
    let ext = Filename.extension path in
    match
      List.find_map config.Mconfig.merlin.suffixes ~f:(fun (impl, intf) ->
        if String.equal ext intf
        then Some Kind.Intf
        else if String.equal ext impl
        then Some Kind.Impl
        else None)
    with
    | Some kind -> Ok kind
    | None ->
      (match Kind.of_fname_opt path with
       | Some kind -> Ok kind
       | None -> Error [ "unsupported file extension: " ^ ext ])
  ;;

  let kind (t : t) =
    match t.fixed_configuration with
    | Some { origin = Plural { kind = Implementation; _ }; _ } -> Kind.Impl
    | Some { origin = Plural { kind = Interface; _ }; _ } -> Kind.Intf
    | Some { origin = Legacy_file; config; _ } ->
      (match legacy_kind t config with
       | Ok kind -> kind
       | Error _ -> Kind.unsupported (Text_document.documentUri t.tdoc))
    | None ->
      (match t.kind with
       | Some kind -> kind
       | None -> Kind.unsupported (Text_document.documentUri t.tdoc))
  ;;

  let plural_kind configurations =
    let kinds =
      Merlin_config.configuration_list configurations
      |> List.filter_map ~f:(fun configuration ->
        match configuration.Merlin_config.origin with
        | Legacy_file -> None
        | Plural { kind; _ } -> Some kind)
    in
    match kinds with
    | [] -> None
    | first :: rest ->
      if List.for_all rest ~f:(fun kind -> kind = first)
      then
        Some
          (Ok
             (match first with
              | Merlin_config.Implementation -> Kind.Impl
              | Interface -> Kind.Intf))
      else Some (Error [ "Merlin configurations disagree on the source kind" ])
  ;;

  let configuration_context_of_set t configurations =
    let kind =
      match plural_kind configurations with
      | Some kind -> kind
      | None -> legacy_kind t (Merlin_config.primary configurations).config
    in
    Result.map kind ~f:(fun kind -> { configurations; kind })
  ;;

  let configuration_context t =
    match t.fixed_configuration with
    | Some configuration ->
      Fiber.return
        (configuration_context_of_set t (Merlin_config.singleton configuration))
    | None ->
      let+ configurations = Merlin_config.configurations t.merlin_config in
      Result.bind configurations ~f:(configuration_context_of_set t)
  ;;

  let configuration_context_exn t =
    let+ context = configuration_context t in
    match context with
    | Ok context -> context
    | Error messages ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:RequestFailed
           ~message:(String.concat ~sep:"\n" messages)
           ())
  ;;

  let with_pipeline ?name (t : t) f =
    match t.fixed_configuration with
    | None -> Single_pipeline.use ?name t.pipeline ~doc:t.tdoc ~config:t.merlin_config ~f
    | Some configuration ->
      Single_pipeline.use_with_config
        ?name
        t.pipeline
        ~doc:t.tdoc
        ~config:configuration.config
        ~f
  ;;

  let with_configurable_pipeline ?name ~config (t : t) f =
    Single_pipeline.use_with_config ?name t.pipeline ~doc:t.tdoc ~config ~f
  ;;

  let with_configuration ?name (t : t) configuration f =
    with_configurable_pipeline ?name ~config:configuration.Merlin_config.config t f
  ;;

  let with_configurations ?name (t : t) ~configurations f =
    let rec loop acc = function
      | [] -> Fiber.return (List.rev acc)
      | configuration :: configurations ->
        let name =
          Option.map name ~f:(fun name ->
            sprintf "%s (%s)" name (Merlin_config.configuration_label configuration))
        in
        let* result = with_configuration ?name t configuration (f configuration) in
        loop ({ configuration; result } :: acc) configurations
    in
    match Merlin_config.configuration_list configurations with
    | first :: rest ->
      let+ results = loop [] (first :: rest) in
      (match results with
       | first :: rest -> Merlin_dot_protocol.Nonempty_list.create first rest
       | [] -> invalid_arg "Document.Merlin.with_configurations")
    | [] -> invalid_arg "Document.Merlin.with_configurations"
  ;;

  let mconfig (t : t) =
    match t.fixed_configuration with
    | None -> Merlin_config.config t.merlin_config
    | Some configuration -> Fiber.return configuration.config
  ;;

  let fixed_configuration t = t.fixed_configuration

  let with_pipeline_exn ?name doc f =
    let+ res = with_pipeline ?name doc f in
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let with_configurable_pipeline_exn ?name ~config doc f =
    let+ res = with_configurable_pipeline ?name ~config doc f in
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let dispatch ?name t command =
    with_pipeline ?name t (fun pipeline -> Query_commands.dispatch pipeline command)
  ;;

  let dispatch_exn ?name t command =
    with_pipeline_exn ?name t (fun pipeline -> Query_commands.dispatch pipeline command)
  ;;

  let dispatch_all ?name t ~configurations command =
    with_configurations ?name t ~configurations (fun _ pipeline ->
      Query_commands.dispatch pipeline command)
  ;;

  let doc_comment pipeline pos =
    let res =
      let command = Query_protocol.Document (None, pos) in
      Query_commands.dispatch pipeline command
    in
    match res with
    | `Found s | `Builtin s -> Some s
    | _ -> None
  ;;

  let syntax_doc pipeline pos =
    let res =
      let command = Query_protocol.Syntax_document pos in
      Query_commands.dispatch pipeline command
    in
    match res with
    | `Found s -> Some s
    | `No_documentation -> None
  ;;

  type type_enclosing =
    { loc : Loc.t
    ; typ : string
    ; doc : string option
    ; syntax_doc : Query_protocol.syntax_doc_result option
    }

  let type_enclosing ?name doc pos verbosity ~with_syntax_doc =
    with_pipeline_exn ?name doc (fun pipeline ->
      let command = Query_protocol.Type_enclosing (None, pos, Some 0) in
      let pipeline =
        match verbosity with
        | 0 -> pipeline
        | verbosity ->
          let source = source doc in
          let config = Mpipeline.final_config pipeline in
          let config =
            { config with query = { config.query with verbosity = Lvl verbosity } }
          in
          Mpipeline.make config source
      in
      let res = Query_commands.dispatch pipeline command in
      match res with
      | [] | (_, `Index _, _) :: _ -> None
      | (loc, `String typ, _) :: _ ->
        let doc = doc_comment pipeline pos in
        let syntax_doc =
          match with_syntax_doc with
          | true -> syntax_doc pipeline pos
          | false -> None
        in
        Some { loc; typ; doc; syntax_doc })
  ;;

  let doc_comment ?name doc pos =
    with_pipeline_exn ?name doc (fun pipeline -> doc_comment pipeline pos)
  ;;
end

let kind = function
  | Merlin merlin -> `Merlin merlin
  | Dune _ | Other _ -> `Other
;;

let merlin_exn t =
  match kind t with
  | `Merlin m -> m
  | `Other -> invalid_arg ("Document.merlin_exn: " ^ DocumentUri.to_string @@ uri t)
;;

let with_merlin_configuration t (configuration : Merlin_config.configuration) =
  match t with
  | Merlin merlin -> Merlin { merlin with fixed_configuration = Some configuration }
  | Dune _ | Other _ -> t
;;

let close t =
  match t with
  | Dune _ | Other _ -> Fiber.return ()
  | Merlin t ->
    Fiber.fork_and_join_unit
      (fun () -> Merlin_config.destroy t.merlin_config)
      (fun () -> Lev_fiber.Timer.Wheel.cancel t.timer)
;;

let get_impl_intf_counterparts m uri =
  let uri = Source_path.uri uri in
  let fpath = Uri.to_path uri in
  let fname = Filename.basename fpath in
  let ml, mli, eliom, eliomi, re, rei, mll, mly, mlx =
    "ml", "mli", "eliom", "eliomi", "re", "rei", "mll", "mly", "mlx"
  in
  let exts_to_switch_to =
    let kind =
      match m with
      | Some m -> Merlin.kind m
      | None ->
        (* still try to guess the kind *)
        (match Kind.of_fname_opt fpath with
         | Some k -> k
         | None -> Kind.unsupported uri)
    in
    match Syntax.of_fname fname with
    | Dune | Cram -> []
    | Mlx ->
      (match kind with
       | Intf -> [ ml; mly; mll; mlx; re ]
       | Impl -> [ rei; mli; mly; mll; rei ])
    | Ocaml ->
      (match kind with
       | Intf -> [ ml; mly; mll; eliom; re; mlx ]
       | Impl -> [ mli; mly; mll; eliomi; rei ])
    | Reason ->
      (match kind with
       | Intf -> [ re; ml; mlx ]
       | Impl -> [ rei; mli ])
    | Ocamllex -> [ mli; rei ]
    | Menhir -> [ mli; rei ]
  in
  let fpath_w_ext ext = Filename.remove_extension fpath ^ "." ^ ext in
  let find_switch exts =
    List.filter_map exts ~f:(fun ext ->
      let file_to_switch_to = fpath_w_ext ext in
      Option.some_if (Sys.file_exists file_to_switch_to) file_to_switch_to)
  in
  let files_to_switch_to =
    match find_switch exts_to_switch_to with
    | [] ->
      let switch_to_ext = List.hd_exn exts_to_switch_to in
      let switch_to_fpath = fpath_w_ext switch_to_ext in
      [ switch_to_fpath ]
    | to_switch_to -> to_switch_to
  in
  List.map ~f:Source_path.of_path files_to_switch_to
;;

let get_impl_intf_counterparts_for_configurations merlin configurations uri =
  let uri = Source_path.uri uri in
  let configured = Merlin_config.configuration_list configurations in
  if
    List.for_all configured ~f:(fun configuration ->
      match configuration.Merlin_config.origin with
      | Legacy_file -> true
      | Plural _ -> false)
  then get_impl_intf_counterparts (Some merlin) uri
  else (
    let exact =
      List.filter_map configured ~f:(fun configuration ->
        match configuration.Merlin_config.origin with
        | Legacy_file -> None
        | Plural { counterpart = None; _ } -> None
        | Plural { counterpart = Some uri; _ } ->
          let uri = Source_path.uri uri in
          Option.some_if (Sys.file_exists (Uri.to_path uri)) uri)
      |> List.fold_left ~init:[] ~f:(fun uris uri ->
        if List.mem uris uri ~equal:Uri.equal then uris else uri :: uris)
      |> List.rev
    in
    match exact with
    | _ :: _ -> exact
    | [] ->
      let proposed =
        List.map configured ~f:(fun configuration ->
          match configuration.Merlin_config.origin with
          | Legacy_file | Plural { counterpart = Some _; _ } -> None
          | Plural { counterpart = None; kind; _ } ->
            let path = Uri.to_path uri in
            let extension = Filename.extension path in
            let target_extension =
              List.find_map configuration.config.merlin.suffixes ~f:(fun (impl, intf) ->
                match kind with
                | Implementation when String.equal extension impl -> Some intf
                | Interface when String.equal extension intf -> Some impl
                | Implementation | Interface -> None)
            in
            Option.bind target_extension ~f:(fun target_extension ->
              let target = Filename.remove_extension path ^ target_extension in
              Option.some_if (not (Sys.file_exists target)) (Source_path.of_path target)))
      in
      (match proposed with
       | Some first :: rest
         when List.for_all rest ~f:(fun candidate ->
                Option.exists candidate ~f:(Uri.equal first)) -> [ first ]
       | [] | _ -> []))
;;
