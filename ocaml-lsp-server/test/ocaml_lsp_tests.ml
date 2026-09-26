open Lsp.Types
module Position = Lsp.Position
module Range = Lsp.Range

let%expect_test "configured query failure policies" =
  let open Ocaml_lsp_server.Testing in
  let configured mode ~is_default result =
    let configuration =
      { Merlin_config.origin = Plural { mode; kind = Implementation; counterpart = None }
      ; is_default
      ; config = Merlin_kernel.Mconfig.initial
      }
    in
    { Merlin.configuration; result }
  in
  let ocaml = configured Ocaml ~is_default:true in
  let melange = configured Melange ~is_default:false in
  let error code message =
    Stdune.Exn_with_backtrace.try_with (fun () ->
      Jsonrpc.Response.Error.raise (Jsonrpc.Response.Error.make ~code ~message ()))
  in
  let failed message = error RequestFailed message in
  let print_values values =
    List.iter
      (fun (configuration, value) ->
         Printf.printf "%s: %s\n" (Merlin_config.configuration_label configuration) value)
      values
  in
  let print_error error =
    Printf.printf
      "%s: %s\n"
      (Jsonrpc.Response.Error.Code.to_string error.Jsonrpc.Response.Error.code)
      error.message
  in
  let run label policy first rest =
    print_endline label;
    let results = Merlin_dot_protocol.Nonempty_list.create first rest in
    match policy results with
    | values -> print_values values
    | exception Jsonrpc.Response.Error.E error -> print_error error
  in
  let partial = Merlin.successful_results ~name:"hover" in
  let all results =
    match Merlin.all_results ~name:"rename" results with
    | Ok values -> values
    | Error error -> Jsonrpc.Response.Error.raise error
  in
  run "partial success" partial (melange (failed "melange error")) [ ocaml (Ok "int") ];
  run
    "all failed, default last"
    partial
    (melange (failed "melange error"))
    [ ocaml (failed "ocaml error") ];
  run
    "all failed, no default"
    partial
    (melange (failed "first error"))
    [ configured (Other "future") ~is_default:false (failed "second error") ];
  run "all succeed" all (ocaml (Ok "first")) [ melange (Ok "second") ];
  run "one fails" all (ocaml (Ok "first")) [ melange (failed "melange error") ];
  run "both fail" all (ocaml (failed "ocaml error")) [ melange (failed "melange error") ];
  let cancelled = melange (error RequestCancelled "cancelled") in
  run "partial cancellation" partial (ocaml (Ok "int")) [ cancelled ];
  run "require-all cancellation" all (ocaml (Ok "int")) [ cancelled ];
  [%expect
    {|
    partial success
    OCaml: int
    all failed, default last
    RequestFailed: ocaml error
    all failed, no default
    RequestFailed: first error
    all succeed
    OCaml: first
    Melange: second
    one fails
    RequestFailed: rename failed for modes: Melange
    both fail
    RequestFailed: rename failed for modes: OCaml, Melange
    partial cancellation
    RequestCancelled: cancelled
    require-all cancellation
    RequestCancelled: cancelled
  |}]
;;

let%expect_test "convert an LSP position to a Merlin logical position" =
  let position = Position.create ~line:2 ~character:3 in
  let (`Logical (line, column)) = Ocaml_lsp_server.Testing.Position.logical position in
  Printf.printf
    "LSP position (%d, %d) -> Merlin logical position (%d, %d)\n"
    position.line
    position.character
    line
    column;
  [%expect {| LSP position (2, 3) -> Merlin logical position (3, 3) |}]
;;

let%expect_test "document-symbol selection range relationships" =
  let range start_line start_character end_line end_character =
    let start = Position.create ~line:start_line ~character:start_character in
    let end_ = Position.create ~line:end_line ~character:end_character in
    Range.create ~start ~end_
  in
  let full_range = range 1 2 3 8 in
  let relation = function
    | None -> "ghost"
    | Some (selection_range : Range.t) ->
      if Position.compare selection_range.start selection_range.end_ > 0
      then "invalid"
      else if Range.contains full_range selection_range
      then "contained"
      else (
        let start =
          if Position.compare full_range.start selection_range.start >= 0
          then full_range.start
          else selection_range.start
        in
        let end_ =
          if Position.compare full_range.end_ selection_range.end_ <= 0
          then full_range.end_
          else selection_range.end_
        in
        match Position.compare start end_ with
        | n when n < 0 -> "overlap"
        | 0 -> "touch"
        | _ -> "disjoint")
  in
  let print label selection_range =
    Printf.printf "%s: %s\n" label (relation selection_range)
  in
  print "contained" (Some (range 1 4 2 6));
  print "contained empty" (Some (range 2 4 2 4));
  print "overlap before" (Some (range 0 9 1 5));
  print "overlap after" (Some (range 3 4 4 1));
  print "enclosing" (Some (range 0 0 4 0));
  print "touch before" (Some (range 0 0 1 2));
  print "touch after" (Some (range 3 8 4 0));
  print "disjoint before" (Some (range 0 0 1 1));
  print "disjoint after" (Some (range 3 9 4 0));
  print "ghost" None;
  print "reversed" (Some (range 2 6 2 4));
  [%expect
    {|
    contained: contained
    contained empty: contained
    overlap before: overlap
    overlap after: overlap
    enclosing: overlap
    touch before: touch
    touch after: touch
    disjoint before: disjoint
    disjoint after: disjoint
    ghost: ghost
    reversed: invalid
    |}]
;;

let%expect_test "diagnostic message equality ignores insignificant whitespace" =
  let test left right =
    let relation =
      if Ocaml_lsp_server.Diagnostics.equal_message left right
      then "equal"
      else "different"
    in
    Printf.printf "%S <> %S: %s\n" left right relation
  in
  test "foo bar" "foo  bar";
  test " foobar" "foobar";
  test "foobar" "foobar ";
  test "foobar" "foobar\t";
  test "foobar" "foobar\n";
  test "foobar" "foo bar";
  test "foo bar" "foo Bar";
  [%expect
    {|
    "foo bar" <> "foo  bar": equal
    " foobar" <> "foobar": equal
    "foobar" <> "foobar ": equal
    "foobar" <> "foobar\t": equal
    "foobar" <> "foobar\n": equal
    "foobar" <> "foo bar": different
    "foo bar" <> "foo Bar": different
    |}]
;;
