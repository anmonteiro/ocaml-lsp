open Test.Import
module Configurations = Ocaml_lsp_server.Custom_request.Merlin_configurations

let fake_ocaml_merlin_exe () =
  let cwd = Sys.getcwd () in
  [ "fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ; "../../../_build/default/ocaml-lsp-server/test/e2e-new/fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ; "_build/default/ocaml-lsp-server/test/e2e-new/fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ]
  |> List.map ~f:(Filename.concat cwd)
  |> List.find ~f:Sys.file_exists
  |> Option.value_exn
;;

let extra_env ?(root = Sys.getcwd ()) protocol =
  let executable = fake_ocaml_merlin_exe () in
  let path =
    Filename.dirname executable ^ ":" ^ Option.value_exn (Sys.getenv_opt "PATH")
  in
  [ "OCAMLLSP_PROJECT_BUILD_SYSTEM=" ^ Filename.basename executable
  ; "OCAMLLSP_PROJECT_ROOT=" ^ root
  ; "FAKE_OCAML_MERLIN_EXE=" ^ executable
  ; "FAKE_OCAML_MERLIN_PROTOCOL=" ^ protocol
  ; "PATH=" ^ path
  ]
;;

let request client uri =
  let text_document = TextDocumentIdentifier.create ~uri in
  let params =
    Configurations.Request_params.create ~text_document
    |> Configurations.Request_params.yojson_of_t
    |> Jsonrpc.Structured.t_of_yojson
    |> Option.some
  in
  Client.request
    client
    (Lsp.Client_request.UnknownRequest { meth = Configurations.meth; params })
;;

let print_error request =
  let* result = Fiber.collect_errors (fun () -> request) in
  match result with
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; _ } ] ->
    Printf.printf "%s\n" error.message;
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
  | Ok response ->
    Test.print_result response;
    Fiber.return ()
;;

let source = "let value = 1"

let completion_items = function
  | None -> []
  | Some (`CompletionList { CompletionList.items; _ } | `List items) -> items
;;

let has_completion items label =
  List.exists items ~f:(fun (item : CompletionItem.t) -> String.equal item.label label)
;;

let code_action_capabilities () =
  let showDocument = ShowDocumentClientCapabilities.create ~support:true in
  let window = WindowClientCapabilities.create ~showDocument () in
  ClientCapabilities.create ~window ()
;;

let rename client ~position ~newName =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentRename (RenameParams.create ~textDocument ~position ~newName ()))
;;

let print_request_error request =
  let* result = Fiber.collect_errors (fun () -> request) in
  match result with
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; _ } ] ->
    Printf.printf
      "%s: %s\n"
      (Jsonrpc.Response.Error.Code.to_string error.code)
      error.message;
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
  | Ok _ ->
    print_endline "unexpected success";
    Fiber.return ()
;;

let rec wait_for_preprocess_mode path mode =
  let found =
    Sys.file_exists path
    && In_channel.with_open_text path In_channel.input_all
       |> String.split_lines
       |> fun modes -> List.mem modes mode ~equal:String.equal
  in
  if found
  then Fiber.return ()
  else
    let* () = Lev_fiber.Timer.sleepf 0.01 in
    wait_for_preprocess_mode path mode
;;

let run_diagnostics protocol source =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params ->
           let* current = Fiber.Ivar.peek diagnostics in
           (match current with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill diagnostics params)
         | _ -> Fiber.return ())
      ()
  in
  Test.run_initialized ~handler ~extra_env:(extra_env protocol) (fun client ->
    let textDocument =
      TextDocumentItem.create
        ~uri:Helpers.uri
        ~languageId:(LanguageKind.Other "ocaml")
        ~version:0
        ~text:source
    in
    let* () =
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let* diagnostics = Fiber.Ivar.read diagnostics in
    PublishDiagnosticsParams.yojson_of_t diagnostics |> Test.print_result;
    Test.shutdown_client client)
;;

let%expect_test "plural and exclusive configuration sets" =
  let run protocol =
    Helpers.test ~extra_env:(extra_env protocol) source (fun client ->
      let+ response = request client Helpers.uri in
      Test.print_result response)
  in
  run "plural";
  run "exclusive";
  [%expect
    {|
    [
      { "mode": "ocaml", "isDefault": true },
      { "mode": "melange", "isDefault": false }
    ]
    [ { "mode": "melange", "isDefault": false } ]
    |}]
;;

let%expect_test "old Dune falls back only on the exact unsupported response" =
  Helpers.test ~extra_env:(extra_env "legacy") source (fun client ->
    let+ response = request client Helpers.uri in
    Test.print_result response);
  [%expect {| [ { "mode": null, "isDefault": true } ] |}]
;;

let%expect_test "invalid plural responses do not synthesize a default" =
  let run protocol =
    Helpers.test ~extra_env:(extra_env protocol) source (fun client ->
      print_error (request client Helpers.uri))
  in
  run "malformed";
  run "empty";
  run "error";
  [%expect
    {|
    A tagged configuration response was expected, instead got: "malformed"
    Empty configuration response
    configuration failed
    |}]
;;

let%expect_test "concurrent documents receive their own response" =
  let uri_a = DocumentUri.of_path "a.ml" in
  let uri_b = DocumentUri.of_path "b.ml" in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run_initialized ~handler ~extra_env:(extra_env "by-path") (fun client ->
    let settings = `Assoc [ "diagnostics_delay", `Float 10.0 ] in
    let* () = Client.notification client (ChangeConfiguration { settings }) in
    let open_document uri =
      let textDocument =
        TextDocumentItem.create
          ~uri
          ~languageId:(LanguageKind.Other "ocaml")
          ~version:0
          ~text:source
      in
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let* () = open_document uri_a in
    let* () = open_document uri_b in
    let* response_a, response_b =
      Fiber.fork_and_join
        (fun () -> request client uri_a)
        (fun () -> request client uri_b)
    in
    Test.print_result response_a;
    Test.print_result response_b;
    Test.exit_client client);
  [%expect
    {|
    [ { "mode": "a", "isDefault": true } ]
    [ { "mode": "b", "isDefault": true } ]
    |}]
;;

let%expect_test "exclusive files execute only their applicable mode" =
  let dir = Test.temp_dir "exclusive-mode" in
  let log = Filename.concat dir "preprocess.log" in
  Test.write_file log "";
  let extra_env =
    ("FAKE_OCAML_MERLIN_PP_LOG=" ^ log) :: extra_env "exclusive-preprocessed"
  in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run_initialized ~handler ~extra_env (fun client ->
    let settings = `Assoc [ "diagnostics_delay", `Float 10.0 ] in
    let* () = Client.notification client (ChangeConfiguration { settings }) in
    let source = "let value = MODE_EXPR\nlet _ = value" in
    let* () = Test.open_document ~client ~uri:Helpers.uri ~source () in
    let* (_ : Hover.t option) =
      Hover_helpers.hover client (Position.create ~line:1 ~character:10)
    in
    let modes =
      In_channel.with_open_text log In_channel.input_all
      |> String.split_lines
      |> List.dedup_and_sort ~compare:String.compare
    in
    Test.print_result (`List (List.map modes ~f:(fun mode -> `String mode)));
    Test.exit_client client);
  [%expect {| [ "melange" ] |}]
;;

let%expect_test "diagnostics retain mode contributors" =
  run_diagnostics
    "plural"
    {ocaml|let head = function
  | value :: _ -> value
|ocaml};
  run_diagnostics "plural" "let =";
  [%expect
    {|
    {
      "diagnostics": [
        {
          "data": { "ocamllsp": { "version": 1, "modes": [ "ocaml" ] } },
          "message": "Warning 8: this pattern-matching is not exhaustive.\n  Here is an example of a case that is not matched: []",
          "range": {
            "end": { "character": 23, "line": 1 },
            "start": { "character": 11, "line": 0 }
          },
          "severity": 2,
          "source": "ocamllsp (OCaml)"
        }
      ],
      "uri": "file:///test.ml"
    }
    {
      "diagnostics": [
        {
          "data": {
            "ocamllsp": { "version": 1, "modes": [ "ocaml", "melange" ] }
          },
          "message": "Syntax error: let-extension (with punning) expected.",
          "range": {
            "end": { "character": 3, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///test.ml"
    }
    |}]
;;

let%expect_test "generated diagnostic locations are not clamped to the source" =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params -> Fiber.Ivar.fill diagnostics params
         | _ -> Fiber.return ())
      ()
  in
  (Test.run_initialized ~handler ~extra_env:(extra_env "extended-preprocessed")
   @@ fun client ->
   let* () = Test.open_document ~client ~uri:Helpers.uri ~source:"let value = 1" () in
   let* { PublishDiagnosticsParams.diagnostics = published; _ } =
     Fiber.Ivar.read diagnostics
   in
   let generated_location_retained =
     List.exists published ~f:(fun { Diagnostic.range = { Range.start; _ }; _ } ->
       start.line > 0)
   in
   Printf.printf "generated location retained: %b\n" generated_location_retained;
   Test.exit_client client);
  [%expect {| generated location retained: true |}]
;;

let%expect_test "completion intersects modes and honors comment suppression" =
  let source = "let shared = 1\nlet MODE_NAME = 2\nlet _ = \nOCAML_ON List. OCAML_OFF" in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* response =
      Client.request
        client
        (TextDocumentCompletion
           (CompletionParams.create
              ~textDocument
              ~position:(Position.create ~line:2 ~character:8)
              ()))
    in
    let items = completion_items response in
    Printf.printf "shared: %b\n" (has_completion items "shared");
    Printf.printf "ocaml-only: %b\n" (has_completion items "ocamlOnly");
    Printf.printf "melange-only: %b\n" (has_completion items "melanOnly");
    let context =
      CompletionContext.create
        ~triggerKind:CompletionTriggerKind.TriggerCharacter
        ~triggerCharacter:"."
        ()
    in
    let+ response =
      Client.request
        client
        (TextDocumentCompletion
           (CompletionParams.create
              ~textDocument
              ~position:(Position.create ~line:3 ~character:14)
              ~context
              ()))
    in
    Printf.printf "suppressed: %b\n" (Option.is_none response));
  [%expect
    {|
    shared: true
    ocaml-only: false
    melange-only: false
    suppressed: true
    |}]
;;

let%expect_test "completion uses the declared default as its primary configuration" =
  let source = "let value = MODE_EXPR\nlet _ = val" in
  Helpers.test ~extra_env:(extra_env "reversed-preprocessed") source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let+ response =
      Client.request
        client
        (TextDocumentCompletion
           (CompletionParams.create
              ~textDocument
              ~position:(Position.create ~line:1 ~character:11)
              ()))
    in
    let item =
      completion_items response
      |> List.find ~f:(fun (item : CompletionItem.t) -> String.equal item.label "value")
      |> Option.value_exn
    in
    let detail = Option.value item.detail ~default:"" in
    Printf.printf "default detail first: %b\n" (String.is_prefix detail ~prefix:"OCaml:"));
  [%expect {| default detail first: true |}]
;;

let%expect_test "hover preserves divergent mode results" =
  let source = "let value = MODE_EXPR\nlet _ = value" in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let+ response = Hover_helpers.hover client (Position.create ~line:1 ~character:10) in
    Hover_helpers.print_hover response);
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "OCaml:\nint\n\nMelange:\nstring"
      },
      "range": {
        "end": { "character": 13, "line": 1 },
        "start": { "character": 8, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover folds an arbitrary configuration list" =
  let source = "let value = MODE_EXPR\nlet _ = value" in
  Helpers.test ~extra_env:(extra_env "three-preprocessed") source (fun client ->
    let+ response = Hover_helpers.hover client (Position.create ~line:1 ~character:10) in
    Hover_helpers.print_hover response);
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "OCaml:\nint\n\nMelange:\nstring\n\nnative:\nbool"
      },
      "range": {
        "end": { "character": 13, "line": 1 },
        "start": { "character": 8, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "partial hover labels its successful mode" =
  let source = "OCAML_ON let value = 1 OCAML_OFF\nOCAML_ON let _ = value OCAML_OFF" in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let+ response = Hover_helpers.hover client (Position.create ~line:1 ~character:19) in
    Hover_helpers.print_hover response);
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "OCaml:\nint" },
      "range": {
        "end": { "character": 22, "line": 1 },
        "start": { "character": 17, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "signature help preserves mode-specific active parameters" =
  let source =
    "let apply = MODE_LAMBDA________________\nlet _ = MODE_CALL__________________2"
  in
  let position =
    Position.create ~line:1 ~character:(8 + String.length "MODE_CALL__________________")
  in
  let capabilities = Signature_help.make_capabilities ~activeParameterSupport:true () in
  Helpers.test ~capabilities ~extra_env:(extra_env "preprocessed") source (fun client ->
    let+ response = Signature_help.signature_help client position in
    let { SignatureHelp.signatures; _ } = response in
    List.iter signatures ~f:(fun { SignatureInformation.activeParameter; label; _ } ->
      let active_parameter = Option.join activeParameter |> Option.value_exn in
      Printf.printf "%s (active %d)\n" label active_parameter));
  [%expect
    {|
    apply : int -> int -> int (active 1)
    apply : int -> int -> int -> int (active 2)
    |}]
;;

let%expect_test "document symbols preserve mode-specific type details" =
  let capabilities =
    let documentSymbol =
      DocumentSymbolClientCapabilities.create ~hierarchicalDocumentSymbolSupport:true ()
    in
    let textDocument = TextDocumentClientCapabilities.create ~documentSymbol () in
    ClientCapabilities.create ~textDocument ()
  in
  Helpers.test
    ~capabilities
    ~extra_env:(extra_env "preprocessed")
    "let shared = 1\nlet value = MODE_SYMBOL_EXPR"
    (fun client ->
       let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
       let params = DocumentSymbolParams.create ~textDocument () in
       let+ response = Client.request client (Lsp.Client_request.DocumentSymbol params) in
       let symbols =
         match response with
         | Some (`DocumentSymbol symbols) -> symbols
         | Some (`SymbolInformation _) | None -> failwith "missing document symbols"
       in
       let find_symbol name =
         List.find_exn symbols ~f:(fun (symbol : DocumentSymbol.t) ->
           String.equal symbol.name name)
       in
       Printf.printf "symbols: %d\n" (List.length symbols);
       Printf.printf
         "shared detail: %s\n"
         (Option.value (find_symbol "shared").detail ~default:"<none>");
       Printf.printf
         "divergent detail:\n%s\n"
         (Option.value_exn (find_symbol "value").detail));
  [%expect
    {|
    symbols: 2
    shared detail: <none>
    divergent detail:
    OCaml: int
    Melange: string
    |}]
;;

let%expect_test "semantic tokens intersect mode-specific regions" =
  let source =
    "let shared = 1\nOCAML_ON let ocaml_only = shared OCAML_OFF\nlet use = shared"
  in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let+ response =
      Client.request
        client
        (SemanticTokensFull (SemanticTokensParams.create ~textDocument ()))
    in
    let data = Option.value_exn response |> fun tokens -> tokens.SemanticTokens.data in
    let lines, _ =
      Array.foldi data ~init:([], 0) ~f:(fun index (lines, line) value ->
        if Int.rem index 5 = 0
        then (
          let line = line + value in
          line :: lines, line)
        else lines, line)
    in
    let lines = List.dedup_and_sort lines ~compare:Int.compare in
    Test.print_result (`List (List.map lines ~f:(fun line -> `Int line))));
  [%expect {| [ 0, 2 ] |}]
;;

let%expect_test "definition unions mode-specific targets" =
  let source =
    "OCAML_ON let target = 1 OCAML_OFF\n\
     MELANGE_ON let target = 2 MELANGE_OFF\n\
     let _ = target"
  in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let+ response =
      Client.request
        client
        (TextDocumentDefinition
           (DefinitionParams.create
              ~textDocument
              ~position:(Position.create ~line:2 ~character:10)
              ()))
    in
    match response with
    | None -> print_endline "none"
    | Some (`Location locations) ->
      List.iter locations ~f:(fun { Location.uri; range = { Range.start; _ } } ->
        Printf.printf
          "%s:%d:%d\n"
          (DocumentUri.to_path uri |> Filename.basename)
          start.line
          start.character)
    | Some (`LocationLink links) ->
      List.iter
        links
        ~f:(fun { LocationLink.targetUri; targetRange = { Range.start; _ }; _ } ->
          Printf.printf
            "%s:%d:%d\n"
            (DocumentUri.to_path targetUri |> Filename.basename)
            start.line
            start.character));
  [%expect
    {|
    test.ml:0:13
    test.ml:1:15
    |}]
;;

let%expect_test "rename requires structural consensus" =
  Helpers.test
    ~extra_env:(extra_env "preprocessed")
    "let value = 1\nlet _ = value"
    (fun client ->
       let+ edit =
         rename
           client
           ~position:(Position.create ~line:1 ~character:10)
           ~newName:"renamed"
       in
       let count =
         Option.value edit.WorkspaceEdit.changes ~default:[]
         |> List.sum (module Int) ~f:(fun (_, edits) -> List.length edits)
       in
       Printf.printf "consensus edits: %d\n" count);
  Helpers.test
    ~extra_env:(extra_env "preprocessed")
    "let value = 1\nOCAML_ON let only = value OCAML_OFF\nlet _ = value"
    (fun client ->
       print_request_error
         (rename
            client
            ~position:(Position.create ~line:2 ~character:10)
            ~newName:"renamed"));
  [%expect
    {|
    consensus edits: 2
    RequestFailed: The applicable modes produced different rename targets
    |}]
;;

let%expect_test "code actions require the same edit in every mode" =
  let source = "let value = MODE_EXPR" in
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let context =
      CodeActionContext.create
        ~diagnostics:[]
        ~only:[ CodeActionKind.Other "type-annotate" ]
        ()
    in
    let range =
      Range.create
        ~start:(Position.create ~line:0 ~character:4)
        ~end_:(Position.create ~line:0 ~character:9)
    in
    let+ response =
      Client.request
        client
        (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
    in
    let count = Option.value response ~default:[] |> List.length in
    Printf.printf "portable actions: %d\n" count);
  [%expect {| portable actions: 0 |}]
;;

let%expect_test "code actions filter diagnostic provenance by wire mode key" =
  let source = "let f x =\n  let y = 1 in\n  0" in
  let range =
    Range.create
      ~start:(Position.create ~line:1 ~character:6)
      ~end_:(Position.create ~line:1 ~character:7)
  in
  let run modes =
    let data =
      `Assoc
        [ ( "ocamllsp"
          , `Assoc
              [ "version", `Int 1
              ; "modes", `List (List.map modes ~f:(fun mode -> `String mode))
              ] )
        ]
    in
    let diagnostic =
      Diagnostic.create
        ~range
        ~message:(`String "Error (warning 26): unused variable")
        ~severity:DiagnosticSeverity.Warning
        ~source:"ocamllsp"
        ~data
        ()
    in
    Helpers.test ~extra_env:(extra_env "plural") source (fun client ->
      let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
      let context =
        CodeActionContext.create
          ~diagnostics:[ diagnostic ]
          ~only:[ CodeActionKind.QuickFix ]
          ()
      in
      let+ response =
        Client.request
          client
          (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
      in
      let count =
        Option.value response ~default:[]
        |> List.count ~f:(function
          | `CodeAction { CodeAction.title; _ } -> String.equal title "Mark as unused"
          | `Command _ -> false)
      in
      Printf.printf "%s: %d\n" (String.concat modes ~sep:",") count)
  in
  run [ "ocaml"; "melange" ];
  run [ "ocaml" ];
  [%expect
    {|
    ocaml,melange: 1
    ocaml: 0
    |}]
;;

let%expect_test "a code-action request fetches its configuration set once" =
  let dir = Test.temp_dir "code-action-configuration-snapshot" in
  let log = Filename.concat dir "requests.log" in
  Test.write_file log "";
  let extra_env = ("FAKE_OCAML_MERLIN_LOG=" ^ log) :: extra_env "plural" in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run_initialized ~handler ~capabilities:(code_action_capabilities ()) ~extra_env
   @@ fun client ->
   let settings =
     `Assoc
       [ "diagnostics_delay", `Float 10.0
       ; "merlinJumpCodeActions", `Assoc [ "enable", `Bool true ]
       ]
   in
   let* () = Client.notification client (ChangeConfiguration { settings }) in
   let source = "let value = 1" in
   let* () = Test.open_document ~client ~uri:Helpers.uri ~source () in
   Test.write_file log "";
   let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
   let context =
     CodeActionContext.create
       ~diagnostics:[]
       ~only:
         [ CodeActionKind.Other "switch"
         ; CodeActionKind.Other "merlin-jump-let"
         ; CodeActionKind.Other "type-annotate"
         ]
       ()
   in
   let position = Position.create ~line:0 ~character:4 in
   let range = Range.create ~start:position ~end_:position in
   let* (_ : [ `CodeAction of CodeAction.t | `Command of Command.t ] list option) =
     Client.request
       client
       (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
   in
   let request_count =
     In_channel.with_open_text log In_channel.input_all
     |> String.split_lines
     |> List.count ~f:(String.is_substring ~substring:"File-Configurations")
   in
   Printf.printf "configuration requests: %d\n" request_count;
   Test.exit_client client);
  [%expect {| configuration requests: 1 |}]
;;

let request_fun_jump client ~uri ~position =
  let settings = `Assoc [ "merlinJumpCodeActions", `Assoc [ "enable", `Bool true ] ] in
  let* () = Client.notification client (ChangeConfiguration { settings }) in
  let textDocument = TextDocumentIdentifier.create ~uri in
  let context =
    CodeActionContext.create
      ~diagnostics:[]
      ~only:[ CodeActionKind.Other "merlin-jump-fun" ]
      ()
  in
  let range = Range.create ~start:position ~end_:position in
  Client.request
    client
    (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
;;

let print_fun_jumps = function
  | None -> print_endline "<none>"
  | Some actions ->
    List.iter actions ~f:(function
      | `Command _ -> ()
      | `CodeAction
          { CodeAction.title; command = Some { arguments = Some arguments; _ }; _ } ->
        (match arguments with
         | [ _; range ] ->
           let { Range.start; _ } = Range.t_of_yojson range in
           Printf.printf "%s: %d\n" title start.line
         | _ -> ())
      | `CodeAction _ -> ())
;;

let%expect_test "Merlin jump actions union and label mode-specific targets" =
  let run source position =
    Helpers.test
      ~capabilities:(code_action_capabilities ())
      ~extra_env:(extra_env "preprocessed")
      source
      (fun client ->
         let+ actions = request_fun_jump client ~uri:Helpers.uri ~position in
         print_fun_jumps actions)
  in
  run "let result = (fun () ->\n  1) ()" (Position.create ~line:1 ~character:2);
  run
    (String.concat
       ~sep:"\n"
       [ "OCAML_ON let result = (fun () -> OCAML_OFF"
       ; "MELANGE_ON let result = (fun _ -> MELANGE_OFF"
       ; "  1"
       ; "OCAML_ON ) () OCAML_OFF"
       ; "MELANGE_ON ) 0 MELANGE_OFF"
       ])
    (Position.create ~line:2 ~character:2);
  [%expect
    {|
    Fun jump: 0
    Fun jump (OCaml): 0
    Fun jump (Melange): 1
    |}]
;;

let%expect_test "Merlin jump commands canonicalize a symlinked document" =
  let dir = Test.temp_dir "jump-symlink" in
  let source = "let result = (fun () ->\n  1) ()" in
  let original = Filename.concat dir "original.ml" in
  let link = Filename.concat dir "link.ml" in
  Test.write_file original source;
  Unix.symlink original link;
  let uri = DocumentUri.of_path link in
  Helpers.test
    ~uri
    ~capabilities:(code_action_capabilities ())
    ~extra_env:(extra_env "preprocessed")
    source
    (fun client ->
       let+ actions =
         request_fun_jump client ~uri ~position:(Position.create ~line:1 ~character:2)
       in
       let command_uri =
         Option.value_exn actions
         |> List.find_map ~f:(function
           | `CodeAction
               { CodeAction.command = Some { arguments = Some (uri :: _); _ }; _ } ->
             Some (DocumentUri.t_of_yojson uri)
           | `Command _ | `CodeAction _ -> None)
         |> Option.value_exn
       in
       Printf.printf
         "canonical command URI: %b\n"
         (String.equal (DocumentUri.to_path command_uri) (Unix.realpath original)));
  [%expect {| canonical command URI: true |}]
;;

let%expect_test "edit-producing custom requests reject multiple modes" =
  Helpers.test ~extra_env:(extra_env "preprocessed") source (fun client ->
    print_request_error
      (Test.custom_request
         client
         "ocamllsp/inferIntf"
         (`List [ DocumentUri.yojson_of_t Helpers.uri ])));
  [%expect
    {| RequestFailed: This request is unavailable for files with multiple Merlin configurations |}]
;;

let%expect_test "cancellation discards a partial multi-mode hover" =
  let dir = Test.temp_dir "cancel-modes" in
  let log = Filename.concat dir "preprocess.log" in
  Test.write_file log "";
  let extra_env = ("FAKE_OCAML_MERLIN_PP_LOG=" ^ log) :: extra_env "slow-preprocessed" in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run_initialized ~handler ~extra_env (fun client ->
    let source = "let value = MODE_EXPR\nlet _ = value" in
    let* () = Test.open_document ~client ~uri:Helpers.uri ~source () in
    let settings = `Assoc [ "diagnostics_delay", `Float 10.0 ] in
    let* () = Client.notification client (ChangeConfiguration { settings }) in
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let params =
      HoverParams.create
        ~textDocument
        ~position:(Position.create ~line:1 ~character:10)
        ()
    in
    let cancel = Fiber.Cancel.create () in
    let* result, () =
      Fiber.fork_and_join
        (fun () -> Client.request_with_cancel client cancel (TextDocumentHover params))
        (fun () ->
           let* () = wait_for_preprocess_mode log "melange" in
           Fiber.Cancel.fire cancel)
    in
    (match result with
     | `Cancelled -> print_endline "cancelled"
     | `Ok _ -> print_endline "unexpected result");
    Test.exit_client client);
  [%expect {| cancelled |}]
;;

let%expect_test "missing counterpart creation requires mode consensus" =
  let run protocol =
    let dir = Test.temp_dir ("missing-counterpart-" ^ protocol) in
    let path = Filename.concat dir "main.ml" in
    Test.write_file path source;
    let uri = DocumentUri.of_path path in
    let on_notification, _ = Test.drain_diagnostics () in
    let handler = Client.Handler.make ~on_notification () in
    Test.run_initialized ~cwd:dir ~handler ~extra_env:(extra_env ~root:dir protocol)
    @@ fun client ->
    let* () = Test.open_document ~client ~uri ~source () in
    let* response =
      Test.custom_request
        client
        "ocamllsp/switchImplIntf"
        (`List [ DocumentUri.yojson_of_t uri ])
    in
    let basenames =
      Yojson.Safe.Util.to_list response
      |> List.map ~f:(fun path ->
        Yojson.Safe.Util.to_string path
        |> DocumentUri.of_string
        |> DocumentUri.to_path
        |> Filename.basename)
    in
    Printf.printf
      "%s: %s\n"
      protocol
      (match basenames with
       | [] -> "<none>"
       | basenames -> String.concat basenames ~sep:", ");
    Test.shutdown_client client
  in
  run "plural";
  run "divergent-suffixes";
  [%expect
    {|
    plural: main.mli
    divergent-suffixes: <none>
    |}]
;;

let%expect_test "exact counterparts are unioned by mode" =
  let dir = Test.temp_dir "counterparts" in
  let path name = Filename.concat dir name in
  List.iter [ "main.ml"; "main.ocaml.mli"; "main.melange.mli" ] ~f:(fun name ->
    Test.write_file (path name) "let value = 1\n");
  let uri = DocumentUri.of_path (path "main.ml") in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run_initialized ~cwd:dir ~handler ~extra_env:(extra_env ~root:dir "counterparts")
   @@ fun client ->
   let* () = Test.open_document ~client ~uri ~source () in
   let* response =
     Test.custom_request
       client
       "ocamllsp/switchImplIntf"
       (`List [ DocumentUri.yojson_of_t uri ])
   in
   let basenames =
     Yojson.Safe.Util.to_list response
     |> List.map ~f:(fun path ->
       Yojson.Safe.Util.to_string path
       |> DocumentUri.of_string
       |> DocumentUri.to_path
       |> Filename.basename)
   in
   List.iter basenames ~f:print_endline;
   Test.shutdown_client client);
  [%expect
    {|
    main.ocaml.mli
    main.melange.mli
    |}]
;;

let%expect_test "legacy counterpart lookup starts from the symlink target" =
  let dir = Test.temp_dir "counterpart-symlink" in
  let original = Filename.concat dir "original.ml" in
  let interface = Filename.concat dir "original.mli" in
  let link = Filename.concat dir "link.ml" in
  Test.write_file original source;
  Test.write_file interface "val value : int\n";
  Unix.symlink original link;
  let uri = DocumentUri.of_path link in
  let on_notification, _ = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run_initialized ~handler ~extra_env:(extra_env ~root:dir "legacy")
   @@ fun client ->
   let* () = Test.open_document ~client ~uri ~source () in
   let* response =
     Test.custom_request
       client
       "ocamllsp/switchImplIntf"
       (`List [ DocumentUri.yojson_of_t uri ])
   in
   let counterpart =
     Yojson.Safe.Util.to_list response
     |> List.map ~f:Yojson.Safe.Util.to_string
     |> List.hd_exn
     |> DocumentUri.of_string
     |> DocumentUri.to_path
   in
   Printf.printf
     "original interface: %b\n"
     (String.equal counterpart (Unix.realpath interface));
   Test.exit_client client);
  [%expect {| original interface: true |}]
;;

let%expect_test "cross-document edits pair exact counterparts by mode" =
  let run label protocol ~ocaml_impl ~melange_impl =
    let dir = Test.temp_dir ("counterpart-edits-" ^ label) in
    let path name = Filename.concat dir name in
    let interface = "val existing : int\n" in
    Test.write_file (path "main.mli") interface;
    Test.write_file (path "main.ocaml.ml") ocaml_impl;
    Test.write_file (path "main.melange.ml") melange_impl;
    let uri = DocumentUri.of_path (path "main.mli") in
    let on_notification, _ = Test.drain_diagnostics () in
    let handler = Client.Handler.make ~on_notification () in
    Test.run_initialized ~cwd:dir ~handler ~extra_env:(extra_env ~root:dir protocol)
    @@ fun client ->
    let settings = `Assoc [ "diagnostics_delay", `Float 10.0 ] in
    let* () = Client.notification client (ChangeConfiguration { settings }) in
    let* () = Test.open_document ~client ~uri ~source:interface () in
    let textDocument = TextDocumentIdentifier.create ~uri in
    let context =
      CodeActionContext.create
        ~diagnostics:[]
        ~only:[ CodeActionKind.Other "inferred_intf" ]
        ()
    in
    let position = Position.create ~line:0 ~character:0 in
    let range = Range.create ~start:position ~end_:position in
    let* response =
      Client.request
        client
        (CodeAction (CodeActionParams.create ~textDocument ~range ~context ()))
    in
    let actions = Option.value response ~default:[] in
    Printf.printf "%s: %d\n" label (List.length actions);
    Test.exit_client client
  in
  run
    "matching"
    "counterparts"
    ~ocaml_impl:"let value = 1\n"
    ~melange_impl:"let value = 1\n";
  run
    "divergent"
    "counterparts"
    ~ocaml_impl:"let value = 1\n"
    ~melange_impl:"let value = \"one\"\n";
  run
    "disjoint"
    "disjoint-counterparts"
    ~ocaml_impl:"let value = 1\n"
    ~melange_impl:"let value = 1\n";
  [%expect
    {|
    matching: 1
    divergent: 0
    disjoint: 0
    |}]
;;
