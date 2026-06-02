open Test.Import

module Configurations = Ocaml_lsp_server.Custom_request.Merlin_configurations
module Select = Ocaml_lsp_server.Custom_request.Select_merlin_configuration
module Merlin_call = Ocaml_lsp_server.Custom_request.Merlin_call_compatible

let uri = DocumentUri.of_path "test.ml"
let text_document = TextDocumentIdentifier.create ~uri

let fake_ocaml_merlin_exe () =
  let cwd = Sys.getcwd () in
  [ "fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ; "_build/default/ocaml-lsp-server/test/e2e-new/fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ; "ocaml-lsp/ocaml-lsp-server/test/e2e-new/fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ; "_build/default/ocaml-lsp/ocaml-lsp-server/test/e2e-new/fake_ocaml_merlin/fake_ocaml_merlin.exe"
  ]
  |> List.map ~f:(Filename.concat cwd)
  |> List.find ~f:Sys.file_exists
  |> Option.value_exn
;;

let extra_env protocol =
  [ "OCAMLLSP_PROJECT_BUILD_SYSTEM=" ^ fake_ocaml_merlin_exe ()
  ; "OCAMLLSP_PROJECT_ROOT=" ^ Sys.getcwd ()
  ; "FAKE_OCAML_MERLIN_PROTOCOL=" ^ protocol
  ]
;;

let request client ~meth ~params =
  let params = params |> Jsonrpc.Structured.t_of_yojson |> Option.some in
  Client.request client (Lsp.Client_request.UnknownRequest { meth; params })
;;

let print_response_or_error request =
  let open Fiber.O in
  let+ result =
    Fiber.collect_errors (fun () ->
      let+ response = request in
      `Response response)
  in
  match result with
  | Ok (`Response response) -> Test.print_result response
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; _ } ] ->
    Test.print_result (Jsonrpc.Response.Error.yojson_of_t error)
  | Error errors ->
    List.iter errors ~f:(fun error ->
      Exn_with_backtrace.to_dyn error |> Dyn.to_string |> print_endline)
;;

let merlin_configurations client =
  let params =
    Configurations.Request_params.create ~text_document
    |> Configurations.Request_params.yojson_of_t
  in
  request client ~meth:Configurations.meth ~params
;;

let select_merlin_configuration client id =
  let params =
    Select.Request_params.create ~text_document ~id
    |> Select.Request_params.yojson_of_t
  in
  request client ~meth:Select.meth ~params
;;

let merlin_errors client =
  let params =
    Merlin_call.Request_params.create
      ~text_document
      ~result_as_sexp:false
      ~command:"errors"
      ~args:[]
    |> Merlin_call.Request_params.yojson_of_t
  in
  request client ~meth:Merlin_call.meth ~params
;;

let source = {|let () = match Some 3 with | None -> ()|}

let%expect_test "new protocol: list and select configurations" =
  let request client =
    let open Fiber.O in
    let* response = merlin_configurations client in
    Test.print_result response;
    let* response = select_merlin_configuration client "melange" in
    Test.print_result response;
    let+ response = merlin_configurations client in
    Test.print_result response
  in
  Helpers.test ~extra_env:(extra_env "new") source request;
  [%expect
    {|
    [
      { "id": "ocaml", "mode": "ocaml", "isDefault": true, "isActive": true },
      {
        "id": "melange",
        "mode": "melange",
        "isDefault": false,
        "isActive": false
      }
    ]
    null
    [
      { "id": "ocaml", "mode": "ocaml", "isDefault": true, "isActive": false },
      {
        "id": "melange",
        "mode": "melange",
        "isDefault": false,
        "isActive": true
      }
    ]
    |}]
;;

let%expect_test "new protocol: selecting unknown configuration fails" =
  let request client = print_response_or_error (select_merlin_configuration client "js") in
  Helpers.test ~extra_env:(extra_env "new") source request;
  [%expect
    {|
    {
      "data": { "id": "js" },
      "code": -32602,
      "message": "Unknown Merlin configuration \"js\""
    }
    |}]
;;

let%expect_test "new protocol: selected configuration affects existing requests" =
  let request client =
    let open Fiber.O in
    let* response = merlin_errors client in
    Test.print_result response;
    let* (_ : Yojson.Safe.t) = select_merlin_configuration client "melange" in
    let+ response = merlin_errors client in
    Test.print_result response
  in
  Helpers.test ~extra_env:(extra_env "new") source request;
  [%expect
    {|
    {
      "resultAsSexp": false,
      "result": "{\"class\":\"return\",\"value\":[{\"start\":{\"line\":1,\"col\":9},\"end\":{\"line\":1,\"col\":39},\"type\":\"warning\",\"sub\":[],\"valid\":true,\"message\":\"Warning 8: this pattern-matching is not exhaustive.\\n  Here is an example of a case that is not matched: Some _\"}]}"
    }
    { "resultAsSexp": false, "result": "{\"class\":\"return\",\"value\":[]}" }
    |}]
;;

let%expect_test "old protocol: existing requests fall back to File" =
  let request client =
    let open Fiber.O in
    let+ response = merlin_errors client in
    Test.print_result response
  in
  Helpers.test ~extra_env:(extra_env "old") source request;
  [%expect
    {|
    {
      "resultAsSexp": false,
      "result": "{\"class\":\"return\",\"value\":[{\"start\":{\"line\":1,\"col\":9},\"end\":{\"line\":1,\"col\":39},\"type\":\"warning\",\"sub\":[],\"valid\":true,\"message\":\"Warning 8: this pattern-matching is not exhaustive.\\n  Here is an example of a case that is not matched: Some _\"}]}"
    }
    |}]
;;

let%expect_test "old protocol: new request exposes fallback configuration" =
  let request client =
    let open Fiber.O in
    let+ response = merlin_configurations client in
    Test.print_result response
  in
  Helpers.test ~extra_env:(extra_env "old") source request;
  [%expect
    {|
    [ { "id": "default", "mode": null, "isDefault": true, "isActive": true } ]
    |}]
;;
