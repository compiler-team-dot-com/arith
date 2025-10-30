open Lib

type expectation = Expect_ok | Expect_error of string

let read_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
  really_input_string ic (in_channel_length ic)

let split_metadata contents =
  let lines = String.split_on_char '\n' contents in
  let rec aux directives = function
    | line :: rest when String.length line >= 3 && String.sub line 0 3 = "-- "
      ->
        let directive =
          String.trim (String.sub line 3 (String.length line - 3))
        in
        aux (directive :: directives) rest
    | remaining -> (List.rev directives, String.concat "\n" remaining)
  in
  aux [] lines

let parse_expect directive =
  let trimmed = String.trim directive in
  let prefix_error = "expect: error" in
  if String.equal trimmed "expect: ok" then Ok Expect_ok
  else if
    String.length trimmed >= String.length prefix_error
    && String.sub trimmed 0 (String.length prefix_error) = prefix_error
  then
    let msg =
      String.trim
        (String.sub trimmed
           (String.length prefix_error)
           (String.length trimmed - String.length prefix_error))
    in
    Ok (Expect_error msg)
  else Error directive

let run_fixture path =
  let contents = read_file path in
  let directives, program = split_metadata contents in
  let expectation =
    match directives with [] -> Ok Expect_ok | dir :: _ -> parse_expect dir
  in
  match expectation with
  | Error bad -> Alcotest.failf "%s: unknown metadata %s" path bad
  | Ok Expect_ok -> (
      match Parser.parse_expression program with
      | Error msg -> Alcotest.failf "%s: unexpected parse error %s" path msg
      | Ok expr -> (
          match Typed.evaluate expr with
          | Ok _ -> ()
          | Error msg -> Alcotest.failf "%s: unexpected type error %s" path msg)
      )
  | Ok (Expect_error expected) -> (
      match Parser.parse_expression program with
      | Error msg ->
          Alcotest.(check string) (path ^ " parse error") expected msg
      | Ok expr -> (
          match Typed.evaluate expr with
          | Ok _ -> Alcotest.failf "%s: expected error %s" path expected
          | Error msg ->
              Alcotest.(check string) (path ^ " type error") expected msg))

let () =
  let fixtures =
    Sys.readdir "programs" |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".lambda")
    |> List.map (Filename.concat "programs")
  in
  let tests =
    List.map
      (fun path -> Alcotest.test_case path `Quick (fun () -> run_fixture path))
      fixtures
  in
  Alcotest.run "parser-driver" [ ("fixtures", tests) ]
