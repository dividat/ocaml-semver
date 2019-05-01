open OUnit
open Semver

let (valids, invalids) =
  let json = Yojson.Basic.from_file "fixtures.json" in
  let open Yojson.Basic.Util in
  let read_list key = json |> member key |> to_list |> List.map to_string in
  (read_list "valid", read_list "invalid")

let test_valids () =
  List.iter (fun v -> assert_equal (to_string (of_string v)) v) valids

let test_invalids () =
  List.iter (fun v -> assert_bool "is invalid" (not (is_valid v))) invalids

let _ = test_valids (); test_invalids
