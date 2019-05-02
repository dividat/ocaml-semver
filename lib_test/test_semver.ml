open OUnit
open Semver

let (valids, invalids, sorted) =
  let json = Yojson.Basic.from_file "fixtures.json" in
  let open Yojson.Basic.Util in
  let read_list key = json |> member key |> to_list |> List.map to_string in
  (read_list "valid", read_list "invalid", read_list "compare")

let test_valids () =
  List.iter (fun v -> assert_equal (to_string (of_string v)) v) valids

let test_invalids () =
  List.iter (fun v -> assert_bool (v ^ " is invalid") (not (is_valid v))) invalids

let test_compare () =
  assert_equal sorted (sorted |> List.map of_string |> List.sort compare |> List.map to_string)

let suite =
  "semver suite" >:::
  ["test valids" >:: test_valids;
   "test invalids" >:: test_invalids;
   "test compare" >:: test_compare]

let _ = run_test_tt_main suite
