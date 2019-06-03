open Angstrom

type t = {
  major: int;
  minor: int;
  patch: int;
  prerelease: string list;
  build: string list;
}

let mk_version major minor patch prerelease build =
  { major; minor; patch; prerelease; build }

let to_string v =
  let print_series mark identifiers =
    match identifiers with
    | [] -> ""
    | _ -> mark ^ String.concat "." identifiers
  in
  string_of_int v.major ^ "." ^ string_of_int v.minor ^ "." ^ string_of_int v.patch ^
  print_series "-" v.prerelease ^ print_series "+" v.build

let is_digit = function '0' .. '9' -> true | _ -> false

let no_leading_zero str =
  String.length str <= 1 || String.get str 0 != '0'

let nat: int Angstrom.t =
  take_while1 is_digit >>= fun str ->
  if no_leading_zero str then
    try return (int_of_string str) with _ -> fail ("invalid version number: " ^ str)
  else
    fail ("leading 0 in version number: " ^ str)

let isOk = function
  | Ok _ -> true
  | Error _ -> false

let%test "natural number is ok" =
  parse_string (nat <* end_of_input) "123" |> isOk

let%test "number with leading zero fails" =
  parse_string (nat <* end_of_input) "0123" |> isOk |> not

let%test "0 is ok" =
  parse_string (nat <* end_of_input) "0" |> isOk

let%test "alphanum fails" =
  parse_string (nat <* end_of_input) "0hey" |> isOk |> not

let dot =
  char '.'

let base_identifier =
  take_while1 (function '-' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

let prerelease_identifier =
  let verify_numeric str =
    try
      let _ = int_of_string str in
      if no_leading_zero str then return str else fail ("leading 0 in prerelease numeric identifier: " ^ str)
    with
    _ -> return str
  in
  base_identifier >>= verify_numeric

let identifier_list id_parser sep =
  char sep *> sep_by1 dot id_parser <|> return []

let version_parser =
  (lift4 mk_version
     nat
     (dot *> nat)
     (dot *> nat)
     (identifier_list prerelease_identifier '-')
   <*> (identifier_list base_identifier '+'))
  <* end_of_input

let from_parts major minor patch prerelease build =
  let check_prerelease_item s =
    parse_string (prerelease_identifier <* end_of_input) s |> isOk
  in
  let check_build_item s =
    parse_string (base_identifier <* end_of_input) s |> isOk
  in
  if major >= 0 && minor >= 0 && patch >= 0 &&
     List.for_all check_prerelease_item prerelease &&
     List.for_all check_build_item build then
    Some (mk_version major minor patch prerelease build)
  else
    None

let of_string str =
  match parse_string version_parser str with
  | Ok v -> Some v
  | Error _ -> None

let is_valid str =
  match parse_string version_parser str with
  | Ok _ -> true
  | Error _ -> false

(** Prerelease precedence rules:

    * Identifiers consisting of only digits are compared numerically and
      identifiers with letters or hyphens are compared lexically in ASCII sort
      order.

    * Numeric identifiers always have lower precedence than non-numeric
      identifiers.

    * A larger set of pre-release fields has a higher precedence than a smaller
      set, if all of the preceding identifiers are equal.

   <https://semver.org/#spec-item-11>
*)
let compare_identifiers ia ib =
  match parse_string (nat <* end_of_input) ia, parse_string (nat <* end_of_input) ib with
  | Ok na, Ok nb -> compare na nb
  | Ok _, _ -> -1
  | _, Ok _ -> 1
  | _ -> compare ia ib

let%test "numeric lower than non-numeric" =
  compare_identifiers "1" "beta" == -1

let%test "numeric comparison" =
  compare_identifiers "2" "11" == -1

let%test "alphabetical comparison" =
  compare_identifiers "alpha" "beta" == -1

let rec compare_prerelease is_first pra prb =
  match pra, prb with
  | [], [] -> 0
  | [], _ -> if is_first then 1 else -1
  | _, [] -> if is_first then -1 else 1
  | ia :: ta, ib :: tb ->
    if compare ia ib != 0 then
      compare_identifiers ia ib
    else
      compare_prerelease false ta tb

let%test "larger set has higher precedence" =
  compare_prerelease true ["alpha"] ["alpha"; "1"] == -1

let%test "compare first non-equal identifiers" =
  compare_prerelease true ["alpha"; "1"] ["alpha"; "beta"] == -1

let compare a b =
  if a.major != b.major then
    compare a.major b.major
  else if a.minor != b.minor then
    compare a.minor b.minor
  else if a.patch != b.patch then
    compare a.patch b.patch
  else compare_prerelease true a.prerelease b.prerelease

let less_than a b =
  compare a b < 0

let greater_than a b =
  compare a b > 0
