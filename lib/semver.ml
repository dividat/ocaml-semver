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

let to_string (v: t) =
  let print_series mark identifiers =
    match identifiers with
    | [] -> ""
    | _ -> mark ^ String.concat "." identifiers
  in
  string_of_int v.major ^ "." ^ string_of_int v.minor ^ "." ^ string_of_int v.patch ^
  print_series "-" v.prerelease ^ print_series "+" v.build

let is_num0 = function '0' .. '9' -> true | _ -> false
let is_num1 = function '1' .. '9' -> true | _ -> false

let nat =
  lift2 (^) (satisfy is_num1 >>| Char.escaped) (take_while is_num0) <|> (satisfy is_num0 >>| Char.escaped)
  >>= fun str -> try return (int_of_string str) with _ -> fail ("invalid number: " ^ str)

let dot =
  char '.'

let identifier =
  take_while1 (function '-' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

let identifier_list sep =
  (char sep) *> (sep_by (char '.') identifier) <|> (return [])

let version_parser = (lift4 mk_version nat (dot *> nat) (dot *> nat) (identifier_list '-') <*> (identifier_list '+')) <* end_of_input

let of_string (str: string) : t =
  match parse_string version_parser str with
  | Ok v -> v
  | Error msg -> failwith msg

let is_valid str =
  match parse_string version_parser str with
  | Ok _ -> true
  | Error _ -> false

let compare_identifiers i1 i2 =
  match (parse_string (nat <* end_of_input) i1, parse_string (nat <* end_of_input) i2) with
  | (Ok n1, Ok n2) -> compare n1 n2
  | _ -> compare i1 i2

let rec compare_prerelease is_first p1 p2 =
  match (p1, p2) with
  | ([], []) -> 0
  | ([], _) -> if is_first then 1 else -1
  | (_, []) -> if is_first then -1 else -1
  | (h1 :: t1, h2 :: t2) -> if h1 == h2 then compare_prerelease false t1 t2 else compare h1 h2

let compare v1 v2 =
  if v1.major != v2.major then
    compare v1.major v2.major
  else if v1.minor != v2.minor then
    compare v1.minor v2.minor
  else if v1.patch != v2.patch then
    compare v1.patch v2.patch
  else compare_prerelease true v1.prerelease v2.prerelease

let less_than v1 v2 =
  compare v1 v2 < 0

let greater_than v1 v2 =
  compare v1 v2 > 0
