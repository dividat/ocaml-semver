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

let nat =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>= fun str -> try return (int_of_string str) with _ -> fail ("invalid number: " ^ str)

let dot =
  char '.'

let identifier =
  take_while1 (function '-' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

let identifier_list sep =
  (char sep) *> (sep_by (char '.') identifier) <|> (return [])

let version_parser = (mk_version <$> nat <*> (dot *> nat) <*> (dot *> nat) <*> identifier_list '-' <*> identifier_list '+') <* end_of_input

let of_string (str: string) : t =
  match parse_string version_parser str with
  | Ok v -> v
  | Error msg -> failwith msg

let is_valid (str: string) : bool =
  match parse_string version_parser str with
  | Ok _ -> true
  | Error _ -> false
