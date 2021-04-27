open Import

type t = Uri_lexer.t =
  { scheme : string option
  ; authority : string
  ; path : string
  }

let t_of_yojson json = Json.Conv.string_of_yojson json |> Uri_lexer.of_string

let to_string { scheme; authority; path } =
  let b = Buffer.create 64 in
  Option.iter scheme ~f:(fun s ->
      Buffer.add_string b s;
      Buffer.add_char b ':');
  Buffer.add_string b "//";
  Buffer.add_string b authority;
  if not (String.is_prefix path ~prefix:"/") then Buffer.add_char b '/';
  Buffer.add_string b path;
  Buffer.contents b

let yojson_of_t t = `String (to_string t)

let equal = Poly.equal

let hash = Poly.hash

let to_dyn { scheme; authority; path } =
  let open Dyn.Encoder in
  record
    [ ("scheme", (option string) scheme)
    ; ("authority", string authority)
    ; ("path", string path)
    ]

let to_path t =
  let path =
    t.path
    |> String.replace_all ~pattern:"\\" ~with_:"/"
    |> String.replace_all ~pattern:"%5C" ~with_:"/"
  in
  if Sys.win32 then
    path
  else
    Filename.concat "/" path

let of_path (path : string) =
  let path =
    String.drop_prefix ~prefix:"/" path |> Option.value ~default:path
  in
  let path = String.replace_all ~pattern:"\\" ~with_:"/" path in
  { path; scheme = Some "file"; authority = "" }
