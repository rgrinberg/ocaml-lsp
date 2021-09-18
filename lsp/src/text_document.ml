open Types
module String = StringLabels

let find_offset ~utf8 ~(utf16_position : Position.t) =
  let dec =
    Uutf.decoder
      ~nln:(`ASCII (Uchar.of_char '\n'))
      ~encoding:`UTF_8 (`String utf8)
  in
  let rec find_char bytes enc char =
    if char = 0 || Uutf.decoder_line dec = utf16_position.line then
      Uutf.decoder_byte_count dec
    else
      match Uutf.decode dec with
      | `Await -> assert false
      | `End -> Uutf.decoder_byte_count dec
      | `Malformed _ -> assert false
      | `Uchar _ as u ->
        Uutf.Manual.dst enc bytes 0 4;
        (match Uutf.encode enc u with
        | `Partial -> assert false
        | `Ok -> ());
        let char = char - ((4 - Uutf.Manual.dst_rem enc) / 2) in
        find_char bytes enc char
  in
  let rec find_line () =
    if Uutf.decoder_line dec - 1 = utf16_position.line then
      let enc = Uutf.encoder `UTF_16LE `Manual in
      find_char (Bytes.create 4) enc utf16_position.character
    else
      match Uutf.decode dec with
      | `Malformed _
      | `Uchar _ ->
        find_line ()
      | `Await -> assert false
      | `End -> Uutf.decoder_byte_count dec
  in
  find_line ()

(* Text is received as UTF-8. However, the protocol specifies offsets should be
   computed based on UTF-16. Therefore we reencode every file into utf16 for
   analysis. *)

type t = TextDocumentItem.t

let text (t : TextDocumentItem.t) = t.text

let make (t : DidOpenTextDocumentParams.t) = t.textDocument

let documentUri (t : TextDocumentItem.t) = t.uri

let version (t : TextDocumentItem.t) = t.version

let languageId (t : TextDocumentItem.t) = t.languageId

let apply_content_change ?version (t : TextDocumentItem.t)
    (change : TextDocumentContentChangeEvent.t) =
  (* Changes can only be applied using utf16 offsets *)
  let version =
    match version with
    | None -> t.version + 1
    | Some version -> version
  in
  match change.range with
  | None -> { t with version; text = change.text }
  | Some { Range.start; end_ } ->
    let start_offset = find_offset ~utf8:t.text ~utf16_position:start in
    let end_offset = find_offset ~utf8:t.text ~utf16_position:end_ in
    let text =
      String.concat ~sep:""
        [ String.sub t.text ~pos:0 ~len:start_offset
        ; change.text
        ; String.sub t.text ~pos:end_offset
            ~len:(String.length t.text - end_offset)
        ]
    in
    { t with text }
