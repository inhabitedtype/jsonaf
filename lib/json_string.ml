(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module Parse = struct
  open Angstrom

  type t =
    [ `Unescaped
    | `Escaped
    | `UTF8  of char list
    | `UTF16 of int * [`S | `U | `C of char list]
    | `Error of string
    | `Done ]

  let to_string : [`Terminate | t] -> string = function
    | `Unescaped -> "unescaped"
    | `Escaped   -> "escaped"
    | `UTF8 _    -> "utf-8 _"
    | `UTF16 _   -> "utf-16 _ _"
    | `Error e   -> Printf.sprintf "error %S" e
    | `Terminate -> "terminate"
    | `Done      -> "done"

  let unescaped buf = function
    | '"'  -> `Terminate
    | '\\' -> `Escaped
    | c    ->
      if c <= '\031'
      then `Error (Printf.sprintf "unexpected character '%c'" c)
      else begin Buffer.add_char buf c; `Unescaped end

  let escaped buf = function
    | '\x22' -> Buffer.add_char buf '\x22'; `Unescaped
    | '\x5c' -> Buffer.add_char buf '\x5c'; `Unescaped
    | '\x2f' -> Buffer.add_char buf '\x2f'; `Unescaped
    | '\x62' -> Buffer.add_char buf '\x08'; `Unescaped
    | '\x66' -> Buffer.add_char buf '\x0c'; `Unescaped
    | '\x6e' -> Buffer.add_char buf '\x0a'; `Unescaped
    | '\x72' -> Buffer.add_char buf '\x0d'; `Unescaped
    | '\x74' -> Buffer.add_char buf '\x09'; `Unescaped
    | '\x75' -> `UTF8 []
    | _      -> `Error "invalid escape sequence"

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _          -> 255

  (* Copied from Yojson *)
  let utf8_of_code buf x =

    (* Straight <= doesn't work with signed 31-bit ints *)
    let maxbits n x = x lsr n = 0 in

    if maxbits 7 x then
      (* 7 *)
      Buffer.add_char buf (Char.unsafe_chr x)
    else if maxbits 11 x then begin
      (* 5 + 6 *)
      Buffer.add_char buf (Char.unsafe_chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (x         land 0b00111111)))
    end else if maxbits 16 x then begin
      (* 4 + 6 + 6 *)
      Buffer.add_char buf (Char.unsafe_chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (x          land 0b00111111)))
    end else if maxbits 21 x then begin
      (* 3 + 6 + 6 + 6 *)
      Buffer.add_char buf (Char.unsafe_chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (x          land 0b00111111)));
    end else if maxbits 26 x then begin
      (* 2 + 6 + 6 + 6 + 6 *)
      Buffer.add_char buf (Char.unsafe_chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (x          land 0b00111111)));
    end else begin
      assert (maxbits 31 x);
      (* 1 + 6 + 6 + 6 + 6 + 6 *)
      Buffer.add_char buf (Char.unsafe_chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
      Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (x          land 0b00111111)));
    end

  let utf_8 buf d = function
    | [c;b;a] ->
      let a = hex a and b = hex b and c = hex c and d = hex d in
      if a lor b lor c lor d = 255 then
        `Error "invalid hex escape"
      else
        let cp = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if cp >= 0xd800 && cp <= 0xdbff then
          `UTF16(cp, `S)
        else begin
          utf8_of_code buf cp ;
          `Unescaped
        end
    | cs -> `UTF8 (d::cs)

  let utf_16 buf d x s =
    match s, d with
    | `S        , '\\' -> `UTF16(x, `U)
    | `U        , 'u'  -> `UTF16(x, `C [])
    | `C [c;b;a], _    ->
      let a = hex a and b = hex b and c = hex c and d = hex d in
      if a lor b lor c lor d = 255 then
        `Error "invalid hex escape"
      else
        let y = (a  lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if y >= 0xdc00 && y <= 0xdfff then begin
          let hi = x - 0xd800 in
          let lo = y - 0xdc00 in
          let cp = 0x10000 + ((hi lsl 10) lor lo) in
          Buffer.add_char buf (Char.unsafe_chr (0b11110000 lor ((cp lsr 18) land 0b00000111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((cp lsr 12) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((cp lsr  6) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp          land 0b00111111)));
          `Unescaped
        end else
          `Error "invalid escape sequence for utf-16 low surrogate"
    | `C cs,      _    -> `UTF16(x, `C (d::cs))
    | _, _             -> `Error "invalid escape sequence for utf-16 low surrogate"

  let str buf =
    let state : t ref = ref `Unescaped in
    skip_while (fun c ->
      match
        begin match !state with
        | `Unescaped    -> unescaped buf c
        | `Escaped      -> escaped   buf c
        | `UTF8 cs      -> utf_8     buf c cs
        | `UTF16(x, cs) -> utf_16    buf c x cs
        | (`Error _ | `Done) as state -> state
        end
      with
        | (`Error _) | `Done -> false
        | `Terminate         -> state := `Done; true
        | #t as state'       -> state := state'; true)
    >>= fun () ->
      match !state with
      | `Done ->
        let result = Buffer.contents buf in
        Buffer.clear buf;
        state := `Unescaped;
        return result
      | `Error msg ->
        Buffer.clear buf; state := `Unescaped; fail msg
      | `Unescaped | `Escaped | `UTF8 _ | `UTF16 _ ->
        Buffer.clear buf; state := `Unescaped; fail "unterminated string"
end

let parse buffer = 
  Parse.str buffer

let to_hex_digit i =
	if i < 10 then i + 48 else i + 87
;;

let serialize t s =
  let open Faraday in
  (* TODO: Implement proper unicode verification. *)
  let flush ~off ~len = 
    if len <> 0 then write_string t ~off ~len s in
  let rec go ~off ~len = 
    if String.length s = off + len 
    then flush ~off ~len
    else
      let i = off + len in
      match String.get s i with
      | c when c <= '\031' -> (* non-visible characters have to be escaped *)
        let c = Char.code c in
        flush ~off ~len;
        write_string t "\\u00"; 
        write_uint8 t (to_hex_digit (c lsr 4));
        write_uint8 t (to_hex_digit (c land 0xf));
        go ~off:(i+1) ~len:0;
      | '"'    -> flush ~off ~len; write_string t "\\\""; go ~off:(i+1) ~len:0
      | '\b'   -> flush ~off ~len; write_string t "\\b" ; go ~off:(i+1) ~len:0
      | '\012' -> flush ~off ~len; write_string t "\\f" ; go ~off:(i+1) ~len:0
      | '\n'   -> flush ~off ~len; write_string t "\\n" ; go ~off:(i+1) ~len:0
      | '\r'   -> flush ~off ~len; write_string t "\\r" ; go ~off:(i+1) ~len:0
      | '\t'   -> flush ~off ~len; write_string t "\\t" ; go ~off:(i+1) ~len:0
      | '\\'   -> flush ~off ~len; write_string t "\\\\"; go ~off:(i+1) ~len:0
      | _      -> go ~off ~len:(len + 1)
  in
  write_char t '"';
  go ~off:0 ~len:0;
	write_char t '"'
;;
