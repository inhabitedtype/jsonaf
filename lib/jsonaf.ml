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

module List = ListLabels

type bigstring = Angstrom.bigstring
 constraint bigstring = Faraday.bigstring

module With_number = struct
  type 'number t =
    [ `Null
    | `False
    | `True
    | `String of string
    | `Number of 'number
    | `Object of (string * 'number t) list
    | `Array  of 'number t list ]

  module Parser = struct
    open Angstrom

    let ws = skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false)

    let lchar c =
      ws *> char c

    let rsb = lchar ']'
    let rcb = lchar '}'
    let ns, vs  = lchar ':', lchar ','
    let quo = lchar '"'

    let _false = string "false" *> return `False
    let _true  = string "true"  *> return `True
    let _null  = string "null"  *> return `Null

  let num number =
    take_while1 (function
      | '\x20' | '\x0a' | '\x0d' | '\x09'
      | '[' | ']' | '{' | '}' | ':' | ',' -> false
      | _               -> true)
    >>= fun s ->
      match number s with
      | Ok x      -> return (`Number x)
      | Error msg -> fail msg
    ;;

    let parse number =
      let open Angstrom in
      let advance1 = advance 1 in
      let pair x y = (x, y) in
      let buf = Buffer.create 0x1000 in
      let str = Json_string.parse buf in
      fix (fun json ->
        let mem = lift2 pair (quo *> str <* ns) json in
        let obj = advance1 *> sep_by vs mem  <* rcb >>| fun ms -> `Object ms in
        let arr = advance1 *> sep_by vs json <* rsb >>| fun vs -> `Array  vs in
        let str = advance1 *> str >>| fun s -> `String s in
        ws *> peek_char_fail
        >>= function
          | 'f' -> _false
          | 'n' -> _null
          | 't' -> _true
          | '{' -> obj
          | '[' -> arr
          | '"' -> str
          | _   -> num number) <?> "json"
  end

  let parse = Parser.parse

  let of_string number_of_string string =
    Angstrom.parse_string ~consume:Prefix (parse number_of_string) string

  let of_bigstring number_of_string bigstring =
    Angstrom.parse_bigstring ~consume:Prefix (parse number_of_string) bigstring

  let rec serialize serialize_number t faraday =
    match t with
    | `Null  -> Faraday.write_string faraday "null"
    | `False -> Faraday.write_string faraday "false"
    | `True  -> Faraday.write_string faraday "true"
    | `String string -> Json_string.serialize faraday string
    | `Number number -> serialize_number      faraday number
    | `Object []     -> Faraday.write_string faraday "{}"
    | `Object ((k, v) :: kvs)->
      Faraday.write_char faraday '{';
      serialize_kv faraday serialize_number k v;
      List.iter kvs ~f:(fun (k, v) ->
        Faraday.write_char faraday ',';
        serialize_kv       faraday serialize_number k v);
      Faraday.write_char faraday '}';
    | `Array []      -> Faraday.write_string faraday "[]"
    | `Array (t :: ts) ->
      Faraday.write_char faraday '[';
      serialize serialize_number t faraday;
      List.iter ts ~f:(fun t ->
        Faraday.write_char faraday ',';
        serialize serialize_number t faraday);
      Faraday.write_char faraday ']';
  and serialize_kv faraday serialize_number k v =
    Faraday.write_char   faraday '"';
    Faraday.write_string faraday k;
    Faraday.write_char   faraday '"';
    Faraday.write_char   faraday ':';
    serialize serialize_number v faraday;
  ;;

  let to_string serialize_number t =
    let faraday = Faraday.create 0x1000 in
    serialize serialize_number t faraday;
    Faraday.serialize_to_string faraday;
  ;;

  let to_bigstring serialize_number t =
    let faraday = Faraday.create 0x1000 in
    serialize serialize_number t faraday;
    Faraday.serialize_to_bigstring faraday;
  ;;
end

type t = string With_number.t

let parse : t Angstrom.t =
  With_number.parse (fun x -> Ok x)
;;

let serialize (t : t) faraday =
  With_number.serialize (fun f n -> Faraday.write_string f n) t faraday
;;

let of_string    = With_number.of_string    (fun x -> Ok x)
let of_bigstring = With_number.of_bigstring (fun x -> Ok x)

let to_string    = With_number.to_string    (fun f n -> Faraday.write_string f n)
let to_bigstring = With_number.to_bigstring (fun f n -> Faraday.write_string f n)
