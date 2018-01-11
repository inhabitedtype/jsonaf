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

type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array  of t list ]

type bigstring = Angstrom.bigstring
 constraint bigstring = Faraday.bigstring

val parse : t Angstrom.t
val serialize : t -> Faraday.t -> unit

val of_string    : string    -> (t, string) Result.result
val of_bigstring : bigstring -> (t, string) Result.result

val to_string    : t -> string
val to_bigstring : t -> bigstring

module With_number : sig
  type 'number t =
    [ `Null
    | `False
    | `True
    | `String of string
    | `Number of 'number
    | `Object of (string * 'number t) list
    | `Array  of 'number t list ]

  val parse
    :  (string -> ('number, string) Result.result)
    -> 'number t Angstrom.t

  val serialize
    :  (Faraday.t -> 'number -> unit)
    -> 'number t
    -> Faraday.t
    -> unit

  val of_string
    : (string -> ('number, string) Result.result)
    -> string
    -> ('number t, string) Result.result

  val of_bigstring
    : (string -> ('number, string) Result.result)
    -> Angstrom.bigstring
    -> ('number t, string) Result.result

  val to_string
    :  (Faraday.t -> 'number -> unit)
    -> 'number t
    -> string

  val to_bigstring
    : (Faraday.t -> 'number -> unit)
    -> 'number t
    -> bigstring
end
