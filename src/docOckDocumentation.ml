(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOckPaths

type style =
  | Bold
  | Italic
  | Emphasize
  | Center
  | Left
  | Right
  | Superscript
  | Subscript
  | Custom of string

type 'a reference =
  | Element of 'a Reference.any
  | Link of string
  | Custom of string * string

type see =
  | Url of string
  | File of string
  | Doc of string

type 'a text = 'a text_element list

and 'a text_element =
  | Raw of string
  | Code of string
  | PreCode of string
  | Verbatim of string
  | Style of style * 'a text
  | List of 'a text list
  | Enum of 'a text list
  | Newline
  | Title of int * 'a Identifier.label option * 'a text
  | Reference of 'a reference * 'a text option
  | Target of string option * string
  | Special of 'a special

and 'a tag =
  | Author of string
  | Version of string
  | See of see * 'a text
  | Since of string
  | Before of string * 'a text
  | Deprecated of 'a text
  | Param of string * 'a text
  | Raise of string * 'a text
  | Return of 'a text
  | Inline
  | Tag of string * 'a text
  | Canonical of 'a Path.module_ * 'a Reference.module_

and 'a special =
  | Modules of ('a Reference.module_ * 'a text) list
  | Index


module Error = struct

  module Position = struct

    type t =
      { line: int;
        column: int; }

  end

  module Offset = struct

    type t =
      { start: Position.t;
        finish: Position.t; }

  end

  module Location = struct

    type t =
      { filename: string;
        start: Position.t;
        finish: Position.t; }

  end

  type 'a t =
    { origin: 'a Identifier.any; (** TODO remove this *)
      offset: Offset.t;
      location: Location.t option;
      message: string; }

end

type 'a body =
  { text: 'a text;
    tags: 'a tag list; }

type 'a t =
  | Ok of 'a body
  | Error of 'a Error.t

type 'a comment =
  | Documentation of 'a t
  | Stop

