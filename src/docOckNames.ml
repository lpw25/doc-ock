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

module type Name = sig

  type t

  val to_string : t -> string

  val of_string : string -> t

  val of_ident : Ident.t -> t

  val equal : t -> t -> bool

  val is_hidden : t -> bool

end

module Name : Name = struct

  type t = string

  let to_string s = s

  let of_string s =
    match s with
    | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
    | "lxor" | "mod" -> "(" ^ s ^ ")"
    | _ ->
      if (String.length s > 0) then
        match s.[0] with
        | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_'
        | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> s
        | _ -> "(" ^ s ^ ")"
      else s

  let of_ident id = of_string (Ident.name id)

  let equal = String.equal

  let is_hidden s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then false else
      if Char.equal s.[i] '_' && Char.equal s.[i + 1] '_' then true
      else aux (i + 1)
    in
    aux 0

end

module ModuleName = Name

module FunctorParameterName = Name

module ModuleTypeName = Name

module TypeName = Name

module ConstructorName = Name

module FieldName = Name

module ExtensionName = Name

module ExceptionName = Name

module ValueName = Name

module ClassName = Name

module ClassTypeName = Name

module MethodName = Name

module InstanceVariableName = Name

module UnitName = Name

module LabelName = Name

module PageName = Name
