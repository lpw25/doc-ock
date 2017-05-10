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

type t =
  | List of t list
  | Atom of string

val pp : Format.formatter -> t -> unit

type sexp = t

module type Sexpable0 = sig

  type t

  val to_sexp : t -> sexp

end

module type Sexpable1 = sig

  type 'a t

  val to_sexp : ('a -> sexp) -> 'a t -> sexp

end

module ModuleName :
  Sexpable0 with type t := DocOckNames.ModuleName.t

module FunctorParameterName :
  Sexpable0 with type t := DocOckNames.FunctorParameterName.t

module ModuleTypeName :
  Sexpable0 with type t := DocOckNames.ModuleTypeName.t

module TypeName :
  Sexpable0 with type t := DocOckNames.TypeName.t

module ConstructorName :
  Sexpable0 with type t := DocOckNames.ConstructorName.t

module FieldName :
  Sexpable0 with type t := DocOckNames.FieldName.t

module ExtensionName :
  Sexpable0 with type t := DocOckNames.ExtensionName.t

module ExceptionName :
  Sexpable0 with type t := DocOckNames.ExceptionName.t

module ValueName :
  Sexpable0 with type t := DocOckNames.ValueName.t

module ClassName :
  Sexpable0 with type t := DocOckNames.ClassName.t

module ClassTypeName :
  Sexpable0 with type t := DocOckNames.ClassTypeName.t

module MethodName :
  Sexpable0 with type t := DocOckNames.MethodName.t

module InstanceVariableName :
  Sexpable0 with type t := DocOckNames.InstanceVariableName.t

module UnitName :
  Sexpable0 with type t := DocOckNames.UnitName.t

module PageName :
  Sexpable0 with type t := DocOckNames.PageName.t

module LabelName :
  Sexpable0 with type t := DocOckNames.LabelName.t

module Identifier :
  Sexpable1 with type 'a t := 'a DocOckPaths.Identifier.t

module Path : sig

  include Sexpable1 with type 'a t := 'a DocOckPaths.Path.t

  module Resolved : Sexpable1
    with type 'a t := 'a DocOckPaths.Path.Resolved.t

end

module Fragment : sig

  include Sexpable1
    with type 'a t := 'a DocOckPaths.Fragment.t

  module Resolved : Sexpable1
    with type 'a t := 'a DocOckPaths.Fragment.Resolved.t

end

module Reference : sig

  include Sexpable1
    with type 'a t := 'a DocOckPaths.Reference.t

  module Resolved : Sexpable1
    with type 'a t := 'a DocOckPaths.Reference.Resolved.t

end
