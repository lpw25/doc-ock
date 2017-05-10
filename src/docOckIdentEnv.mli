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

module OCamlPath = Path
open DocOckPaths

type 'a t

val empty : 'a t

val add_module : 'a Identifier.Signature.t -> Ident.t -> 'a t -> 'a t

val add_parameter : 'a Identifier.Signature.t -> Ident.t -> 'a t -> 'a t

val add_module_type : 'a Identifier.Signature.t -> Ident.t -> 'a t -> 'a t

val add_type : 'a Identifier.Signature.t -> Ident.t -> 'a t -> 'a t

val add_class :
  'a Identifier.Signature.t ->
    Ident.t -> Ident.t -> Ident.t -> Ident.t -> 'a t -> 'a t

val add_class_type :
  'a Identifier.Signature.t -> Ident.t -> Ident.t -> Ident.t -> 'a t -> 'a t

val add_signature_type_items :
  'a Identifier.Signature.t -> Types.signature -> 'a t -> 'a t

val add_signature_tree_items :
  'a Identifier.Signature.t -> Typedtree.signature -> 'a t -> 'a t

val add_structure_tree_items :
  'a Identifier.Signature.t -> Typedtree.structure -> 'a t -> 'a t

module Path : sig

  val read_module : 'a t -> OCamlPath.t -> 'a Path.Module.t

  val read_module_type : 'a t -> OCamlPath.t -> 'a Path.ModuleType.t

  val read_type : 'a t -> OCamlPath.t -> 'a Path.Type.t

  val read_class_type : 'a t -> OCamlPath.t -> 'a Path.ClassType.t

end


module Fragment : sig

  val read_module : Longident.t -> 'a Fragment.Module.t

  val read_type : Longident.t -> 'a Fragment.Type.t

end
