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
open DocOckTypes.Documentation

val empty : 'a t

val read_module_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Module.t ->
    Parsetree.attributes -> 'a t

val read_module_type_attributes :
  'a Identifier.Signature.t -> 'a Identifier.ModuleType.t ->
    Parsetree.attributes -> 'a t

val read_type_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Type.t ->
    Parsetree.attributes -> 'a t

val read_constructor_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Constructor.t ->
    Parsetree.attributes -> 'a t

val read_field_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Field.t ->
    Parsetree.attributes -> 'a t

val read_extension_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Extension.t ->
    Parsetree.attributes -> 'a t

val read_exception_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Exception.t ->
    Parsetree.attributes -> 'a t

val read_value_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Value.t ->
    Parsetree.attributes -> 'a t

val read_class_attributes :
  'a Identifier.Signature.t -> 'a Identifier.Class.t ->
    Parsetree.attributes -> 'a t

val read_class_type_attributes :
  'a Identifier.Signature.t -> 'a Identifier.ClassType.t ->
    Parsetree.attributes -> 'a t

val read_method_attributes :
  'a Identifier.ClassSignature.t -> 'a Identifier.Method.t ->
    Parsetree.attributes -> 'a t

val read_instance_variable_attributes :
  'a Identifier.ClassSignature.t -> 'a Identifier.InstanceVariable.t ->
    Parsetree.attributes -> 'a t

val read_signature_attributes :
  'a Identifier.Signature.t -> Parsetree.attributes -> 'a t

val read_class_signature_attributes :
  'a Identifier.ClassSignature.t -> Parsetree.attributes -> 'a t

val read_signature_comment :
  'a Identifier.Signature.t -> Parsetree.attribute -> 'a comment option

val read_class_signature_comment :
  'a Identifier.ClassSignature.t -> Parsetree.attribute -> 'a comment option

val read_signature_comments :
  'a Identifier.Signature.t -> Parsetree.attributes -> 'a comment list

val read_class_signature_comments :
  'a Identifier.ClassSignature.t -> Parsetree.attributes -> 'a comment list

val read_string : 'a Identifier.LabelParent.t -> Location.t -> string -> 'a comment
(** The parent identifier is used to define labels in the given string (i.e.
    for things like [{1:some_section Some title}]) and the location is used for
    error messages.

    This function is meant to be used to read arbitrary files containing text in
    the ocamldoc syntax. *)
