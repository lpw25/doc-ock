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

val read_interface: 'a -> string -> Typedtree.signature ->
  'a DocOckPaths.Identifier.module_ *
  'a DocOckTypes.Documentation.t *
  'a DocOckTypes.Signature.t

val read_module_type : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature -> int ->
  Typedtree.module_type -> 'a DocOckTypes.ModuleType.expr

val read_value_description : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.value_description ->
  'a DocOckTypes.Signature.item list -> 'a DocOckSource.Use.t list ->
  'a DocOckTypes.Signature.item list * 'a DocOckSource.Use.t list

val read_type_declaration : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature -> 'a DocOckPaths.Defn.t option ->
  Typedtree.type_declaration ->
  'a DocOckTypes.Signature.item list -> 'a DocOckSource.Use.t list ->
  'a DocOckTypes.Signature.item list * 'a DocOckSource.Use.t list

val read_module_type_declaration : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.module_type_declaration -> 'a DocOckTypes.ModuleType.t

val read_class_type_declaration : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.class_type Typedtree.class_infos ->
  'a DocOckTypes.Signature.item list -> 'a DocOckSource.Use.t list ->
  'a DocOckTypes.Signature.item list * 'a DocOckSource.Use.t list



val add_core_type_uses : string -> Typedtree.core_type ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_core_type_option_uses : string -> Typedtree.core_type option ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_core_types_uses : string -> Typedtree.core_type list ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_value_description_uses : string -> Typedtree.value_description ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_extension_constructor_kind_uses : string ->
  Typedtree.extension_constructor_kind ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_extension_constructor_uses : string ->
  Typedtree.extension_constructor ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_type_extension_uses : string -> Typedtree.type_extension ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_type_declarations_uses : string -> Typedtree.type_declaration list ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_type_params_uses : string ->
  (Typedtree.core_type * Asttypes.variance) list ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_constructor_arguments_uses : string ->
  Typedtree.constructor_arguments ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_type_constraints_uses : string ->
  (Typedtree.core_type * Typedtree.core_type * Location.t) list ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_class_type_uses : string -> Typedtree.class_type ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_class_type_option_uses : string -> Typedtree.class_type option ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_class_type_declaration_uses : string ->
  Typedtree.class_type_declaration ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_module_type_uses : string -> Typedtree.module_type ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_module_type_option_uses : string -> Typedtree.module_type option ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_module_type_declaration_uses : string ->
  Typedtree.module_type_declaration ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_open_description_uses : string -> Typedtree.open_description ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list


val add_module_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_module_type_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_type_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_constructor_use : string -> Longident.t Location.loc ->
  Types.constructor_description ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_field_use : string -> Longident.t Location.loc ->
  Types.label_description ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_value_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_class_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val add_class_type_use : string -> Longident.t Location.loc -> Path.t ->
  'a DocOckSource.Use.t list -> 'a DocOckSource.Use.t list

val read_location : string -> Location.t -> DocOckSource.Location.t option
