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

(** Paths of documentation *)

(** {3 Sexp serialization} *)

type sexp =
  | List of sexp list
  | Atom of string

val string_of_sexp : sexp -> string

(**/**)

val contains_double_underscore : string -> bool
(* not the best place for this but. *)

(**/**)

(** {1 Paths} *)

(** Every path is annotated with its kind. *)
module Kind : sig

  (** {4 General purpose kinds} *)

  (** Any possible referent *)
  type any =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label | `Page ]

  (** A referent that can contain signature items *)
  type signature = [ `Module | `ModuleType ]

  (** A referent that can contain class signature items *)
  type class_signature = [ `Class | `ClassType ]

  (** A referent that can contain datatype items *)
  type datatype = [ `Type ]

  (** A referent that can contain page items *)
  type page = [ `Page ]

  (** A referent that can contain other items *)
  type parent = [ signature | class_signature | datatype ]

  type label_parent = [ parent | page ]

  (** {4 Identifier kinds}

      The kind of an identifier directly corresponds to the kind of its
      referent. *)

  type identifier = any

  type identifier_module = [ `Module ]
  type identifier_module_type = [ `ModuleType ]
  type identifier_type =  [ `Type ]
  type identifier_constructor = [ `Constructor ]
  type identifier_field = [ `Field ]
  type identifier_extension = [ `Extension ]
  type identifier_exception = [ `Exception ]
  type identifier_value = [ `Value ]
  type identifier_class = [ `Class ]
  type identifier_class_type = [ `ClassType ]
  type identifier_method = [ `Method ]
  type identifier_instance_variable = [ `InstanceVariable ]
  type identifier_label = [ `Label ]
  type identifier_page = [ `Page ]

  (** {4 Path kinds}

      There are four kinds of OCaml path:

        - module
        - module type
        - type
        - class type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path may refer to a class definition). *)

  type path = [ `Module | `ModuleType | `Type | `Class | `ClassType ]

  type path_module = [ `Module ]
  type path_module_type = [ `ModuleType ]
  type path_type = [ `Type | `Class | `ClassType ]
  type path_class_type = [ `Class | `ClassType ]

  (** {4 Fragment kinds}

      There are two kinds of OCaml path fragment:

        - module
        - type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path fragment may refer to a class
      definition). *)

  type fragment = [ `Module | `Type | `Class | `ClassType ]

  type fragment_module = [ `Module ]
  type fragment_type = [ `Type | `Class | `ClassType ]

  (** {4 Reference kinds}

      There is one reference kind for each kind of referent. However,
      the kind of a reference does not refer to the kind of its
      referent, but to the kind with which the reference was annotated.

      This means that reference kinds do not correspond directly to the
      kind of their referent because we used more relaxed rules when
      resolving a reference. For example, a reference annotated as being
      to a constructor can be resolved to the definition of an exception
      (which is a sort of constructor). *)

  type reference = any

  type reference_module = [ `Module ]
  type reference_module_type = [ `ModuleType ]
  type reference_type = [ `Type | `Class | `ClassType ]
  type reference_constructor = [ `Constructor | `Extension | `Exception ]
  type reference_field = [ `Field ]
  type reference_extension = [ `Extension | `Exception ]
  type reference_exception = [ `Exception ]
  type reference_value = [ `Value ]
  type reference_class = [ `Class ]
  type reference_class_type = [ `Class | `ClassType ]
  type reference_method = [ `Method ]
  type reference_instance_variable = [ `InstanceVariable ]
  type reference_label = [ `Label ]
  type reference_page = [ `Page ]

end

open Kind

(** Identifiers for definitions *)
module Identifier : sig

  type kind = Kind.identifier

  type ('a, 'b) t =
    | Root : 'a * string -> ('a, [< kind > `Module]) t
    | Page : 'a * string -> ('a, [< kind > `Page]) t
    | Module : 'a signature * string -> ('a, [< kind > `Module]) t
    | Argument : 'a signature * int * string -> ('a, [< kind > `Module]) t
    | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
    | Type : 'a signature * string -> ('a, [< kind > `Type]) t
    | CoreType : string -> ('a, [< kind > `Type]) t
    | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
    | Field : 'a parent * string -> ('a, [< kind > `Field]) t
    | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
    | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
    | CoreException : string -> ('a, [< kind > `Exception]) t
    | Value : 'a signature * string -> ('a, [< kind > `Value]) t
    | Class : 'a signature * string -> ('a, [< kind > `Class]) t
    | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
    | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
    | InstanceVariable : 'a class_signature * string ->
                           ('a, [< kind > `InstanceVariable]) t
    | Label : 'a label_parent * string -> ('a, [< kind > `Label]) t

  and 'a any = ('a, kind) t
  and 'a signature = ('a, Kind.signature) t
  and 'a class_signature = ('a, Kind.class_signature) t
  and 'a datatype = ('a, Kind.datatype) t
  and 'a parent = ('a, Kind.parent) t
  and 'a label_parent = ('a, Kind.label_parent) t

  type 'a module_ = ('a, identifier_module) t
  type 'a module_type = ('a, identifier_module_type) t
  type 'a type_ =  ('a, identifier_type) t
  type 'a constructor = ('a, identifier_constructor) t
  type 'a field = ('a, identifier_field) t
  type 'a extension = ('a, identifier_extension) t
  type 'a exception_ = ('a, identifier_exception) t
  type 'a value = ('a, identifier_value) t
  type 'a class_ = ('a, identifier_class) t
  type 'a class_type = ('a, identifier_class_type) t
  type 'a method_ = ('a, identifier_method) t
  type 'a instance_variable = ('a, identifier_instance_variable) t
  type 'a label = ('a, identifier_label) t
  type 'a page = ('a, identifier_page) t

  type 'a path_module = ('a, Kind.path_module) t
  type 'a path_module_type = ('a, Kind.path_module_type) t
  type 'a path_type =  ('a, Kind.path_type) t
  type 'a path_class_type = ('a, Kind.path_class_type) t

  type 'a fragment_module = ('a, Kind.fragment_module) t
  type 'a fragment_type =  ('a, Kind.fragment_type) t

  type 'a reference_module = ('a, Kind.reference_module) t
  type 'a reference_module_type = ('a, Kind.reference_module_type) t
  type 'a reference_type =  ('a, Kind.reference_type) t
  type 'a reference_constructor = ('a, Kind.reference_constructor) t
  type 'a reference_field = ('a, Kind.reference_field) t
  type 'a reference_extension = ('a, Kind.reference_extension) t
  type 'a reference_exception = ('a, Kind.reference_exception) t
  type 'a reference_value = ('a, Kind.reference_value) t
  type 'a reference_class = ('a, Kind.reference_class) t
  type 'a reference_class_type = ('a, Kind.reference_class_type) t
  type 'a reference_method = ('a, Kind.reference_method) t
  type 'a reference_instance_variable = ('a, Kind.reference_instance_variable) t
  type 'a reference_label = ('a, Kind.reference_label) t
  type 'a reference_page = ('a, Kind.reference_page) t

  (** {2 Explicit coercions} *)

  val signature_of_module : 'a module_ -> 'a signature

  val signature_of_module_type : 'a module_type -> 'a signature

  val class_signature_of_class : 'a class_ -> 'a class_signature

  val class_signature_of_class_type : 'a class_type -> 'a class_signature

  val datatype_of_type : 'a type_ -> 'a datatype

  val parent_of_signature : 'a signature -> 'a parent

  val parent_of_class_signature : 'a class_signature -> 'a parent

  val parent_of_datatype : 'a datatype -> 'a parent

  val label_parent_of_parent : 'a parent -> 'a label_parent

  val label_parent_of_page : 'a page -> 'a label_parent

  val any : ('a, 'b) t -> 'a any

  (** {2 Generic operations} *)

  val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val hash : hash:('a -> int) -> ('a, 'b) t -> int

  (** {3 Printing} *)

  val name : ('a, 'b) t -> string

  val sexp_of_t : ('a -> sexp) -> ('a, _) t -> sexp

  (** {2 Root retrieval} *)

  val signature_root : 'a signature -> 'a * string

  val module_root : 'a module_ -> 'a * string

  val module_type_root : 'a module_type -> 'a * string

  val class_signature_root : 'a class_signature -> 'a * string
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    type kind = Kind.path

    type ('a, 'b) t =
      | Identifier : ('a, 'b) Identifier.t -> ('a, [< kind] as 'b) t
      | Subst : 'a module_type * 'a module_ -> ('a, [< kind > `Module]) t
      | SubstAlias : 'a module_ * 'a module_ -> ('a, [< kind > `Module]) t
      | Hidden : 'a module_ -> ('a, [< kind > `Module ]) t
      | Module : 'a module_ * string -> ('a, [< kind > `Module]) t
        (* TODO: The canonical path should be a reference not a path *)
      | Canonical : 'a module_ * 'a Path.module_ -> ('a, [< kind > `Module]) t
      | Apply : 'a module_ * 'a Path.module_ -> ('a, [< kind > `Module]) t
      | ModuleType : 'a module_ * string -> ('a, [< kind > `ModuleType]) t
      | Type : 'a module_ * string -> ('a, [< kind > `Type]) t
      | Class : 'a module_ * string -> ('a, [< kind > `Class]) t
      | ClassType : 'a module_ * string -> ('a, [< kind > `ClassType]) t

    and 'a any = ('a, kind) t
    and 'a module_ = ('a, path_module) t
    and 'a module_type = ('a, path_module_type) t
    and 'a type_ = ('a, path_type) t
    and 'a class_type = ('a, path_class_type) t

    (** {2 Creators} *)

    val ident_module : 'a Identifier.module_ -> ('a, [< kind > `Module]) t

    val ident_module_type : 'a Identifier.module_type ->
          ('a, [< kind > `ModuleType]) t

    val ident_type : 'a Identifier.type_ -> ('a, [< kind > `Type]) t

    val ident_class : 'a Identifier.class_ -> ('a, [< kind > `Class]) t

    val ident_class_type : 'a Identifier.class_type ->
          ('a, [< kind > `ClassType]) t

    (** {2 Explicit coercion} *)

    val any : ('a, 'b) t -> 'a any

    (** {2 Generic operations} *)

    val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

    val hash : hash:('a -> int) -> ('a, 'b) t -> int

    val identifier: ('a, 'b) t -> ('a, 'b) Identifier.t
    (** [identifier rp] extracts the identifier present at the "root" of [rp]. *)

    val is_hidden : ('a, 'b) t -> bool
    (** [is_hidden rp] is [true] when some prefix of [rp] (which is not under a
        [Canonical]) is the [Hidden] constructor.

        [Canonical] are treated specialy because we expect them to rewrite a
        hidden path to a non-hidden one. *)

    val sexp_of_t : ('a -> sexp) -> ('a, _) t -> sexp

    val rebase : 'a Identifier.signature -> ('a, 'b) t -> ('a, 'b) t

    val equal_identifier : equal:('a -> 'a -> bool) -> ('a, 'b) Identifier.t ->
      ('a, 'b) t -> bool
  end

  type kind = Kind.path

  type ('a, 'b) t =
    | Resolved : ('a, 'b) Resolved.t -> ('a, 'b) t
    | Root : string -> ('a, [< kind > `Module]) t
    | Forward : string -> ('a, [< kind > `Module]) t
    | Dot : 'a module_ * string -> ('a, [< kind]) t
    | Apply : 'a module_ * 'a module_ -> ('a, [< kind > `Module]) t

  and 'a any = ('a, kind) t
  and 'a module_ = ('a, path_module) t
  and 'a module_type = ('a, path_module_type) t
  and 'a type_ = ('a, path_type) t
  and 'a class_type = ('a, path_class_type) t

  (** {2 Creators} *)

  val ident_module : 'a Identifier.module_ -> ('a, [< kind > `Module]) t

  val ident_module_type : 'a Identifier.module_type ->
        ('a, [< kind > `ModuleType]) t

  val ident_type : 'a Identifier.type_ -> ('a, [< kind > `Type]) t

  val ident_class : 'a Identifier.class_ -> ('a, [< kind > `Class]) t

  val ident_class_type : 'a Identifier.class_type ->
        ('a, [< kind > `ClassType]) t

  val module_ : 'a module_ -> string -> ('a, [< kind > `Module]) t

  val apply : 'a module_ -> 'a module_ -> ('a, [< kind > `Module]) t

  val module_type : 'a module_ -> string -> ('a, [< kind > `ModuleType]) t

  val type_ : 'a module_ -> string -> ('a, [< kind > `Type]) t

  val class_ : 'a module_ -> string -> ('a, [< kind > `Class]) t

  val class_type_ : 'a module_ -> string -> ('a, [< kind > `ClassType]) t

  (** {2 Explicit coercions} *)

  val any : ('a, 'b) t -> 'a any

  val type_of_class_type : 'a class_type -> 'a type_

  (** {2 Generic operations} *)

  val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val hash : hash:('a -> int) -> ('a, 'b) t -> int

  val sexp_of_t : ('a -> sexp) -> ('a, _) t -> sexp

  val is_hidden : ('a, 'b) t -> bool
  (** cf. {!Resolved.is_hidden} *)
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    type kind = Kind.fragment

    type sort = [ `Root | `Branch ]

    type ('a, 'b, 'c) raw =
      | Root : ('a, 'b, [< sort > `Root]) raw
      | Subst : 'a Path.Resolved.module_type * 'a module_ ->
                ('a, [< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | SubstAlias : 'a Path.Resolved.module_ * 'a module_ ->
                ('a, [< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
      | Module : 'a signature * string -> ('a, [< kind > `Module], [< sort > `Branch]) raw
      | Type : 'a signature * string -> ('a, [< kind > `Type], [< sort > `Branch]) raw
      | Class : 'a signature * string -> ('a, [< kind > `Class], [< sort > `Branch]) raw
      | ClassType : 'a signature * string -> ('a, [< kind > `ClassType], [< sort > `Branch]) raw

    and ('a, 'b) t = ('a, 'b, [`Branch]) raw
    and 'a any = ('a, kind) t
    and 'a signature = ('a, fragment_module, [`Root | `Branch]) raw
    and 'a module_ = ('a, fragment_module) t

    type 'a type_ = ('a, fragment_type) t

    (** {2 Explicit coercions} *)

    val signature_of_module : 'a module_ -> 'a signature

    val any : ('a, 'b) t -> 'a any

    val any_sort : ('a, 'b, 'c) raw -> ('a, 'b, sort) raw

    (** {2 Attaching fragments to valid paths} *)

    val path: 'a Path.module_ -> ('a, 'b) t -> ('a, 'b) Path.t

    val identifier: 'a Identifier.signature -> ('a, 'b) t ->
                    ('a, 'b) Identifier.t

    (** {2 Generic operations} *)

    val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

    val hash : hash:('a -> int) -> ('a, 'b) t -> int

    val sexp_of_t : ('a -> sexp) -> ('a, _, _) raw -> sexp

    val split : ('a, 'b) t -> string * ('a, 'b) t option

  end

  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('a, 'b, 'c) raw =
    | Resolved : ('a, 'b, 'c) Resolved.raw -> ('a, 'b, 'c) raw
    | Dot : 'a signature * string -> ('a, [< kind], [< sort > `Branch]) raw

  and ('a, 'b) t = ('a, 'b, [`Branch]) raw
  and 'a any = ('a, kind) t
  and 'a signature = ('a, fragment_module, [`Root | `Branch]) raw

  type 'a module_ = ('a, fragment_module) t
  type 'a type_ = ('a, fragment_type) t

  (** {2 Explicit coercions} *)

  val signature_of_module : 'a module_ -> 'a signature

  val any_sort : ('a, 'b, 'c) raw -> ('a, 'b, sort) raw

  val any : ('a, 'b) t -> 'a any

  (** {2 Attaching fragments to valid paths} *)

  val path: 'a Path.module_ -> ('a, 'b) t -> ('a, 'b) Path.t

  (** {2 Generic operations} *)

  val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val hash : hash:('a -> int) -> ('a, 'b) t -> int

  val sexp_of_t : ('a -> sexp) -> ('a, _, _) raw -> sexp

  val split: ('a, 'b) t -> string * ('a, 'b) t option

end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig

  module Resolved : sig

    type kind = Kind.reference

    type ('a, 'b) t =
      | Identifier : ('a, 'b) Identifier.t -> ('a, 'b) t
      | SubstAlias : 'a Path.Resolved.module_ * 'a module_ -> ('a, [< kind > `Module ]) t
      | Module : 'a signature * string -> ('a, [< kind > `Module]) t
      | Canonical : 'a module_ * 'a Reference.module_ -> ('a, [< kind > `Module]) t
      | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
      | Type : 'a signature * string -> ('a, [< kind > `Type]) t
      | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
      | Field : 'a parent * string -> ('a, [< kind > `Field]) t
      | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
      | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
      | Value : 'a signature * string -> ('a, [< kind > `Value]) t
      | Class : 'a signature * string -> ('a, [< kind > `Class]) t
      | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
      | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
      | InstanceVariable : 'a class_signature * string ->
                             ('a, [< kind > `InstanceVariable]) t
      | Label : 'a label_parent * string -> ('a, [< kind > `Label]) t

    and 'a any = ('a, kind) t
    and 'a signature = ('a, Kind.signature) t
    and 'a class_signature = ('a, Kind.class_signature) t
    and 'a datatype = ('a, Kind.datatype) t
    and 'a parent = ('a, Kind.parent) t
    and 'a module_ = ('a, reference_module) t
    and 'a label_parent = ('a, Kind.label_parent) t

    type 'a module_type = ('a, reference_module_type) t
    type 'a type_ = ('a, reference_type) t
    type 'a constructor = ('a, reference_constructor) t
    type 'a field = ('a, reference_field) t
    type 'a extension = ('a, reference_extension) t
    type 'a exception_ = ('a, reference_exception) t
    type 'a value = ('a, reference_value) t
    type 'a class_ = ('a, reference_class) t
    type 'a class_type = ('a, reference_class_type) t
    type 'a method_ = ('a, reference_method) t
    type 'a instance_variable = ('a, reference_instance_variable) t
    type 'a label = ('a, reference_label) t
    type 'a page = ('a, reference_page) t

    (** {2 Creators} *)

    val ident_module : 'a Identifier.module_ -> ('a, [< kind > `Module]) t

    val ident_module_type : 'a Identifier.module_type ->
          ('a, [< kind > `ModuleType]) t

    val ident_type : 'a Identifier.type_ -> ('a, [< kind > `Type])t

    val ident_constructor : 'a Identifier.constructor ->
          ('a, [< kind > `Constructor])t

    val ident_field : 'a Identifier.field -> ('a, [< kind > `Field])t

    val ident_extension : 'a Identifier.extension ->
          ('a, [< kind > `Extension])t

    val ident_exception : 'a Identifier.exception_ ->
          ('a, [< kind > `Exception])t

    val ident_value : 'a Identifier.value -> ('a, [< kind > `Value])t

    val ident_class : 'a Identifier.class_ -> ('a, [< kind > `Class])t

    val ident_class_type : 'a Identifier.class_type ->
          ('a, [< kind > `ClassType])t

    val ident_method : 'a Identifier.method_ -> ('a, [< kind > `Method])t

    val ident_instance_variable : 'a Identifier.instance_variable ->
          ('a, [< kind > `InstanceVariable])t

    val ident_label : 'a Identifier.label -> ('a, [< kind > `Label]) t

    val ident_page : 'a Identifier.page -> ('a, [< kind > `Page]) t

    (** {2 Explicit coercions} *)

    val signature_of_module : 'a module_ -> 'a signature

    val signature_of_module_type : 'a module_type -> 'a signature

    val class_signature_of_class : 'a class_ -> 'a class_signature

    val class_signature_of_class_type : 'a class_type -> 'a class_signature

    val parent_of_signature : 'a signature -> 'a parent

    val parent_of_class_signature : 'a class_signature -> 'a parent

    val parent_of_datatype : 'a datatype -> 'a parent

    val label_parent_of_parent : 'a parent -> 'a label_parent

    val label_parent_of_page : 'a page -> 'a label_parent

    val any : ('a, 'b) t -> 'a any

    (** {2 Generic operations} *)

    val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

    val hash : hash:('a -> int) -> ('a, 'b) t -> int

    val identifier: ('a, 'b) t -> ('a, 'b) Identifier.t
    (** [identifier rr] extracts the identifier present at the "root" of [rr]. *)

    val rebase : 'a Identifier.signature -> ('a, 'b) t -> ('a, 'b) t

    val sexp_of_t : ('a -> sexp) -> ('a, _) t -> sexp

  end

  type kind = Kind.reference

  type _ tag =
    | TUnknown : [< kind ] tag
    | TModule : [< kind > `Module ] tag
    | TModuleType : [< kind > `ModuleType ] tag
    | TType : [< kind > `Type ] tag
    | TConstructor : [< kind > `Constructor ] tag
    | TField : [< kind > `Field ] tag
    | TExtension : [< kind > `Extension ] tag
    | TException : [< kind > `Exception ] tag
    | TValue : [< kind > `Value ] tag
    | TClass : [< kind > `Class ] tag
    | TClassType : [< kind > `ClassType ] tag
    | TMethod : [< kind > `Method ] tag
    | TInstanceVariable : [< kind > `InstanceVariable ] tag
    | TLabel : [< kind > `Label ] tag
    | TPage : [< kind > `Page ] tag

  type ('a, 'b) t =
    | Resolved : ('a, 'b) Resolved.t -> ('a, 'b) t
    | Root : string * 'b tag -> ('a, 'b) t
    | Dot : 'a label_parent * string -> ('a, [< kind ] as 'b) t
    | Module : 'a signature * string -> ('a, [< kind > `Module]) t
    | ModuleType : 'a signature * string -> ('a, [< kind > `ModuleType]) t
    | Type : 'a signature * string -> ('a, [< kind > `Type]) t
    | Constructor : 'a datatype * string -> ('a, [< kind > `Constructor]) t
    | Field : 'a parent * string -> ('a, [< kind > `Field]) t
    | Extension : 'a signature * string -> ('a, [< kind > `Extension]) t
    | Exception : 'a signature * string -> ('a, [< kind > `Exception]) t
    | Value : 'a signature * string -> ('a, [< kind > `Value]) t
    | Class : 'a signature * string -> ('a, [< kind > `Class]) t
    | ClassType : 'a signature * string -> ('a, [< kind > `ClassType]) t
    | Method : 'a class_signature * string -> ('a, [< kind > `Method]) t
    | InstanceVariable : 'a class_signature * string ->
      ('a, [< kind > `InstanceVariable]) t
    | Label : 'a label_parent * string -> ('a, [< kind > `Label]) t

  and 'a any = ('a, kind) t
  and 'a signature = ('a, Kind.signature) t
  and 'a class_signature = ('a, Kind.class_signature) t
  and 'a datatype = ('a, Kind.datatype) t
  and 'a parent = ('a, Kind.parent) t
  and 'a label_parent = ('a, [ Kind.parent | Kind.page ]) t

  type 'a module_ = ('a, reference_module) t
  type 'a module_type = ('a, reference_module_type) t
  type 'a type_ = ('a, reference_type) t
  type 'a constructor = ('a, reference_constructor) t
  type 'a field = ('a, reference_field) t
  type 'a extension = ('a, reference_extension) t
  type 'a exception_ = ('a, reference_exception) t
  type 'a value = ('a, reference_value) t
  type 'a class_ = ('a, reference_class) t
  type 'a class_type = ('a, reference_class_type) t
  type 'a method_ = ('a, reference_method) t
  type 'a instance_variable = ('a, reference_instance_variable) t
  type 'a label = ('a, reference_label) t
  type 'a page = ('a, reference_page) t

  (** {2 Creators} *)

  val ident_module : 'a Identifier.module_ -> ('a, [< kind > `Module]) t

  val ident_module_type : 'a Identifier.module_type ->
        ('a, [< kind > `ModuleType]) t

  val ident_type : 'a Identifier.type_ -> ('a, [< kind > `Type])t

  val ident_constructor : 'a Identifier.constructor ->
        ('a, [< kind > `Constructor])t

  val ident_field : 'a Identifier.field -> ('a, [< kind > `Field])t

  val ident_extension : 'a Identifier.extension ->
        ('a, [< kind > `Extension])t

  val ident_exception : 'a Identifier.exception_ ->
        ('a, [< kind > `Exception])t

  val ident_value : 'a Identifier.value -> ('a, [< kind > `Value])t

  val ident_class : 'a Identifier.class_ -> ('a, [< kind > `Class])t

  val ident_class_type : 'a Identifier.class_type ->
        ('a, [< kind > `ClassType])t

  val ident_method : 'a Identifier.method_ -> ('a, [< kind > `Method])t

  val ident_instance_variable : 'a Identifier.instance_variable ->
        ('a, [< kind > `InstanceVariable])t

  val ident_label : 'a Identifier.label -> ('a, [< kind > `Label]) t

  val module_ : 'a signature -> string -> ('a, [< kind > `Module]) t

  val module_type : 'a signature -> string ->
        ('a, [< kind > `ModuleType]) t

  val type_ : 'a signature -> string -> ('a, [< kind > `Type])t

  val constructor : 'a datatype -> string ->
        ('a, [< kind > `Constructor])t

  val field : 'a parent -> string -> ('a, [< kind > `Field])t

  val extension : 'a signature -> string ->
        ('a, [< kind > `Extension])t

  val exception_ : 'a signature -> string ->
        ('a, [< kind > `Exception])t

  val value : 'a signature -> string -> ('a, [< kind > `Value])t

  val class_ : 'a signature -> string -> ('a, [< kind > `Class])t

  val class_type : 'a signature -> string ->
        ('a, [< kind > `ClassType])t

  val method_ : 'a class_signature -> string -> ('a, [< kind > `Method])t

  val instance_variable : 'a class_signature -> string ->
        ('a, [< kind > `InstanceVariable])t

  val label : 'a label_parent -> string -> ('a, [< kind > `Label]) t

  (** {2 Explicit coercions} *)

  val signature_of_module : 'a module_ -> 'a signature

  val signature_of_module_type : 'a module_type -> 'a signature

  val class_signature_of_class : 'a class_ -> 'a class_signature

  val class_signature_of_class_type : 'a class_type -> 'a class_signature

  val parent_of_signature : 'a signature -> 'a parent

  val parent_of_class_signature : 'a class_signature -> 'a parent

  val parent_of_datatype : 'a datatype -> 'a parent

  val label_parent_of_parent : 'a parent -> 'a label_parent

  val any : ('a, 'b) t -> 'a any

  (** {2 Generic operations} *)

  val equal : equal:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val hash : hash:('a -> int) -> ('a, 'b) t -> int

  val sexp_of_t : ('a -> sexp) -> ('a, _) t -> sexp
end

module Defn : sig

  module Index : sig

    type t

    type allocator

    val allocator : unit -> allocator

    val allocate : allocator -> t

    val to_string : t -> string

  end

  type 'a t =
    { root: 'a;
      name: string;
      index : Index.t; }

end

module Decl : sig

  module Index : sig

    type t

    type allocator

    val allocator : unit -> allocator

    val allocate : allocator -> t

    val to_string : t -> string

  end

  type 'a t =
    { root: 'a;
      name: string;
      index : Index.t; }

end
