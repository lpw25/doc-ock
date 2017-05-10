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

open DocOckNames

(** {1 Paths} *)

(** Identifiers for definitions *)
module Identifier : sig

  module rec Module : sig

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Signature.t * ModuleName.t
      | FunctorParameter of 'a Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Signature.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and ModuleType : sig

    type 'a t =
      | ModuleType of 'a Signature.t * ModuleTypeName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Type :sig

    type 'a t =
      | Type of 'a Signature.t * TypeName.t
      | CoreType of TypeName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Constructor : sig

    type 'a t =
      | Constructor of 'a Type.t * ConstructorName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Field : sig

    type 'a t =
      | Field of 'a FieldParent.t * FieldName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Extension : sig

    type 'a t =
      | Extension of 'a Signature.t * ExtensionName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Exception : sig

    type 'a t =
      | Exception of 'a Signature.t * ExceptionName.t
      | CoreException of ExceptionName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Value : sig

    type 'a t =
      | Value of 'a Signature.t * ValueName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Class : sig

    type 'a t =
      | Class of 'a Signature.t * ClassName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and ClassType : sig

    type 'a t =
      | ClassType of 'a Signature.t * ClassTypeName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Method : sig

    type 'a t =
      | Method of 'a ClassSignature.t * MethodName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and InstanceVariable : sig

    type 'a t =
      | InstanceVariable of 'a ClassSignature.t * InstanceVariableName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Label : sig

    type 'a t =
      | Label of 'a LabelParent.t * LabelName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Page : sig

    type 'a t =
      | Page of 'a * PageName.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Signature : sig

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Signature.t * ModuleName.t
      | FunctorParameter of 'a Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Signature.t
      | ModuleType of 'a Signature.t * ModuleTypeName.t

    val of_module : 'a Module.t -> 'a t

    val of_module_type : 'a ModuleType.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and ClassSignature : sig

    type 'a t =
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t

    val of_class : 'a Class.t -> 'a t

    val of_class_type : 'a ClassType.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and FieldParent : sig

    type 'a t =
      | Type of 'a Signature.t * TypeName.t
      | CoreType of TypeName.t
      | Constructor of 'a Type.t * ConstructorName.t
      | Extension of 'a Signature.t * ExtensionName.t
      | Exception of 'a Signature.t * ExceptionName.t
      | CoreException of ExceptionName.t

    val of_type : 'a Type.t -> 'a t

    val of_constructor : 'a Constructor.t -> 'a t

    val of_extension : 'a Extension.t -> 'a t

    val of_exception : 'a Exception.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and LabelParent : sig

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Signature.t * ModuleName.t
      | FunctorParameter of 'a Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Signature.t
      | ModuleType of 'a Signature.t * ModuleTypeName.t
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t
      | Page of 'a * PageName.t

    val of_module : 'a Module.t -> 'a t

    val of_module_type : 'a ModuleType.t -> 'a t

    val of_class : 'a Class.t -> 'a t

    val of_class_type : 'a ClassType.t -> 'a t

    val of_page : 'a Page.t -> 'a t

    val of_signature : 'a Signature.t -> 'a t

    val of_class_signature : 'a ClassSignature.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  type 'a t =
    | Root of 'a * UnitName.t
    | Module of 'a Signature.t * ModuleName.t
    | FunctorParameter of 'a Signature.t * FunctorParameterName.t
    | FunctorResult of 'a Signature.t
    | ModuleType of 'a Signature.t * ModuleTypeName.t
    | Type of 'a Signature.t * TypeName.t
    | CoreType of TypeName.t
    | Constructor of 'a Type.t * ConstructorName.t
    | Field of 'a FieldParent.t * FieldName.t
    | Extension of 'a Signature.t * ExtensionName.t
    | Exception of 'a Signature.t * ExceptionName.t
    | CoreException of ExceptionName.t
    | Value of 'a Signature.t * ValueName.t
    | Class of 'a Signature.t * ClassName.t
    | ClassType of 'a Signature.t * ClassTypeName.t
    | Method of 'a ClassSignature.t * MethodName.t
    | InstanceVariable of 'a ClassSignature.t * InstanceVariableName.t
    | Label of 'a LabelParent.t * LabelName.t
    | Page of 'a * PageName.t

  val of_module : 'a Module.t -> 'a t

  val of_module_type : 'a ModuleType.t -> 'a t

  val of_type : 'a Type.t -> 'a t

  val of_constructor : 'a Constructor.t -> 'a t

  val of_field : 'a Field.t -> 'a t

  val of_extension : 'a Extension.t -> 'a t

  val of_exception : 'a Exception.t -> 'a t

  val of_value : 'a Value.t -> 'a t

  val of_class : 'a Class.t -> 'a t

  val of_class_type : 'a ClassType.t -> 'a t

  val of_method : 'a Method.t -> 'a t

  val of_instance_variable : 'a InstanceVariable.t -> 'a t

  val of_label : 'a Label.t -> 'a t

  val of_page : 'a Page.t -> 'a t

  val of_signature : 'a Signature.t -> 'a t

  val of_class_signature : 'a ClassSignature.t -> 'a t

  val of_field_parent : 'a FieldParent.t -> 'a t

  val of_label_parent : 'a LabelParent.t -> 'a t

  val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val hash : hash:('a -> int) -> 'a t -> int

  (** Identifiers for paths.

      There are four kinds of OCaml path:

        - module
        - module type
        - type
        - class type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path may refer to a class definition). *)
  module Path : sig

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Signature.t * ModuleName.t
      | FunctorParameter of 'a Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Signature.t
      | ModuleType of 'a Signature.t * ModuleTypeName.t
      | Type of 'a Signature.t * TypeName.t
      | CoreType of TypeName.t
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t

    val of_module : 'a Module.t -> 'a t

    val of_module_type : 'a ModuleType.t -> 'a t

    val of_type : 'a Type.t -> 'a t

    val of_class_type : 'a ClassType.t -> 'a t

    module Module : sig

      type 'a t =
        | Root of 'a * UnitName.t
        | Module of 'a Signature.t * ModuleName.t
        | FunctorParameter of 'a Signature.t * FunctorParameterName.t
        | FunctorResult of 'a Signature.t

      val of_module : 'a Module.t -> 'a t

    end

    module ModuleType : sig

      type 'a t =
        | ModuleType of 'a Signature.t * ModuleTypeName.t

      val of_module_type : 'a ModuleType.t -> 'a t

    end

    module Type : sig

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | CoreType of TypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      val of_type : 'a Type.t -> 'a t

      val of_class : 'a Class.t -> 'a t

      val of_class_type : 'a ClassType.t -> 'a t

    end

    module ClassType : sig

      type 'a t =
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      val of_class : 'a Class.t -> 'a t

      val of_class_type : 'a ClassType.t -> 'a t

    end

    val of_path_module : 'a Module.t -> 'a t

    val of_path_module_type : 'a ModuleType.t -> 'a t

    val of_path_type : 'a Type.t -> 'a t

    val of_path_class_type : 'a ClassType.t -> 'a t

  end

  val of_path_module : 'a Path.Module.t -> 'a t

  val of_path_module_type : 'a Path.ModuleType.t -> 'a t

  val of_path_type : 'a Path.Type.t -> 'a t

  val of_path : 'a Path.t -> 'a t

end

(** Simple OCaml (module) paths -- no functor applications. *)
module rec Simple : sig

  module Resolved : sig

    module Module : sig

      type 'a t =
        | Identifier of 'a Identifier.Module.t
        | Module of 'a t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Module.t

    end

  end

  module Module : sig

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Root of string
      | Forward of string
      | Dot of 'a t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    module rec Module : sig

      type 'a t =
        | Identifier of 'a Identifier.Path.Module.t
          (** Module identifier *)
        | Module of 'a t * ModuleName.t
          (** Module projection *)
        | Apply of 'a t * 'a Path.Module.t
          (** Functor application. The argument may not be resolved. *)
        | Subst of 'a ModuleType.t * 'a t
          (** Substitution of a module type from a functor argument.  The right-hand-side
              is the original path, which should always be used for printing. When
              locating the identifier for this module the right-hand-side should still be
              used, however when locating the identifier for a projection from this module
              the left-hand-side must be used instead. *)
        | SubstAlias of 'a t * 'a t
          (** Substitution of a module from a functor argument.  The right-hand-side is
              the original path, which should always be used for printing. When locating
              the identifier for this module the right-hand-side should still be used,
              however when locating the identifier for a projection from this module the
              left-hand-side must be used instead. *)
        | Alias of 'a Simple.Module.t * 'a t
          (** Substitution of a module alias.  The right-hand-side is the original path,
              which should be used for printing, whilst the left-hand-side is the module
              type path that should be used when locating identifiers. However, if the
              left-hand-side cannot be resolved or is hidden the right-hand-side should
              be used in its place. *)
        | Hidden of 'a t
          (** A hidden path. *)
        | Canonical of 'a Simple.Module.t * 'a t
          (** Replacement by a canonical module path. The right-hand-side is the original
              path, and the left-hand-side is a reference to a module that should replace
              it. However, if the reference cannot be resolved the right-hand-side should
              be used instead. Note that if the canonical reference is to the current
              location (e.g. the definition of [Foo] is an alias whose canonical reference
              is to [Foo]) then this path should be treated as hidden. *)

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Path.Module.t

    end

    and ModuleType : sig

      type 'a t =
        | Identifier of 'a Identifier.Path.ModuleType.t
        | ModuleType of 'a Module.t * ModuleTypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Path.ModuleType.t

    end

    and Type : sig

      type 'a t =
        | Identifier of 'a Identifier.Path.Type.t
        | Type of 'a Module.t * TypeName.t
        | Class of 'a Module.t * ClassName.t
        | ClassType of 'a Module.t * ClassTypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Path.Type.t

    end

    and ClassType : sig

      type 'a t =
        | Identifier of 'a Identifier.Path.ClassType.t
        | Class of 'a Module.t * ClassName.t
        | ClassType of 'a Module.t * ClassTypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Path.ClassType.t

    end

    type 'a t =
      | Identifier of 'a Identifier.Path.t
      | Module of 'a Module.t * ModuleName.t
      | Apply of 'a Module.t * 'a Path.Module.t
      | Subst of 'a ModuleType.t * 'a Module.t
      | SubstAlias of 'a Module.t * 'a Module.t
      | Alias of 'a Simple.Module.t * 'a Module.t
      | Hidden of 'a Module.t
      | Canonical of 'a Simple.Module.t * 'a Module.t
      | ModuleType of 'a Module.t * ModuleTypeName.t
      | Type of 'a Module.t * TypeName.t
      | Class of 'a Module.t * ClassName.t
      | ClassType of 'a Module.t * ClassTypeName.t

    val of_module : 'a Module.t -> 'a t

    val of_module_type : 'a ModuleType.t -> 'a t

    val of_type : 'a Type.t -> 'a t

    val of_class_type : 'a ClassType.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

    val identifier : 'a t -> 'a Identifier.Path.t

  end

  module rec Module : sig

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Root of string
      | Forward of string
      | Dot of 'a t * string
      | Apply of 'a t * 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and ModuleType : sig

    type 'a t =
      | Resolved of 'a Resolved.ModuleType.t
      | Dot of 'a Module.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Type : sig

    type 'a t =
      | Resolved of 'a Resolved.Type.t
      | Dot of 'a Module.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and ClassType : sig

    type 'a t =
      | Resolved of 'a Resolved.ClassType.t
      | Dot of 'a Module.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  type 'a t =
    | Resolved of 'a Resolved.t
    | Root of string
    | Forward of string
    | Dot of 'a Module.t * string
    | Apply of 'a Module.t * 'a Module.t

    val of_module : 'a Module.t -> 'a t

    val of_module_type : 'a ModuleType.t -> 'a t

    val of_type : 'a Type.t -> 'a t

    val of_class_type : 'a ClassType.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

end


(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    module rec Module : sig

      type 'a t =
        | Module of 'a Signature.t * ModuleName.t
        | Subst of 'a Path.Resolved.ModuleType.t * 'a t
        | SubstAlias of 'a Path.Resolved.Module.t * 'a t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier :
        'a Identifier.Signature.t -> 'a t -> 'a Identifier.Path.Module.t

    end

    and Type : sig

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier :
        'a Identifier.Signature.t -> 'a t -> 'a Identifier.Path.Type.t

    end

    and Signature : sig

      type 'a t =
        | Base
        | Module of 'a t * ModuleName.t
        | Subst of 'a Path.Resolved.ModuleType.t * 'a Module.t
        | SubstAlias of 'a Path.Resolved.Module.t * 'a Module.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t

      val of_module : 'a Module.t -> 'a t

      val identifier :
        'a Identifier.Signature.t -> 'a t -> 'a Identifier.Signature.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

    end

    type 'a t =
      | Base
      | Module of 'a Signature.t * ModuleName.t
      | Subst of 'a Path.Resolved.ModuleType.t * 'a Module.t
      | SubstAlias of 'a Path.Resolved.Module.t * 'a Module.t
      | Alias of 'a Simple.Module.t * 'a Module.t
      | Hidden of 'a Module.t
      | Type of 'a Signature.t * TypeName.t
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t

    val of_module : 'a Module.t -> 'a t

    val of_type : 'a Type.t -> 'a t

    val of_signature : 'a Signature.t -> 'a t

    val identifier :
      'a Identifier.Signature.t -> 'a t -> 'a Identifier.t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  module rec Module : sig

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Dot of 'a Signature.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Type : sig

    type 'a t =
      | Resolved of 'a Resolved.Type.t
      | Dot of 'a Signature.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  and Signature : sig

    type 'a t =
      | Resolved of 'a Resolved.Signature.t
      | Dot of 'a t * string

    val of_module : 'a Module.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end

  type 'a t =
    | Resolved of 'a Resolved.t
    | Dot of 'a Signature.t * string

  val of_module : 'a Module.t -> 'a t

  val of_type : 'a Type.t -> 'a t

  val of_signature : 'a Signature.t -> 'a t

  val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val hash : hash:('a -> int) -> 'a t -> int

end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module Reference : sig

  module Resolved : sig

    module rec Module : sig

      type 'a t =
        | Identifier of 'a Identifier.Module.t
        | Module of 'a Signature.t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Module.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    and Type : sig

      type 'a t =
        | Identifier of 'a Identifier.Type.t
        | Type of 'a Signature.t * TypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Type.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    and Signature : sig

      type 'a t =
        | Identifier of 'a Identifier.Signature.t
        | Module of 'a t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t
        | Canonical of 'a Simple.Module.t * 'a Module.t
        | ModuleType of 'a t * ModuleTypeName.t

      val of_module : 'a Module.t -> 'a t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.Signature.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    and ClassSignature : sig

      type 'a t =
        | Identifier of 'a Identifier.ClassSignature.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.ClassSignature.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    and FieldParent : sig

      type 'a t =
        | Identifier of 'a Identifier.FieldParent.t
        | Type of 'a Signature.t * TypeName.t
        | Constructor of 'a Type.t * ConstructorName.t
        | Extension of 'a Signature.t * ExtensionName.t
        | Exception of 'a Signature.t * ExceptionName.t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.FieldParent.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    and LabelParent : sig

      type 'a t =
        | Identifier of 'a Identifier.LabelParent.t
        | Module of 'a Signature.t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t
        | Canonical of 'a Simple.Module.t * 'a Module.t
        | ModuleType of 'a Signature.t * ModuleTypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      val of_signature : 'a Signature.t -> 'a t

      val of_class_signature : 'a ClassSignature.t -> 'a t

      val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      val hash : hash:('a -> int) -> 'a t -> int

      val identifier : 'a t -> 'a Identifier.LabelParent.t

      val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

    end

    type 'a t =
      | Identifier of 'a Identifier.t
      | Module of 'a Signature.t * ModuleName.t
      | Alias of 'a Simple.Module.t * 'a Module.t
      | Hidden of 'a Module.t
      | Canonical of 'a Simple.Module.t * 'a Module.t
      | ModuleType of 'a Signature.t * ModuleTypeName.t
      | Type of 'a Signature.t * TypeName.t
      | Constructor of 'a Type.t * ConstructorName.t
      | Field of 'a FieldParent.t * FieldName.t
      | Extension of 'a Signature.t * ExtensionName.t
      | Exception of 'a Signature.t * ExceptionName.t
      | Value of 'a Signature.t * ValueName.t
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t
      | Method of 'a ClassSignature.t * MethodName.t
      | InstanceVariable of 'a ClassSignature.t * InstanceVariableName.t
      | Label of 'a LabelParent.t * LabelName.t

    val of_module : 'a Module.t -> 'a t

    val of_type : 'a Type.t -> 'a t

    val of_signature : 'a Signature.t -> 'a t

    val of_class_signature : 'a ClassSignature.t -> 'a t

    val of_field_parent : 'a FieldParent.t -> 'a t

    val of_label_parent : 'a LabelParent.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

    val identifier : 'a t -> 'a Identifier.t

    val rebase : 'a Identifier.Signature.t -> 'a t -> 'a t

  end

  type 'a t =
    | Resolved of 'a Resolved.t
    | Module of 'a t option * string
    | ModuleType of 'a t option * string
    | Type of 'a t option * string
    | Constructor of 'a t option * string
    | Field of 'a t option * string
    | Extension of 'a t option * string
    | Exception of 'a t option * string
    | Value of 'a t option * string
    | Class of 'a t option * string
    | ClassType of 'a t option * string
    | Method of 'a t option * string
    | InstanceVariable of 'a t option * string
    | Label of 'a t option * string
    | Page of 'a t option * string
    | Unknown of 'a t option * string

  val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val hash : hash:('a -> int) -> 'a t -> int

end
