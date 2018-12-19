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

(** Type of documentation *)

open DocOckPaths

module Documentation = DocOckDocumentation
module Source = DocOckSource

(** {3 Modules} *)

module rec Module : sig

  type 'a expansion =
    | Not_yet_expanded of 'a Source.Decl_map.Signature.t
    | AlreadyASig
    | Signature of 'a Signature.t
    | Functor of 'a FunctorArgument.t option list * 'a Signature.t

  type 'a expr =
    | Alias of 'a Path.module_
    | ModuleType of 'a ModuleType.expr

  type 'a t =
    { id: 'a Identifier.module_;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      expr: 'a expr;
      canonical : ('a Path.module_ * 'a Reference.module_) option;
      hidden : bool;
      display_expr : 'a expr option;
      expansion: 'a expansion;
    }

  module Equation : sig

    type 'a t = 'a expr

  end

end = Module

and FunctorArgument : sig
  type 'a t = {
    id : 'a Identifier.module_;
    expr : 'a ModuleType.expr;
    expansion: 'a Module.expansion;
  }
end = FunctorArgument

(** {3 Modules Types} *)

and ModuleType : sig

  type 'a substitution =
    | ModuleEq of 'a Fragment.module_ * 'a Module.Equation.t
    | TypeEq of 'a Fragment.type_ * 'a TypeDecl.Equation.t
    | ModuleSubst of 'a Fragment.module_ * 'a Path.module_
    | TypeSubst of 'a Fragment.type_ * 'a TypeDecl.Equation.t

  type 'a expr =
    | Path of 'a Path.module_type
    | Signature of 'a Signature.t
    | Functor of 'a FunctorArgument.t option * 'a expr
    | With of 'a expr * 'a substitution list
    | TypeOf of 'a Module.expr

  type 'a t =
    { id: 'a Identifier.module_type;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      expr: 'a expr option;
      expansion: 'a Module.expansion;
    }

end = ModuleType

(** {3 Signatures} *)

and Signature : sig

  type 'a item =
    | Module of 'a Module.t
    | ModuleType of 'a ModuleType.t
    | Type of 'a TypeDecl.t
    | TypExt of 'a Extension.t
    | Exception of 'a Exception.t
    | Value of 'a Value.t
    | External of 'a External.t
    | Class of 'a Class.t
    | ClassType of 'a ClassType.t
    | Include of 'a Include.t
    | Comment of 'a Documentation.comment

  type 'a t = 'a item list

end = Signature

(** {3 Includes} *)

and Include : sig
  type 'a expansion = {
    resolved: bool;
    content: 'a Signature.t;
    decl_map: 'a Source.Decl_map.Signature.t;
  }

  type 'a t =
    { parent: 'a Identifier.signature;
      doc: 'a Documentation.t;
      expr: 'a Module.expr;
      expansion: 'a expansion; }

end = Include

(** {3 Type Declarations} *)

and TypeDecl : sig

  module Field : sig

    type 'a t =
      { id: 'a Identifier.field;
        doc: 'a Documentation.t;
        decl: 'a Decl.t option;
        defn: 'a Defn.t option;
        mutable_ : bool;
        type_: 'a TypeExpr.t; }

  end

  module Constructor : sig
    type 'a argument =
      | Tuple  of 'a TypeExpr.t list
      | Record of 'a Field.t list

    type 'a t =
      { id: 'a Identifier.constructor;
        doc: 'a Documentation.t;
        decl: 'a Decl.t option;
        defn: 'a Defn.t option;
        args: 'a argument;
        res: 'a TypeExpr.t option; }

  end


  module Representation : sig

    type 'a t =
      | Variant of 'a Constructor.t list
      | Record of 'a Field.t list
      | Extensible

  end

  type variance =
    | Pos
    | Neg

  type param_desc =
    | Any
    | Var of string

  type param = param_desc * variance option

  module Equation : sig

    type 'a t =
      { params: param list;
        private_: bool;
        manifest: 'a TypeExpr.t option;
        constraints: ('a TypeExpr.t * 'a TypeExpr.t) list; }

  end

  type 'a t =
    { id: 'a Identifier.type_;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      equation: 'a Equation.t;
      representation: 'a Representation.t option; }

end = TypeDecl

(** {3 Type extensions} *)

and Extension : sig

  module Constructor : sig

    type 'a t =
      { id: 'a Identifier.extension;
        doc: 'a Documentation.t;
        decl: 'a Decl.t option;
        defn: 'a Defn.t option;
        args: 'a TypeDecl.Constructor.argument;
        res: 'a TypeExpr.t option; }

  end

  type 'a t =
    { type_path: 'a Path.type_;
      doc: 'a Documentation.t;
      type_params: TypeDecl.param list;
      private_: bool;
      constructors: 'a Constructor.t list; }

end = Extension

(** {3 Exception} *)
and Exception : sig

  type 'a t =
    { id: 'a Identifier.exception_;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      args: 'a TypeDecl.Constructor.argument;
      res: 'a TypeExpr.t option; }

end = Exception


(** {3 Values} *)

and Value : sig

  type 'a t =
    { id: 'a Identifier.value;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      type_: 'a TypeExpr.t; }

end = Value

(** {3 External values} *)

and External : sig

  type 'a t =
    { id: 'a Identifier.value;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      type_: 'a TypeExpr.t;
      primitives: string list; }

end = External

(** {3 Classes} *)

and Class : sig

  type 'a expansion =
    | Not_yet_expanded of 'a Source.Decl_map.Class_signature.t
    | AlreadyASig
    | Signature of 'a ClassSignature.t

  type 'a expr =
    | ClassType of 'a ClassType.expr
    | Arrow of TypeExpr.label option * 'a TypeExpr.t * 'a expr

  type 'a t =
    { id: 'a Identifier.class_;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      virtual_: bool;
      params: TypeDecl.param list;
      expr: 'a expr;
      expansion: 'a expansion; }

end = Class

(** {3 Class Types} *)

and ClassType : sig

  type 'a expr =
    | Constr of 'a Path.class_type * 'a TypeExpr.t list
    | Signature of 'a ClassSignature.t

  type 'a t =
    { id: 'a Identifier.class_type;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      virtual_: bool;
      params: TypeDecl.param list;
      expr: 'a expr;
      expansion: 'a Class.expansion; }

end = ClassType

(** {3 Class Signatures} *)

and ClassSignature : sig

  type 'a item =
    | Method of 'a Method.t
    | InstanceVariable of 'a InstanceVariable.t
    | Constraint of 'a TypeExpr.t * 'a TypeExpr.t
    | Inherit of 'a ClassType.expr
    | Comment of 'a Documentation.comment

  type 'a t =
    { self: 'a TypeExpr.t option;
      items: 'a item list; }

end = ClassSignature

(** {3 Methods} *)

and Method : sig

  type 'a t =
    { id: 'a Identifier.method_;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      private_: bool;
      virtual_: bool;
      type_: 'a TypeExpr.t; }

end = Method

(** {3 Instance variables} *)

and InstanceVariable : sig

  type 'a t =
    { id: 'a Identifier.instance_variable;
      doc: 'a Documentation.t;
      decl: 'a Decl.t option;
      defn: 'a Defn.t option;
      mutable_: bool;
      virtual_: bool;
      type_: 'a TypeExpr.t; }

end = InstanceVariable

(** {3 Type expressions} *)

and TypeExpr : sig

  module Variant : sig

    type kind =
      | Fixed
      | Closed of string list
      | Open

    type 'a element =
      | Type of 'a TypeExpr.t
      | Constructor of string * bool * 'a TypeExpr.t list

    type 'a t =
      { kind: kind;
        elements: 'a element list;}

  end

  module Object : sig

    type 'a method_ =
      { name: string;
        type_: 'a TypeExpr.t; }

    type 'a field =
      | Method of 'a method_
      | Inherit of 'a TypeExpr.t

    type 'a t =
      { fields: 'a field list;
        open_ : bool; }

  end

  module Package : sig

    type 'a substitution = 'a Fragment.type_ * 'a TypeExpr.t

    type 'a t =
      { path: 'a Path.module_type;
        substitutions: 'a substitution list; }

  end

  type label =
    | Label of string
    | Optional of string

  type 'a t =
    | Var of string
    | Any
    | Alias of 'a t * string
    | Arrow of label option * 'a t * 'a t
    | Tuple of 'a t list
    | Constr of 'a Path.type_ * 'a t list
    | Variant of 'a TypeExpr.Variant.t
    | Object of 'a TypeExpr.Object.t
    | Class of 'a Path.class_type * 'a t list
    | Poly of string list * 'a t
    | Package of 'a TypeExpr.Package.t

end = TypeExpr

(** {3 Compilation units} *)

module rec Unit : sig

  module Import : sig

    type 'a t =
      | Unresolved of string * Digest.t option
      | Resolved of 'a

  end

  module Packed : sig

    type 'a item =
      { id: 'a Identifier.module_;
        path: 'a Path.module_; }

    type 'a t = 'a item list

  end

  type 'a content =
    | Module of 'a Signature.t
    | Pack of 'a Packed.t

  type 'a expansion =
    | Not_expanded_yet
    | AlreadyASig
    | Signature of 'a Signature.t

  type 'a t =
    { id: 'a Identifier.module_;
      doc: 'a Documentation.t;
      digest: Digest.t;
      imports: 'a Import.t list;
      source : 'a Source.t;
      interface: bool;
      hidden: bool;
      content: 'a content;
      expansion: 'a expansion;
    }

end = Unit

module rec Page : sig

  type 'a t =
    { name: 'a Identifier.page;
      content: 'a Documentation.t;
      digest: Digest.t; }

end = Page
