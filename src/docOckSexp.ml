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

let rec pp ppf = function
  | Atom s ->
      Format.fprintf ppf "%s" s
  | List lst ->
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp) lst

type sexp = t

module type Sexpable0 = sig

  type t

  val to_sexp : t -> sexp

end

module type Sexpable1 = sig

  type 'a t

  val to_sexp : ('a -> sexp) -> 'a t -> sexp

end

module String = struct

  let to_sexp s =
    Atom (Printf.sprintf "%S" s)

end

open DocOckNames

module ModuleName = struct

  let to_sexp s =
    String.to_sexp (ModuleName.to_string s)

end

module FunctorParameterName = struct

  let to_sexp s =
    String.to_sexp (FunctorParameterName.to_string s)

end

module ModuleTypeName = struct

  let to_sexp s =
    String.to_sexp (ModuleTypeName.to_string s)

end

module TypeName = struct

  let to_sexp s =
    String.to_sexp (TypeName.to_string s)

end

module ConstructorName = struct

  let to_sexp s =
    String.to_sexp (ConstructorName.to_string s)

end

module FieldName = struct

  let to_sexp s =
    String.to_sexp (FieldName.to_string s)

end

module ExtensionName = struct

  let to_sexp s =
    String.to_sexp (ExtensionName.to_string s)

end

module ExceptionName = struct

  let to_sexp s =
    String.to_sexp (ExceptionName.to_string s)

end

module ValueName = struct

  let to_sexp s =
    String.to_sexp (ValueName.to_string s)

end

module ClassName = struct

  let to_sexp s =
    String.to_sexp (ClassName.to_string s)

end

module ClassTypeName = struct

  let to_sexp s =
    String.to_sexp (ClassTypeName.to_string s)

end

module MethodName = struct

  let to_sexp s =
    String.to_sexp (MethodName.to_string s)

end

module InstanceVariableName = struct

  let to_sexp s =
    String.to_sexp (InstanceVariableName.to_string s)

end

module UnitName = struct

  let to_sexp s =
    String.to_sexp (UnitName.to_string s)

end

module PageName = struct

  let to_sexp s =
    String.to_sexp (PageName.to_string s)

end

module LabelName = struct

  let to_sexp s =
    String.to_sexp (LabelName.to_string s)

end

open DocOckPaths

let rec identifier_to_sexp sexp_of_root t =
  let open Identifier in
  match t with
  | Root(r, s) ->
      List [ Atom "Root";
             List [sexp_of_root r; UnitName.to_sexp s] ]
  | Page(r, s) ->
      List [ Atom "Root";
             List [sexp_of_root r; PageName.to_sexp s] ]
  | Module(m, s) ->
      let m = of_signature m in
      List [ Atom "Module";
             List [identifier_to_sexp sexp_of_root m;
                   ModuleName.to_sexp s] ]
  | FunctorParameter(m, s) ->
      let m = of_signature m in
      List [ Atom "FunctorParameter";
             List [identifier_to_sexp sexp_of_root m;
                   FunctorParameterName.to_sexp s] ]
  | FunctorResult m ->
      let m = of_signature m in
      List [ Atom "FunctorResult";
             List [identifier_to_sexp sexp_of_root m] ]
  | ModuleType(m, s) ->
      let m = of_signature m in
      List [ Atom "ModuleType";
             List [identifier_to_sexp sexp_of_root m;
                   ModuleTypeName.to_sexp s] ]
  | Type(m, s) ->
      let m = of_signature m in
      List [ Atom "Type";
             List [identifier_to_sexp sexp_of_root m;
                   TypeName.to_sexp s] ]
  | CoreType s ->
      List [ Atom "CoreType"; TypeName.to_sexp s ]
  | Constructor(t, s) ->
      let t = Identifier.of_type t in
      List [ Atom "Constructor";
             List [identifier_to_sexp sexp_of_root t;
                   ConstructorName.to_sexp s] ]
  | Field(t, s) ->
      let t = Identifier.of_field_parent t in
      List [ Atom "Field";
             List [identifier_to_sexp sexp_of_root t;
                   FieldName.to_sexp s] ]
  | Extension(m, s) ->
      let m = of_signature m in
      List [ Atom "Extension";
             List [identifier_to_sexp sexp_of_root m;
                   ExtensionName.to_sexp s] ]
  | Exception(m, s) ->
      let m = of_signature m in
      List [ Atom "Exception";
             List [identifier_to_sexp sexp_of_root m;
                   ExceptionName.to_sexp s] ]
  | CoreException s ->
      List [ Atom "CoreException"; ExceptionName.to_sexp s ]
  | Value(m, s) ->
      let m = of_signature m in
      List [ Atom "Value";
             List [identifier_to_sexp sexp_of_root m;
                   ValueName.to_sexp s] ]
  | Class(m, s) ->
      let m = of_signature m in
      List [ Atom "Class";
             List [identifier_to_sexp sexp_of_root m;
                   ClassName.to_sexp s] ]
  | ClassType(m, s) ->
      let m = of_signature m in
      List [ Atom "ClassType";
             List [identifier_to_sexp sexp_of_root m;
                   ClassTypeName.to_sexp s] ]
  | Method(c, s) ->
      let c = Identifier.of_class_signature c in
      List [ Atom "Method";
             List [identifier_to_sexp sexp_of_root c;
                   MethodName.to_sexp s] ]
  | InstanceVariable(c, s) ->
      let c = Identifier.of_class_signature c in
      List [ Atom "InstanceVariable";
             List [identifier_to_sexp sexp_of_root c;
                   InstanceVariableName.to_sexp s] ]
  | Label(p, s) ->
      let p = Identifier.of_label_parent p in
      List [ Atom "Label";
             List [identifier_to_sexp sexp_of_root p;
                   LabelName.to_sexp s] ]

let rec resolved_simple_to_sexp sexp_of_root t =
  let open Simple.Resolved.Module in
  match t with
  | Identifier id ->
      let id = Identifier.of_module id in
      List [ Atom "Identifier";
             identifier_to_sexp sexp_of_root id ]
  | Alias(p, m) ->
      List [ Atom "Alias";
             List [ simple_to_sexp sexp_of_root p;
                    resolved_simple_to_sexp sexp_of_root m ]]
  | Hidden m ->
      List [ Atom "Hidden";
             List [ resolved_simple_to_sexp sexp_of_root m ]]
  | Module(m, s) ->
      List [ Atom "Module";
             List [ resolved_simple_to_sexp sexp_of_root m;
                    ModuleName.to_sexp s ]]
  | Canonical(r, m) ->
      List [ Atom "Canonical";
             List [ simple_to_sexp sexp_of_root r;
                    resolved_simple_to_sexp sexp_of_root m ]]

and simple_to_sexp sexp_of_root t =
  let open Simple.Module in
  match t with
  | Resolved r ->
      List [ Atom "Resolved";
             resolved_simple_to_sexp sexp_of_root r ]
  | Root s ->
      List [ Atom "Root"; Atom s ]
  | Forward s ->
      List [ Atom "Forward"; Atom s ]
  | Dot(m, s) ->
      List [ Atom "Dot";
             List [simple_to_sexp sexp_of_root m; Atom s]]

let rec path_to_sexp sexp_of_root t =
  let open Path in
  match t with
  | Resolved r ->
      List [ Atom "Resolved";
             resolved_path_to_sexp sexp_of_root r ]
  | Root s ->
      List [ Atom "Root"; Atom s ]
  | Forward s ->
      List [ Atom "Forward"; Atom s ]
  | Dot(m, s) ->
      let m = of_module m in
      List [ Atom "Dot";
             List [path_to_sexp sexp_of_root m; Atom s]]
  | Apply(m1, m2) ->
      let m1 = of_module m1 in
      let m2 = of_module m2 in
      List [ Atom "Apply";
             List [ path_to_sexp sexp_of_root m1;
                    path_to_sexp sexp_of_root m2 ]]

and resolved_path_to_sexp sexp_of_root t =
  let open Path.Resolved in
  match t with
  | Identifier id ->
      let id = Identifier.of_path id in
      List [ Atom "Identifier";
             identifier_to_sexp sexp_of_root id ]
  | Subst(p, m) ->
      let p = of_module_type p in
      let m = of_module m in
      List [ Atom "Subst";
             List [ resolved_path_to_sexp sexp_of_root p;
                    resolved_path_to_sexp sexp_of_root m ]]
  | SubstAlias(p, m) ->
      let p = of_module p in
      let m = of_module m in
      List [ Atom "SubstAlias";
             List [ resolved_path_to_sexp sexp_of_root p;
                    resolved_path_to_sexp sexp_of_root m ]]
  | Alias(p, m) ->
      let m = of_module m in
      List [ Atom "Alias";
             List [ simple_to_sexp sexp_of_root p;
                    resolved_path_to_sexp sexp_of_root m ]]
  | Hidden m ->
      let m = of_module m in
      List [ Atom "Hidden";
             List [ resolved_path_to_sexp sexp_of_root m ]]
  | Module(m, s) ->
      let m = of_module m in
      List [ Atom "Module";
             List [ resolved_path_to_sexp sexp_of_root m;
                    ModuleName.to_sexp s ]]
  | Canonical(p, m) ->
      let m = of_module m in
      List [ Atom "Canonical";
             List [ simple_to_sexp sexp_of_root p;
                    resolved_path_to_sexp sexp_of_root m ]]
  | Apply(m, p) ->
      let m = of_module m in
      let p = Path.of_module p in
      List [ Atom "Apply";
             List [ resolved_path_to_sexp sexp_of_root m;
                    path_to_sexp sexp_of_root p ]]
  | ModuleType(m, s) ->
      let m = of_module m in
      List [ Atom "ModuleType";
             List [ resolved_path_to_sexp sexp_of_root m;
                    ModuleTypeName.to_sexp s ]]
  | Type(m, s) ->
      let m = of_module m in
      List [ Atom "Type";
             List [ resolved_path_to_sexp sexp_of_root m;
                    TypeName.to_sexp s ]]
  | Class(m, s) ->
      let m = of_module m in
      List [ Atom "Class";
             List [ resolved_path_to_sexp sexp_of_root m;
                    ClassName.to_sexp s ]]
  | ClassType(m, s) ->
      let m = of_module m in
      List [ Atom "ClassType";
             List [ resolved_path_to_sexp sexp_of_root m;
                    ClassTypeName.to_sexp s ]]

let rec resolved_fragment_to_sexp sexp_of_root p =
  let open Fragment.Resolved in
  match p with
  | Base -> Atom "Base"
  | Subst(p, m) ->
      let p = Path.Resolved.of_module_type p in
      let m = of_module m in
      List [
        Atom "Subst";
        List [ resolved_path_to_sexp sexp_of_root p;
               resolved_fragment_to_sexp sexp_of_root m ]]
  | SubstAlias(p, m) ->
      let p = Path.Resolved.of_module p in
      let m = of_module m in
      List [
        Atom "SubstAlias";
        List [ resolved_path_to_sexp sexp_of_root p;
               resolved_fragment_to_sexp sexp_of_root m ]]
  | Alias(p, m) ->
      let m = of_module m in
      List [
        Atom "Alias";
        List [ simple_to_sexp sexp_of_root p;
               resolved_fragment_to_sexp sexp_of_root m ]]
  | Hidden m ->
      let m = of_module m in
      List [
        Atom "Hidden";
        List [ resolved_fragment_to_sexp sexp_of_root m ]]
  | Module(m, s) ->
      let m = of_signature m in
      List [
        Atom "Module";
        List [ resolved_fragment_to_sexp sexp_of_root m;
               ModuleName.to_sexp s ]]
  | Type(m, s) ->
      let m = of_signature m in
      List [
        Atom "Type";
        List [ resolved_fragment_to_sexp sexp_of_root m;
               TypeName.to_sexp s ]]
  | Class(m, s) ->
      let m = of_signature m in
      List [
        Atom "Class";
        List [ resolved_fragment_to_sexp sexp_of_root m;
               ClassName.to_sexp s ]]
  | ClassType(m, s) ->
      let m = of_signature m in
      List [
        Atom "ClassType";
        List [ resolved_fragment_to_sexp sexp_of_root m;
               ClassTypeName.to_sexp s ]]

let rec fragment_to_sexp sexp_of_root raw =
  let open Fragment in
  match raw with
  | Resolved r ->
      List [
        Atom "Resolved";
        resolved_fragment_to_sexp sexp_of_root r ]
  | Dot(m, s) ->
      let m = of_signature m in
      List [
        Atom "Dot";
        List [ fragment_to_sexp sexp_of_root m; Atom s ]]

let rec resolved_reference_to_sexp sexp_of_root t =
  let open Reference.Resolved in
  match t with
  | Identifier id ->
      List [ Atom "Identifier";
             identifier_to_sexp sexp_of_root id ]
  | Alias(p, m) ->
      let m = of_module m in
      List [ Atom "Alias";
             List [ simple_to_sexp sexp_of_root p;
                    resolved_reference_to_sexp sexp_of_root m ]]
  | Module(m, s) ->
      let m = of_signature m in
      List [ Atom "Module";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    ModuleName.to_sexp s ]]
  | Hidden m ->
      let m = of_module m in
      List [ Atom "Hidden";
             List [ resolved_reference_to_sexp sexp_of_root m ]]
  | Canonical(p, m) ->
      let m = of_module m in
      List [ Atom "Canonical";
             List [ simple_to_sexp sexp_of_root p;
                    resolved_reference_to_sexp sexp_of_root m ]]
  | ModuleType(m, s) ->
      let m = of_signature m in
      List [ Atom "ModuleType";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    ModuleTypeName.to_sexp s ]]
  | Type(m, s) ->
      let m = of_signature m in
      List [ Atom "Type";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    TypeName.to_sexp s ]]
  | Constructor(t, s) ->
      let t = of_type t in
      List [ Atom "Constructor";
             List [ resolved_reference_to_sexp sexp_of_root t;
                    ConstructorName.to_sexp s ]]
  | Field(t, s) ->
      let t = of_field_parent t in
      List [ Atom "Field";
             List [ resolved_reference_to_sexp sexp_of_root t;
                    FieldName.to_sexp s ]]
  | Extension(m, s) ->
      let m = of_signature m in
      List [ Atom "Extension";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    ExtensionName.to_sexp s ]]
  | Exception(m, s) ->
      let m = of_signature m in
      List [ Atom "Exception";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    ExceptionName.to_sexp s ]]
  | Value(m, s) ->
      let m = of_signature m in
      List [ Atom "Value";
             List [resolved_reference_to_sexp sexp_of_root m;
                   ValueName.to_sexp s ]]
  | Class(m, s) ->
      let m = of_signature m in
      List [ Atom "Class";
             List [resolved_reference_to_sexp sexp_of_root m;
                   ClassName.to_sexp s ]]
  | ClassType(m, s) ->
      let m = of_signature m in
      List [ Atom "ClassType";
             List [ resolved_reference_to_sexp sexp_of_root m;
                    ClassTypeName.to_sexp s ]]
  | Method(c, s) ->
      let c = of_class_signature c in
      List [ Atom "Method";
             List [resolved_reference_to_sexp sexp_of_root c;
                   MethodName.to_sexp s ]]
  | InstanceVariable(c, s) ->
      let c = of_class_signature c in
      List [ Atom "InstanceVariable";
             List [ resolved_reference_to_sexp sexp_of_root c;
                    InstanceVariableName.to_sexp s ]]
  | Label(m, s) ->
      let m = of_label_parent m in
      List [ Atom "Label";
             List [resolved_reference_to_sexp sexp_of_root m;
                   LabelName.to_sexp s ]]

let rec reference_to_sexp sexp_of_root t =
  let open Reference in
  match t with
  | Resolved r ->
      List [ Atom "Resolved";
             resolved_reference_to_sexp sexp_of_root r ]
  | Module(Some m, s) ->
      List [ Atom "Module";
             List [ reference_to_sexp sexp_of_root m; Atom s ] ]
  | Module(None, s) ->
      List [ Atom "Module"; Atom s ]
  | ModuleType(Some m, s) ->
      List [ Atom "ModuleType";
             List [ reference_to_sexp sexp_of_root m; Atom s] ]
  | ModuleType(None, s) ->
      List [ Atom "ModuleType"; Atom s ]
  | Type(Some m, s) ->
      List [ Atom "Type";
             List [ reference_to_sexp sexp_of_root m; Atom s ] ]
  | Type(None, s) ->
      List [ Atom "Type"; Atom s ]
  | Constructor(Some t, s) ->
      List [ Atom "Constructor";
             List [ reference_to_sexp sexp_of_root t; Atom s] ]
  | Constructor(None, s) ->
      List [ Atom "Constructor"; Atom s ]
  | Field(Some t, s) ->
      List [ Atom "Field";
             List [ reference_to_sexp sexp_of_root t; Atom s] ]
  | Field(None, s) ->
      List [ Atom "Field"; Atom s ]
  | Extension(Some m, s) ->
      List [ Atom "Extension";
             List [ reference_to_sexp sexp_of_root m; Atom s ] ]
  | Extension(None, s) ->
      List [ Atom "Extension"; Atom s ]
  | Exception(Some m, s) ->
      List [ Atom "Exception";
             List [ reference_to_sexp sexp_of_root m; Atom s ] ]
  | Exception(None, s) ->
      List [ Atom "Exception"; Atom s ]
  | Value(Some m, s) ->
      List [ Atom "Value";
             List [reference_to_sexp sexp_of_root m; Atom s] ]
  | Value(None, s) ->
      List [ Atom "Value"; Atom s ]
  | Class(Some m, s) ->
      List [ Atom "Class";
             List [reference_to_sexp sexp_of_root m; Atom s] ]
  | Class(None, s) ->
      List [ Atom "Class"; Atom s ]
  | ClassType(Some m, s) ->
      List [ Atom "ClassType";
             List [ reference_to_sexp sexp_of_root m; Atom s ] ]
  | ClassType(None, s) ->
      List [ Atom "ClassType"; Atom s ]
  | Method(Some c, s) ->
      List [ Atom "Method";
             List [reference_to_sexp sexp_of_root c; Atom s] ]
  | Method(None, s) ->
      List [ Atom "Method"; Atom s ]
  | InstanceVariable(Some c, s) ->
      List [ Atom "InstanceVariable";
             List [ reference_to_sexp sexp_of_root c; Atom s ] ]
  | InstanceVariable(None, s) ->
      List [ Atom "InstanceVariable"; Atom s ]
  | Label(Some m, s) ->
      List [ Atom "Label";
             List [reference_to_sexp sexp_of_root m; Atom s] ]
  | Label(None, s) ->
      List [ Atom "Label"; Atom s ]
  | Page(Some m, s) ->
      List [ Atom "Page";
             List [reference_to_sexp sexp_of_root m; Atom s] ]
  | Page(None, s) ->
      List [ Atom "Page"; Atom s ]
  | Unknown(Some m, s) ->
      List [ Atom "Unknown";
             List [reference_to_sexp sexp_of_root m; Atom s]]
  | Unknown(None, s) ->
      List [ Atom "Unknown"; Atom s ]

module Identifier = struct

  let to_sexp = identifier_to_sexp

end

module Path = struct

  module Resolved = struct

    let to_sexp = resolved_path_to_sexp

  end

  let to_sexp = path_to_sexp

end

module Fragment = struct

  module Resolved = struct

    let to_sexp = resolved_fragment_to_sexp

  end

  let to_sexp = fragment_to_sexp

end

module Reference = struct

  module Resolved = struct

    let to_sexp = resolved_reference_to_sexp

  end

  let to_sexp = reference_to_sexp

end

