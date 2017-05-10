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

let (=) = ()

open DocOckNames

module Identifier = struct

  module Pre = struct

    module Signature = struct

      type 'a t =
        | Root of 'a * UnitName.t
        | Module of 'a t * ModuleName.t
        | FunctorParameter of 'a t * FunctorParameterName.t
        | FunctorResult of 'a t
        | ModuleType of 'a t * ModuleTypeName.t

      let equal ~equal id1 id2 =
        let rec loop ~equal id1 id2 =
          match id1, id2 with
          | Root(r1, s1), Root(r2, s2) ->
              UnitName.equal s1 s2
              && equal r1 r2
          | Root _, _ -> false
          | Module(id1, s1), Module(id2, s2) ->
              ModuleName.equal s1 s2
              && loop ~equal id1 id2
          | Module _, _ -> false
          | FunctorParameter(id1, s1), FunctorParameter(id2, s2) ->
              FunctorParameterName.equal s1 s2
              && loop ~equal id1 id2
          | FunctorParameter _, _ -> false
          | FunctorResult id1, FunctorResult id2 ->
              loop ~equal id1 id2
          | FunctorResult _, _ -> false
          | ModuleType(id1, s1), ModuleType(id2, s2) ->
              ModuleTypeName.equal s1 s2
              && loop ~equal id1 id2
          | ModuleType _, _ -> false
        in
        loop ~equal id1 id2

      let rec hash ~hash id =
        let rec loop ~hash id =
          match id with
          | Root(r, s) ->
              Hashtbl.hash (1, hash r, s)
          | Module(id, s) ->
              Hashtbl.hash (2, loop ~hash id, s)
          | FunctorParameter(id, s) ->
              Hashtbl.hash (3, loop ~hash id, s)
          | FunctorResult id ->
              Hashtbl.hash (4, loop ~hash id)
          | ModuleType(id, s) ->
              Hashtbl.hash (5, loop ~hash id, s)
        in
        loop ~hash id

    end

    module Type = struct

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | CoreType of TypeName.t

      let equal ~equal id1 id2 =
        match id1, id2 with
        | Type(id1, s1), Type(id2, s2) ->
            TypeName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Type _, _ -> false
        | CoreType s1, CoreType s2 ->
            TypeName.equal s1 s2
        | CoreType _, _ -> false

      let hash ~hash id =
        match id with
        | Type(id, s) ->
            Hashtbl.hash (6, Signature.hash ~hash id, s)
        | CoreType s ->
            Hashtbl.hash (7, s)

    end

    module ClassSignature = struct

      type 'a t =
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let equal ~equal id1 id2 =
        match id1, id2 with
        | Class(id1, s1), Class(id2, s2) ->
            ClassName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Class _, _ -> false
        | ClassType(id1, s1), ClassType(id2, s2) ->
            ClassTypeName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | ClassType _, _ -> false

      let hash ~hash id =
        match id with
        | Class(id, s) ->
            Hashtbl.hash (14, Signature.hash ~hash id, s)
        | ClassType(id, s) ->
            Hashtbl.hash (15, Signature.hash ~hash id, s)

    end

    module FieldParent = struct

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | CoreType of TypeName.t
        | Constructor of 'a Type.t * ConstructorName.t
        | Extension of 'a Signature.t * ExtensionName.t
        | Exception of 'a Signature.t * ExceptionName.t
        | CoreException of ExceptionName.t

      let rec equal ~equal id1 id2 =
        match id1, id2 with
        | Type(id1, s1), Type(id2, s2) ->
            TypeName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Type _, _ -> false
        | CoreType s1, CoreType s2 ->
            TypeName.equal s1 s2
        | CoreType _, _ -> false
        | Constructor(id1, s1), Constructor(id2, s2) ->
            ConstructorName.equal s1 s2
            && Type.equal ~equal id1 id2
        | Constructor _, _ -> false
        | Extension(id1, s1), Extension(id2, s2) ->
            ExtensionName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Extension _, _ -> false
        | Exception(id1, s1), Exception(id2, s2) ->
            ExceptionName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Exception _, _ -> false
        | CoreException s1, CoreException s2 ->
            ExceptionName.equal s1 s2
        | CoreException _, _ -> false

      let hash ~hash id =
        match id with
        | Type(id, s) ->
            Hashtbl.hash (6, Signature.hash ~hash id, s)
        | CoreType s ->
            Hashtbl.hash (7, s)
        | Constructor(id, s) ->
            Hashtbl.hash (8, Type.hash ~hash id, s)
        | Extension(id, s) ->
            Hashtbl.hash (10, Signature.hash ~hash id, s)
        | Exception(id, s) ->
            Hashtbl.hash (11, Signature.hash ~hash id, s)
        | CoreException s ->
            Hashtbl.hash (12, s)

    end

    module LabelParent = struct

      type 'a t =
        | Root of 'a * UnitName.t
        | Module of 'a Signature.t * ModuleName.t
        | FunctorParameter of 'a Signature.t * FunctorParameterName.t
        | FunctorResult of 'a Signature.t
        | ModuleType of 'a Signature.t * ModuleTypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t
        | Page of 'a * PageName.t

      let rec equal ~equal id1 id2 =
        match id1, id2 with
        | Root(r1, s1), Root(r2, s2) ->
            UnitName.equal s1 s2
            && equal r1 r2
        | Root _, _ -> false
        | Module(id1, s1), Module(id2, s2) ->
            ModuleName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Module _, _ -> false
        | FunctorParameter(id1, s1), FunctorParameter(id2, s2) ->
            FunctorParameterName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | FunctorParameter _, _ -> false
        | FunctorResult id1, FunctorResult id2 ->
            Signature.equal ~equal id1 id2
        | FunctorResult _, _ -> false
        | ModuleType(id1, s1), ModuleType(id2, s2) ->
            ModuleTypeName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | ModuleType _, _ -> false
        | Class(id1, s1), Class(id2, s2) ->
            ClassName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | Class _, _ -> false
        | ClassType(id1, s1), ClassType(id2, s2) ->
            ClassTypeName.equal s1 s2
            && Signature.equal ~equal id1 id2
        | ClassType _, _ -> false
        | Page(r1, s1), Page(r2, s2) ->
            PageName.equal s1 s2
            && equal r1 r2
        | Page _, _ -> false

      let hash ~hash id =
        match id with
        | Root(r, s) ->
            Hashtbl.hash (1, hash r, s)
        | Module(id, s) ->
            Hashtbl.hash (2, Signature.hash ~hash id, s)
        | FunctorParameter(id, s) ->
            Hashtbl.hash (3, Signature.hash ~hash id, s)
        | FunctorResult id ->
            Hashtbl.hash (4, Signature.hash ~hash id)
        | ModuleType(id, s) ->
            Hashtbl.hash (5, Signature.hash ~hash id, s)
        | Class(id, s) ->
            Hashtbl.hash (14, Signature.hash ~hash id, s)
        | ClassType(id, s) ->
            Hashtbl.hash (15, Signature.hash ~hash id, s)
        | Page(r, s) ->
          Hashtbl.hash (19, hash r, s)

    end

  end

  type 'a t =
    | Root of 'a * UnitName.t
    | Module of 'a Pre.Signature.t * ModuleName.t
    | FunctorParameter of 'a Pre.Signature.t * FunctorParameterName.t
    | FunctorResult of 'a Pre.Signature.t
    | ModuleType of 'a Pre.Signature.t * ModuleTypeName.t
    | Type of 'a Pre.Signature.t * TypeName.t
    | CoreType of TypeName.t
    | Constructor of 'a Pre.Type.t * ConstructorName.t
    | Field of 'a Pre.FieldParent.t * FieldName.t
    | Extension of 'a Pre.Signature.t * ExtensionName.t
    | Exception of 'a Pre.Signature.t * ExceptionName.t
    | CoreException of ExceptionName.t
    | Value of 'a Pre.Signature.t * ValueName.t
    | Class of 'a Pre.Signature.t * ClassName.t
    | ClassType of 'a Pre.Signature.t * ClassTypeName.t
    | Method of 'a Pre.ClassSignature.t * MethodName.t
    | InstanceVariable of 'a Pre.ClassSignature.t * InstanceVariableName.t
    | Label of 'a Pre.LabelParent.t * LabelName.t
    | Page of 'a * PageName.t

  let equal ~equal id1 id2 =
    match id1, id2 with
    | Root(r1, s1), Root(r2, s2) ->
        UnitName.equal s1 s2
        && equal r1 r2
    | Root _, _ -> false
    | Module(id1, s1), Module(id2, s2) ->
        ModuleName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Module _, _ -> false
    | FunctorParameter(id1, s1), FunctorParameter(id2, s2) ->
        FunctorParameterName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | FunctorParameter _, _ -> false
    | FunctorResult id1, FunctorResult id2 ->
        Pre.Signature.equal ~equal id1 id2
    | FunctorResult _, _ -> false
    | ModuleType(id1, s1), ModuleType(id2, s2) ->
        ModuleTypeName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | ModuleType _, _ -> false
    | Type(id1, s1), Type(id2, s2) ->
        TypeName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Type _, _ -> false
    | CoreType s1, CoreType s2 ->
        TypeName.equal s1 s2
    | CoreType _, _ -> false
    | Constructor(id1, s1), Constructor(id2, s2) ->
        ConstructorName.equal s1 s2
        && Pre.Type.equal ~equal id1 id2
    | Constructor _, _ -> false
    | Field(id1, s1), Field(id2, s2) ->
        FieldName.equal s1 s2
        && Pre.FieldParent.equal ~equal id1 id2
    | Field _, _ -> false
    | Extension(id1, s1), Extension(id2, s2) ->
        ExtensionName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Extension _, _ -> false
    | Exception(id1, s1), Exception(id2, s2) ->
        ExceptionName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Exception _, _ -> false
    | CoreException s1, CoreException s2 ->
        ExceptionName.equal s1 s2
    | CoreException _, _ -> false
    | Value(id1, s1), Value(id2, s2) ->
        ValueName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Value _, _ -> false
    | Class(id1, s1), Class(id2, s2) ->
        ClassName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | Class _, _ -> false
    | ClassType(id1, s1), ClassType(id2, s2) ->
        ClassTypeName.equal s1 s2
        && Pre.Signature.equal ~equal id1 id2
    | ClassType _, _ -> false
    | Method(id1, s1), Method(id2, s2) ->
        MethodName.equal s1 s2
        && Pre.ClassSignature.equal ~equal id1 id2
    | Method _, _ -> false
    | InstanceVariable(id1, s1), InstanceVariable(id2, s2) ->
        InstanceVariableName.equal s1 s2
        && Pre.ClassSignature.equal ~equal id1 id2
    | InstanceVariable _, _ -> false
    | Label(id1, s1), Label(id2, s2) ->
        LabelName.equal s1 s2
        && Pre.LabelParent.equal ~equal id1 id2
    | Label _, _ -> false
    | Page(r1, s1), Page(r2, s2) ->
        PageName.equal s1 s2
        && equal r1 r2
    | Page _, _ -> false

  let hash ~hash id =
    match id with
    | Root(r, s) ->
        Hashtbl.hash (1, hash r, s)
    | Module(id, s) ->
        Hashtbl.hash (2, Pre.Signature.hash ~hash id, s)
    | FunctorParameter(id, s) ->
        Hashtbl.hash (3, Pre.Signature.hash ~hash id, s)
    | FunctorResult id ->
        Hashtbl.hash (4, Pre.Signature.hash ~hash id)
    | ModuleType(id, s) ->
        Hashtbl.hash (5, Pre.Signature.hash ~hash id, s)
    | Type(id, s) ->
        Hashtbl.hash (6, Pre.Signature.hash ~hash id, s)
    | CoreType s ->
        Hashtbl.hash (7, s)
    | Constructor(id, s) ->
        Hashtbl.hash (8, Pre.Type.hash ~hash id, s)
    | Field(id, s) ->
        Hashtbl.hash (9, Pre.FieldParent.hash ~hash id, s)
    | Extension(id, s) ->
        Hashtbl.hash (10, Pre.Signature.hash ~hash id, s)
    | Exception(id, s) ->
        Hashtbl.hash (11, Pre.Signature.hash ~hash id, s)
    | CoreException s ->
        Hashtbl.hash (12, s)
    | Value(id, s) ->
        Hashtbl.hash (13, Pre.Signature.hash ~hash id, s)
    | Class(id, s) ->
        Hashtbl.hash (14, Pre.Signature.hash ~hash id, s)
    | ClassType(id, s) ->
        Hashtbl.hash (15, Pre.Signature.hash ~hash id, s)
    | Method(id, s) ->
        Hashtbl.hash (16, Pre.ClassSignature.hash ~hash id, s)
    | InstanceVariable(id, s) ->
        Hashtbl.hash (17, Pre.ClassSignature.hash ~hash id, s)
    | Label(id, s) ->
        Hashtbl.hash (18, Pre.LabelParent.hash ~hash id, s)
    | Page(r, s) ->
        Hashtbl.hash (19, hash r, s)

  module Module = struct

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Pre.Signature.t * ModuleName.t
      | FunctorParameter of 'a Pre.Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Pre.Signature.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Root(r1, s1), Root(r2, s2) ->
          UnitName.equal s1 s2
          && equal r1 r2
      | Root _, _ -> false
      | Module(id1, s1), Module(id2, s2) ->
          ModuleName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2
      | Module _, _ -> false
      | FunctorParameter(id1, s1), FunctorParameter(id2, s2) ->
          FunctorParameterName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2
      | FunctorParameter _, _ -> false
      | FunctorResult id1, FunctorResult id2 ->
          Pre.Signature.equal ~equal id1 id2
      | FunctorResult _, _ -> false

    let hash ~hash id =
      match id with
      | Root(r, s) ->
          Hashtbl.hash (1, hash r, s)
      | Module(id, s) ->
          Hashtbl.hash (2, Pre.Signature.hash ~hash id, s)
      | FunctorParameter(id, s) ->
          Hashtbl.hash (3, Pre.Signature.hash ~hash id, s)
      | FunctorResult id ->
          Hashtbl.hash (4, Pre.Signature.hash ~hash id)

  end

  module ModuleType = struct

    type 'a t =
      | ModuleType of 'a Pre.Signature.t * ModuleTypeName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | ModuleType(id1, s1), ModuleType(id2, s2) ->
          ModuleTypeName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | ModuleType(id, s) ->
          Hashtbl.hash (5, Pre.Signature.hash ~hash id, s)

  end

  module Type = struct

    include Pre.Type

  end

  module Constructor = struct

    type 'a t =
      | Constructor of 'a Pre.Type.t * ConstructorName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Constructor(id1, s1), Constructor(id2, s2) ->
          ConstructorName.equal s1 s2
          && Pre.Type.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Constructor(id, s) ->
          Hashtbl.hash (8, Pre.Type.hash ~hash id, s)

  end

  module Field = struct

    type 'a t =
      | Field of 'a Pre.FieldParent.t * FieldName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Field(id1, s1), Field(id2, s2) ->
          FieldName.equal s1 s2
          && Pre.FieldParent.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Field(id, s) ->
          Hashtbl.hash (9, Pre.FieldParent.hash ~hash id, s)

  end

  module Extension = struct

    type 'a t =
      | Extension of 'a Pre.Signature.t * ExtensionName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Extension(id1, s1), Extension(id2, s2) ->
          ExtensionName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Extension(id, s) ->
          Hashtbl.hash (10, Pre.Signature.hash ~hash id, s)

  end

  module Exception = struct

    type 'a t =
      | Exception of 'a Pre.Signature.t * ExceptionName.t
      | CoreException of ExceptionName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Exception(id1, s1), Exception(id2, s2) ->
          ExceptionName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2
      | Exception _, _ -> false
      | CoreException s1, CoreException s2 ->
          ExceptionName.equal s1 s2
      | CoreException _, _ -> false

    let hash ~hash id =
      match id with
      | Exception(id, s) ->
          Hashtbl.hash (11, Pre.Signature.hash ~hash id, s)
      | CoreException s ->
          Hashtbl.hash (12, s)

  end

  module Value = struct

    type 'a t =
      | Value of 'a Pre.Signature.t * ValueName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Value(id1, s1), Value(id2, s2) ->
          ValueName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Value(id, s) ->
          Hashtbl.hash (13, Pre.Signature.hash ~hash id, s)

  end

  module Class = struct

    type 'a t =
      | Class of 'a Pre.Signature.t * ClassName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Class(id1, s1), Class(id2, s2) ->
          ClassName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Class(id, s) ->
          Hashtbl.hash (14, Pre.Signature.hash ~hash id, s)

  end

  module ClassType = struct

    type 'a t =
      | ClassType of 'a Pre.Signature.t * ClassTypeName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | ClassType(id1, s1), ClassType(id2, s2) ->
          ClassTypeName.equal s1 s2
          && Pre.Signature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | ClassType(id, s) ->
          Hashtbl.hash (15, Pre.Signature.hash ~hash id, s)

  end

  module Method = struct

    type 'a t =
      | Method of 'a Pre.ClassSignature.t * MethodName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Method(id1, s1), Method(id2, s2) ->
          MethodName.equal s1 s2
          && Pre.ClassSignature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Method(id, s) ->
          Hashtbl.hash (16, Pre.ClassSignature.hash ~hash id, s)

  end

  module InstanceVariable = struct

    type 'a t =
      | InstanceVariable of 'a Pre.ClassSignature.t * InstanceVariableName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | InstanceVariable(id1, s1), InstanceVariable(id2, s2) ->
          InstanceVariableName.equal s1 s2
          && Pre.ClassSignature.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | InstanceVariable(id, s) ->
          Hashtbl.hash (17, Pre.ClassSignature.hash ~hash id, s)

  end

  module Label = struct

    type 'a t =
      | Label of 'a Pre.LabelParent.t * LabelName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Label(id1, s1), Label(id2, s2) ->
          LabelName.equal s1 s2
          && Pre.LabelParent.equal ~equal id1 id2

    let hash ~hash id =
      match id with
      | Label(id, s) ->
          Hashtbl.hash (18, Pre.LabelParent.hash ~hash id, s)

  end

  module Page = struct

    type 'a t =
      | Page of 'a * PageName.t

    let equal ~equal id1 id2 =
      match id1, id2 with
      | Page(r1, s1), Page(r2, s2) ->
          PageName.equal s1 s2
          && equal r1 r2

    let hash ~hash id =
      match id with
      | Page(r, s) ->
          Hashtbl.hash (19, hash r, s)

  end

  module Signature = struct

    include Pre.Signature

    let of_module : 'a Module.t -> 'a t = function
      | Root(r, s) -> Root(r, s)
      | Module(m, s) -> Module(m, s)
      | FunctorParameter(m, s) -> FunctorParameter(m, s)
      | FunctorResult m -> FunctorResult m

    let of_module_type : 'a ModuleType.t -> 'a t = function
      | ModuleType(m, s) -> ModuleType(m, s)

  end

  module ClassSignature = struct

    include Pre.ClassSignature

    let of_class : 'a Class.t -> 'a t = function
      | Class(m, s) -> Class(m, s)

    let of_class_type : 'a ClassType.t -> 'a t = function
      | ClassType(m, s) -> ClassType(m, s)

  end

  module FieldParent = struct

    include Pre.FieldParent

    let of_type : 'a Type.t -> 'a t = function
      | Type(m, s) -> Type(m, s)
      | CoreType s -> CoreType s

    let of_constructor : 'a Constructor.t -> 'a t = function
      | Constructor(m, s) -> Constructor(m, s)

    let of_extension : 'a Extension.t -> 'a t = function
      | Extension(m, s) -> Extension(m, s)

    let of_exception : 'a Exception.t -> 'a t = function
      | Exception(m, s) -> Exception(m, s)
      | CoreException s -> CoreException s

  end

  module LabelParent = struct

    include Pre.LabelParent

    let of_module : 'a Module.t -> 'a t = function
      | Root(r, s) -> Root(r, s)
      | Module(m, s) -> Module(m, s)
      | FunctorParameter(m, s) -> FunctorParameter(m, s)
      | FunctorResult m -> FunctorResult m

    let of_module_type : 'a ModuleType.t -> 'a t = function
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_class : 'a Class.t -> 'a t = function
      | Class(m, s) -> Class(m, s)

    let of_class_type : 'a ClassType.t -> 'a t = function
      | ClassType(m, s) -> ClassType(m, s)

    let of_page : 'a Page.t -> 'a t = function
      | Page(m, s) -> Page(m, s)

    let of_signature : 'a Signature.t -> 'a t = function
      | Root(r, s) -> Root(r, s)
      | Module(m, s) -> Module(m, s)
      | FunctorParameter(m, s) -> FunctorParameter(m, s)
      | FunctorResult m -> FunctorResult m
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_class_signature : 'a ClassSignature.t -> 'a t = function
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

  end

  let of_module : 'a Module.t -> 'a t = function
    | Root(r, s) -> Root(r, s)
    | Module(m, s) -> Module(m, s)
    | FunctorParameter(m, s) -> FunctorParameter(m, s)
    | FunctorResult m -> FunctorResult m

  let of_module_type : 'a ModuleType.t -> 'a t = function
    | ModuleType(m, s) -> ModuleType(m, s)

  let of_type : 'a Type.t -> 'a t = function
    | Type(m, s) -> Type(m, s)
    | CoreType s -> CoreType s

  let of_constructor : 'a Constructor.t -> 'a t = function
    | Constructor(m, s) -> Constructor(m, s)

  let of_field : 'a Field.t -> 'a t = function
    | Field(m, s) -> Field(m, s)

  let of_extension : 'a Extension.t -> 'a t = function
    | Extension(m, s) -> Extension(m, s)

  let of_exception : 'a Exception.t -> 'a t = function
    | Exception(m, s) -> Exception(m, s)
    | CoreException s -> CoreException s

  let of_value : 'a Value.t -> 'a t = function
    | Value(m, s) -> Value(m, s)

  let of_class : 'a Class.t -> 'a t = function
    | Class(m, s) -> Class(m, s)

  let of_class_type : 'a ClassType.t -> 'a t = function
    | ClassType(m, s) -> ClassType(m, s)

  let of_method : 'a Method.t -> 'a t = function
    | Method(m, s) -> Method(m, s)

  let of_instance_variable : 'a InstanceVariable.t -> 'a t = function
    | InstanceVariable(m, s) -> InstanceVariable(m, s)

  let of_label : 'a Label.t -> 'a t = function
    | Label(m, s) -> Label(m, s)

  let of_page : 'a Page.t -> 'a t = function
    | Page(m, s) -> Page(m, s)

  let of_signature : 'a Signature.t -> 'a t = function
    | Root(r, s) -> Root(r, s)
    | Module(m, s) -> Module(m, s)
    | FunctorParameter(m, s) -> FunctorParameter(m, s)
    | FunctorResult m -> FunctorResult m
    | ModuleType(m, s) -> ModuleType(m, s)

  let of_class_signature : 'a ClassSignature.t -> 'a t = function
    | Class(m, s) -> Class(m, s)
    | ClassType(m, s) -> ClassType(m, s)

  let of_field_parent : 'a FieldParent.t -> 'a t = function
    | Type(m, s) -> Type(m, s)
    | CoreType s -> CoreType s
    | Constructor(m, s) -> Constructor(m, s)
    | Extension(m, s) -> Extension(m, s)
    | Exception(m, s) -> Exception(m, s)
    | CoreException s -> CoreException s

  let of_label_parent : 'a LabelParent.t -> 'a t = function
    | Root(r, s) -> Root(r, s)
    | Module(m, s) -> Module(m, s)
    | FunctorParameter(m, s) -> FunctorParameter(m, s)
    | FunctorResult m -> FunctorResult m
    | ModuleType(m, s) -> ModuleType(m, s)
    | Class(m, s) -> Class(m, s)
    | ClassType(m, s) -> ClassType(m, s)
    | Page(m, s) -> Page(m, s)

  (* Identifier types for paths *)
  module Path = struct

    type 'a t =
      | Root of 'a * UnitName.t
      | Module of 'a Pre.Signature.t * ModuleName.t
      | FunctorParameter of 'a Pre.Signature.t * FunctorParameterName.t
      | FunctorResult of 'a Pre.Signature.t
      | ModuleType of 'a Pre.Signature.t * ModuleTypeName.t
      | Type of 'a Signature.t * TypeName.t
      | CoreType of TypeName.t
      | Class of 'a Signature.t * ClassName.t
      | ClassType of 'a Signature.t * ClassTypeName.t

    let of_module : 'a Module.t -> 'a t = function
      | Root(r, s) -> Root(r, s)
      | Module(m, s) -> Module(m, s)
      | FunctorParameter(m, s) -> FunctorParameter(m, s)
      | FunctorResult m -> FunctorResult m

    let of_module_type : 'a ModuleType.t -> 'a t = function
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_type : 'a Type.t -> 'a t = function
      | Type(m, s) -> Type(m, s)
      | CoreType s -> CoreType s

    let of_class : 'a Class.t -> 'a t = function
      | Class(m, s) -> Class(m, s)

    let of_class_type : 'a ClassType.t -> 'a t = function
      | ClassType(m, s) -> ClassType(m, s)

    module Module = struct

      include Module

      let of_module m = m

    end

    module ModuleType = struct

      include ModuleType

      let of_module_type m = m

    end

    module Type = struct

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | CoreType of TypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let of_type : 'a Type.t -> 'a t = function
        | Type(m, s) -> Type(m, s)
        | CoreType s -> CoreType s

      let of_class : 'a Class.t -> 'a t = function
        | Class(m, s) -> Class(m, s)

      let of_class_type : 'a ClassType.t -> 'a t = function
        | ClassType(m, s) -> ClassType(m, s)

    end

    module ClassType = struct

      type 'a t =
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let of_class : 'a Class.t -> 'a t = function
        | Class(m, s) -> Class(m, s)

      let of_class_type : 'a ClassType.t -> 'a t = function
        | ClassType(m, s) -> ClassType(m, s)

    end

    let of_path_module : 'a Module.t -> 'a t = function
      | Root(r, s) -> Root(r, s)
      | Module(m, s) -> Module(m, s)
      | FunctorParameter(m, s) -> FunctorParameter(m, s)
      | FunctorResult m -> FunctorResult m

    let of_path_module_type : 'a ModuleType.t -> 'a t = function
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_path_type : 'a Type.t -> 'a t = function
      | Type(m, s) -> Type(m, s)
      | CoreType s -> CoreType s
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let of_path_class_type : 'a ClassType.t -> 'a t = function
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

  end

  let of_path_module = of_module

  let of_path_module_type = of_module_type

  let of_path_type : 'a Path.Type.t -> 'a t = function
    | Type(m, s) -> Type(m, s)
    | CoreType s -> CoreType s
    | Class(m, s) -> Class(m, s)
    | ClassType(m, s) -> ClassType(m, s)

  let of_path : 'a Path.t -> 'a t = function
    | Root(r, s) -> Root(r, s)
    | Module(m, s) -> Module(m, s)
    | FunctorParameter(m, s) -> FunctorParameter(m, s)
    | FunctorResult m -> FunctorResult m
    | ModuleType(m, s) -> ModuleType(m, s)
    | Type(m, s) -> Type(m, s)
    | CoreType s -> CoreType s
    | Class(m, s) -> Class(m, s)
    | ClassType(m, s) -> ClassType(m, s)

end

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

end = struct

  module Resolved = struct

    module Module = struct

      type 'a t =
        | Identifier of 'a Identifier.Module.t
        | Module of 'a t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier p = failwith "Not implemented"

    end

  end

  module Module = struct

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Root of string
      | Forward of string
      | Dot of 'a t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

end

module rec Path : sig

  module Resolved : sig

    module rec Module : sig

      type 'a t =
        | Identifier of 'a Identifier.Path.Module.t
        | Module of 'a t * ModuleName.t
        | Apply of 'a t * 'a Path.Module.t
        | Subst of 'a ModuleType.t * 'a t
        | SubstAlias of 'a t * 'a t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

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

end = struct

  module Resolved = struct

    module Module = struct

      type 'a t =
        | Identifier of 'a Identifier.Path.Module.t
        | Module of 'a t * ModuleName.t
        | Apply of 'a t * 'a Path.Module.t
        | Subst of 'a Path.Resolved.ModuleType.t * 'a t
        | SubstAlias of 'a t * 'a t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier p = failwith "Not implemented"

    end

    module ModuleType = struct

      type 'a t =
        | Identifier of 'a Identifier.Path.ModuleType.t
        | ModuleType of 'a Module.t * ModuleTypeName.t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier p = failwith "Not implemented"

    end

    module Type = struct

      type 'a t =
        | Identifier of 'a Identifier.Path.Type.t
        | Type of 'a Module.t * TypeName.t
        | Class of 'a Module.t * ClassName.t
        | ClassType of 'a Module.t * ClassTypeName.t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier p = failwith "Not implemented"

    end

    module ClassType = struct

      type 'a t =
        | Identifier of 'a Identifier.Path.ClassType.t
        | Class of 'a Module.t * ClassName.t
        | ClassType of 'a Module.t * ClassTypeName.t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier p = failwith "Not implemented"

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

    let of_module : 'a Module.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.Path.of_path_module id)
      | Module(m, s) -> Module(m, s)
      | Apply(m, p) -> Apply(m, p)
      | Subst(p, m) -> Subst(p, m)
      | SubstAlias(p, m) -> SubstAlias(p, m)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m
      | Canonical(r, m) -> Canonical(r, m)

    let of_module_type : 'a ModuleType.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.Path.of_path_module_type id)
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_type : 'a Type.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.Path.of_path_type id)
      | Type(m, s) -> Type(m, s)
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let of_class_type : 'a ClassType.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.Path.of_path_class_type id)
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

    let identifier p = failwith "Not implemented"

  end

  module Module = struct

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Root of string
      | Forward of string
      | Dot of 'a t * string
      | Apply of 'a t * 'a t

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  module ModuleType = struct

    type 'a t =
      | Resolved of 'a Resolved.ModuleType.t
      | Dot of 'a Module.t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  module Type = struct

    type 'a t =
      | Resolved of 'a Resolved.Type.t
      | Dot of 'a Module.t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  module ClassType = struct

    type 'a t =
      | Resolved of 'a Resolved.ClassType.t
      | Dot of 'a Module.t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  type 'a t =
    | Resolved of 'a Resolved.t
    | Root of string
    | Forward of string
    | Dot of 'a Module.t * string
    | Apply of 'a Module.t * 'a Module.t

  let of_module : 'a Module.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_module p)
    | Root s -> Root s
    | Forward s -> Forward s
    | Dot(m, s) -> Dot(m, s)
    | Apply(m, p) -> Apply(m, p)

  let of_module_type : 'a ModuleType.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_module_type p)
    | Dot(m, s) -> Dot(m, s)

  let of_type : 'a Type.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_type p)
    | Dot(m, s) -> Dot(m, s)

  let of_class_type : 'a ClassType.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_class_type p)
    | Dot(m, s) -> Dot(m, s)

  let equal ~equal p1 p2 = failwith "Not implemented"

  let hash ~hash p = failwith "Not implemented"

end


(** OCaml path fragments for specifying module substitutions *)
module Fragment = struct

  module Resolved = struct

    module rec Signature : sig

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

    end = struct

      type 'a t =
        | Base
        | Module of 'a t * ModuleName.t
        | Subst of 'a Path.Resolved.ModuleType.t * 'a Module.t
        | SubstAlias of 'a Path.Resolved.Module.t * 'a Module.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t

      let of_module : 'a Module.t -> 'a t = function
        | Module(m, s) -> Module(m, s)
        | Subst(p, m) -> Subst(p, m)
        | SubstAlias(p, m) -> SubstAlias(p, m)
        | Alias(p, m) -> Alias(p, m)
        | Hidden m -> Hidden m

      let identifier id p = failwith "Not implemented"

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

    end

    and Module : sig

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

    end = struct

      type 'a t =
        | Module of 'a Signature.t * ModuleName.t
        | Subst of 'a Path.Resolved.ModuleType.t * 'a t
        | SubstAlias of 'a Path.Resolved.Module.t * 'a t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier id p = failwith "Not implemented"

    end

    module Type = struct

      type 'a t =
        | Type of 'a Signature.t * TypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let equal ~equal p1 p2 = failwith "Not implemented"

      let hash ~hash p = failwith "Not implemented"

      let identifier id p = failwith "Not implemented"

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

    let of_module : 'a Module.t -> 'a t = function
      | Module(m, s) -> Module(m, s)
      | Subst(p, m) -> Subst(p, m)
      | SubstAlias(p, m) -> SubstAlias(p, m)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m

    let of_type : 'a Type.t -> 'a t = function
      | Type(m, s) -> Type(m, s)
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let of_signature : 'a Signature.t -> 'a t = function
      | Base -> Base
      | Module(m, s) -> Module(m, s)
      | Subst(p, m) -> Subst(p, m)
      | SubstAlias(p, m) -> SubstAlias(p, m)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m

    let identifier id p = failwith "Not implemented"

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  module rec Signature : sig

    type 'a t =
      | Resolved of 'a Resolved.Signature.t
      | Dot of 'a t * string

    val of_module : 'a Module.t -> 'a t

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end = struct

    type 'a t =
      | Resolved of 'a Resolved.Signature.t
      | Dot of 'a t * string

    let of_module : 'a Module.t -> 'a t = function
      | Resolved p -> Resolved (Resolved.Signature.of_module p)
      | Dot(m, s) -> Dot(m, s)

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  and Module : sig

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Dot of 'a Signature.t * string

    val equal : equal:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val hash : hash:('a -> int) -> 'a t -> int

  end = struct

    type 'a t =
      | Resolved of 'a Resolved.Module.t
      | Dot of 'a Signature.t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  module Type = struct

    type 'a t =
      | Resolved of 'a Resolved.Type.t
      | Dot of 'a Signature.t * string

    let equal ~equal p1 p2 = failwith "Not implemented"

    let hash ~hash p = failwith "Not implemented"

  end

  type 'a t =
    | Resolved of 'a Resolved.t
    | Dot of 'a Signature.t * string

  let of_module : 'a Module.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_module p)
    | Dot(m, s) -> Dot(m, s)

  let of_type : 'a Type.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_type p)
    | Dot(m, s) -> Dot(m, s)

  let of_signature : 'a Signature.t -> 'a t = function
    | Resolved p -> Resolved (Resolved.of_signature p)
    | Dot(m, s) -> Dot(m, s)

  let equal ~equal p1 p2 = failwith "Not implemented"

  let hash ~hash p = failwith "Not implemented"

end

module Reference = struct

  module Resolved = struct

    module rec Signature : sig

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

    end = struct

      type 'a t =
        | Identifier of 'a Identifier.Signature.t
        | Module of 'a t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t
        | Canonical of 'a Simple.Module.t * 'a Module.t
        | ModuleType of 'a t * ModuleTypeName.t

      let of_module : 'a Module.t -> 'a t = function
        | Identifier id -> Identifier (Identifier.Signature.of_module id)
        | Module(m, s) -> Module(m, s)
        | Alias(p, m) -> Alias(p, m)
        | Hidden m -> Hidden m
        | Canonical(p, m) -> Canonical(p, m)

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

    end

    and Module : sig

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

    end = struct

      type 'a t =
        | Identifier of 'a Identifier.Module.t
        | Module of 'a Signature.t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a t
        | Hidden of 'a t
        | Canonical of 'a Simple.Module.t * 'a t

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

    end

    module Type = struct

      type 'a t =
        | Identifier of 'a Identifier.Type.t
        | Type of 'a Signature.t * TypeName.t

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

    end

    module ClassSignature = struct

      type 'a t =
        | Identifier of 'a Identifier.ClassSignature.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

    end

    module FieldParent = struct

      type 'a t =
        | Identifier of 'a Identifier.FieldParent.t
        | Type of 'a Signature.t * TypeName.t
        | Constructor of 'a Type.t * ConstructorName.t
        | Extension of 'a Signature.t * ExtensionName.t
        | Exception of 'a Signature.t * ExceptionName.t

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

    end

    module LabelParent = struct

      type 'a t =
        | Identifier of 'a Identifier.LabelParent.t
        | Module of 'a Signature.t * ModuleName.t
        | Alias of 'a Simple.Module.t * 'a Module.t
        | Hidden of 'a Module.t
        | Canonical of 'a Simple.Module.t * 'a Module.t
        | ModuleType of 'a Signature.t * ModuleTypeName.t
        | Class of 'a Signature.t * ClassName.t
        | ClassType of 'a Signature.t * ClassTypeName.t

      let of_signature : 'a Signature.t -> 'a t = function
        | Identifier id ->
            Identifier (Identifier.LabelParent.of_signature id)
        | Module(m, s) -> Module(m, s)
        | Alias(p, m) -> Alias(p, m)
        | Hidden m -> Hidden m
        | Canonical(p, m) -> Canonical(p, m)
        | ModuleType(m, s) -> ModuleType(m, s)

      let of_class_signature : 'a ClassSignature.t -> 'a t = function
        | Identifier id ->
            Identifier (Identifier.LabelParent.of_class_signature id)
        | Class(m, s) -> Class(m, s)
        | ClassType(m, s) -> ClassType(m, s)

      let equal ~equal r1 r2 = failwith "Not implemented"

      let hash ~hash r = failwith "Not implemented"

      let identifier r = failwith "Not implemented"

      let rebase id r = failwith "Not implemented"

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

    let of_module : 'a Module.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_module id)
      | Module(m, s) -> Module(m, s)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m
      | Canonical(p, m) -> Canonical(p, m)

    let of_type : 'a Type.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_type id)
      | Type(m, s) -> Type(m, s)

    let of_signature : 'a Signature.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_signature id)
      | Module(m, s) -> Module(m, s)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m
      | Canonical(p, m) -> Canonical(p, m)
      | ModuleType(m, s) -> ModuleType(m, s)

    let of_class_signature : 'a ClassSignature.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_class_signature id)
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let of_field_parent : 'a FieldParent.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_field_parent id)
      | Type(m, s) -> Type(m, s)
      | Constructor(m, s) -> Constructor(m, s)
      | Extension(m, s) -> Extension(m, s)
      | Exception(m, s) -> Exception(m, s)

    let of_label_parent : 'a LabelParent.t -> 'a t = function
      | Identifier id -> Identifier (Identifier.of_label_parent id)
      | Module(m, s) -> Module(m, s)
      | Alias(p, m) -> Alias(p, m)
      | Hidden m -> Hidden m
      | Canonical(p, m) -> Canonical(p, m)
      | ModuleType(m, s) -> ModuleType(m, s)
      | Class(m, s) -> Class(m, s)
      | ClassType(m, s) -> ClassType(m, s)

    let equal ~equal r1 r2 = failwith "Not implemented"

    let hash ~hash r = failwith "Not implemented"

    let identifier r = failwith "Not implemented"

    let rebase id r = failwith "Not implemented"

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

  let equal ~equal r1 r2 = failwith "Not implemented"

  let hash ~hash r = failwith "Not implemented"

end
