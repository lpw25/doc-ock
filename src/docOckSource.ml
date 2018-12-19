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

module File = struct

  type t =
    { name: string;
      build_dir: string;
      digest: Digest.t; }

end

module Position = struct

  type t =
    { line: int;
      column: int; }

end

module Location = struct

  type t =
    { start: Position.t;
      finish: Position.t; }

end

module Use = struct

  type 'a path =
    | Module of 'a Path.module_
    | Module_type of 'a Path.module_type
    | Type of 'a Path.type_
    | Constructor of 'a Reference.constructor
    | Field of 'a Reference.field
    | Value of 'a Reference.value
    | Class of 'a Reference.class_
    | Class_type of 'a Path.class_type
    | Method of 'a Reference.method_
    | Instance_variable of 'a Reference.instance_variable

  type 'a t =
    { path: 'a path;
      location: Location.t; }

end

module Declaration = struct

  type t =
    { index: Decl.Index.t;
      location: Location.t; }

end

module Definition = struct

  type t =
    { index: Defn.Index.t;
      location: Location.t; }

end

module Interface = struct

  type 'a t =
    { file : File.t;
      uses : 'a Use.t list;
      declarations : Declaration.t list; }

end

module Implementation = struct

  type 'a t =
    { file : File.t;
      uses : 'a Use.t list;
      definitions : Definition.t list; }

end

type 'a t =
  { interface: 'a Interface.t option;
    implementation: 'a Implementation.t option; }

module SMap = Map.Make(String)

module Decl_map = struct

  module Type = struct

    type 'a t =
      { constructors : 'a Decl.t SMap.t;
        fields : 'a Decl.t SMap.t; }

    let empty =
      let constructors = SMap.empty in
      let fields = SMap.empty in
      { constructors; fields }

    let is_empty t =
      SMap.is_empty t.constructors
      && SMap.is_empty t.fields

    let add_constructor t name decl =
      let constructors = SMap.add name decl t.constructors in
      { t with constructors }

    let add_field t name decl =
      let fields = SMap.add name decl t.fields in
      { t with fields }

    let project_constructor t name =
      SMap.find_opt name t.constructors

    let project_field t name =
      SMap.find_opt name t.fields

  end

  module Class_signature = struct

    type 'a t =
      { methods : 'a Decl.t SMap.t;
        instance_variables : 'a Decl.t SMap.t; }

    let empty =
      let methods = SMap.empty in
      let instance_variables = SMap.empty in
      { methods; instance_variables }

    let is_empty t =
      SMap.is_empty t.methods
      && SMap.is_empty t.instance_variables

    let add_method t name decl =
      let methods = SMap.add name decl t.methods in
      { t with methods }

    let add_instance_variable t name decl =
      let instance_variables = SMap.add name decl t.instance_variables in
      { t with instance_variables }

    let project_method t name =
      SMap.find_opt name t.methods

    let project_instance_variable t name =
      SMap.find_opt name t.instance_variables

  end

  module Signature = struct

    type 'a t =
      { modules : ('a Decl.t * 'a t) SMap.t;
        module_types : ('a Decl.t * 'a t) SMap.t;
        types : ('a Decl.t * 'a Type.t) SMap.t;
        extensions : 'a Decl.t SMap.t;
        classes : ('a Decl.t * 'a Class_signature.t) SMap.t;
        class_types : ('a Decl.t * 'a Class_signature.t) SMap.t; }

    let empty =
      let modules = SMap.empty in
      let module_types = SMap.empty in
      let types = SMap.empty in
      let extensions = SMap.empty in
      let classes = SMap.empty in
      let class_types = SMap.empty in
      { modules; module_types; types; extensions; classes; class_types }

    let is_empty t =
      SMap.is_empty t.modules
      && SMap.is_empty t.module_types
      && SMap.is_empty t.types
      && SMap.is_empty t.extensions
      && SMap.is_empty t.classes
      && SMap.is_empty t.class_types

    let add_module t name decl md =
      let modules = SMap.add name (decl, md) t.modules in
      { t with modules }

    let add_module_type t name decl mty =
      let module_types = SMap.add name (decl, mty) t.module_types in
      { t with module_types }

    let add_type t name decl ty =
      let types = SMap.add name (decl, ty) t.types in
      { t with types }

    let add_extension t name decl =
      let extensions = SMap.add name decl t.extensions in
      { t with extensions }

    let add_class t name decl cl =
      let classes = SMap.add name (decl, cl) t.classes in
      { t with classes }

    let add_class_type t name decl cty =
      let class_types = SMap.add name (decl, cty) t.class_types in
      { t with class_types }

    let project_module t name =
      SMap.find_opt name t.modules

    let project_module_type t name =
      SMap.find_opt name t.module_types

    let project_type t name =
      SMap.find_opt name t.types

    let project_extension t name =
      SMap.find_opt name t.extensions

    let project_class t name =
      SMap.find_opt name t.classes

    let project_class_type t name =
      SMap.find_opt name t.class_types

  end

  let pair_map af bf p =
    let (a, b) = p in
    let a' = af a in
    let b' = bf b in
    if a != a' || b != b' then (a', b')
    else p

  class virtual ['a] map = object (self)

    method virtual decl : 'a Decl.t -> 'a Decl.t

    method source_decl_map_signature signature =
      let open Signature in
      let {modules; module_types; types; extensions; classes; class_types} =
        signature
      in
      let modules' =
        SMap.map
          (pair_map self#decl self#source_decl_map_signature)
          modules
      in
      let module_types' =
        SMap.map
          (pair_map self#decl self#source_decl_map_signature)
          module_types
      in
      let types' =
        SMap.map (pair_map self#decl self#source_decl_map_type) types
      in
      let extensions' = SMap.map self#decl extensions in
      let classes' =
        SMap.map
          (pair_map self#decl self#source_decl_map_class_signature)
          classes
      in
      let class_types' =
        SMap.map
          (pair_map self#decl self#source_decl_map_class_signature)
          class_types
      in
        if
          modules != modules' || module_types != module_types'
          || types != types' || extensions != extensions'
          || classes != classes' || class_types != class_types'
        then
          {modules = modules'; module_types = module_types';
           types = types'; extensions = extensions';
           classes = classes'; class_types = class_types'}
        else
          signature

    method source_decl_map_class_signature class_signature =
      let open Class_signature in
      let {methods; instance_variables} = class_signature in
      let methods' = SMap.map self#decl methods in
      let instance_variables' = SMap.map self#decl instance_variables in
        if
          methods != methods' || instance_variables != instance_variables'
        then
          {methods = methods'; instance_variables = instance_variables'}
        else
          class_signature

    method source_decl_map_type type_ =
      let open Type in
      let {constructors; fields} = type_ in
      let constructors' = SMap.map self#decl constructors in
      let fields' = SMap.map self#decl fields in
        if constructors != constructors' || fields != fields' then
          {constructors = constructors'; fields = fields'}
        else type_

  end

end
