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
    | ModuleType of 'a Path.module_type
    | Type of 'a Path.type_
    | Constructor of 'a Reference.constructor
    | Field of 'a Reference.field
    | Value of 'a Reference.value
    | Class of 'a Reference.class_
    | ClassType of 'a Path.class_type

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

module Defn_map = struct

  module Type = struct

    type 'a t =
      { constructors : 'a Defn.t SMap.t;
        fields : 'a Defn.t SMap.t; }

    let empty =
      let constructors = SMap.empty in
      let fields = SMap.empty in
      { constructors; fields }

    let is_empty t =
      SMap.is_empty t.constructors
      && SMap.is_empty t.fields

    let add_constructor t name defn =
      let constructors = SMap.add name defn t.constructors in
      { t with constructors }

    let add_field t name defn =
      let fields = SMap.add name defn t.fields in
      { t with fields }

    let remove_constructor t name =
      let constructors = SMap.remove name t.constructors in
      { t with constructors }

    let remove_field t name =
      let fields = SMap.remove name t.fields in
      { t with fields }

    let project_constructor t name =
      SMap.find_opt name t.constructors

    let project_field t name =
      SMap.find_opt name t.fields

  end

  module Class = struct

    type 'a t =
      { methods : 'a Defn.t SMap.t;
        instance_variables : 'a Defn.t SMap.t; }

    let empty =
      let methods = SMap.empty in
      let instance_variables = SMap.empty in
      { methods; instance_variables }

    let is_empty t =
      SMap.is_empty t.methods
      && SMap.is_empty t.instance_variables

    let add_method t name defn =
      let methods = SMap.add name defn t.methods in
      { t with methods }

    let add_instance_variable t name defn =
      let instance_variables = SMap.add name defn t.instance_variables in
      { t with instance_variables }

    let remove_method t name =
      let methods = SMap.remove name t.methods in
      { t with methods }

    let remove_instance_variable t name =
      let instance_variables = SMap.remove name t.instance_variables in
      { t with instance_variables }

    let project_method t name =
      SMap.find_opt name t.methods

    let project_instance_variable t name =
      SMap.find_opt name t.instance_variables

  end

  module Module = struct

    type 'a t =
      { modules : ('a Defn.t * 'a t) SMap.t;
        module_types : 'a Defn.t SMap.t;
        types : ('a Defn.t * 'a Type.t) SMap.t;
        extensions : 'a Defn.t SMap.t;
        values : 'a Defn.t SMap.t;
        classes : ('a Defn.t * 'a Class.t) SMap.t;
        class_types : 'a Defn.t SMap.t; }

    let empty =
      let modules = SMap.empty in
      let module_types = SMap.empty in
      let types = SMap.empty in
      let extensions = SMap.empty in
      let values = SMap.empty in
      let classes = SMap.empty in
      let class_types = SMap.empty in
      { modules; module_types; types; extensions; values; classes; class_types }

    let is_empty t =
      SMap.is_empty t.modules
      && SMap.is_empty t.module_types
      && SMap.is_empty t.types
      && SMap.is_empty t.extensions
      && SMap.is_empty t.values
      && SMap.is_empty t.classes
      && SMap.is_empty t.class_types

    let add_module t name defn md =
      let modules = SMap.add name (defn, md) t.modules in
      { t with modules }

    let add_module_type t name defn =
      let module_types = SMap.add name defn t.module_types in
      { t with module_types }

    let add_type t name defn ty =
      let types = SMap.add name (defn, ty) t.types in
      { t with types }

    let add_extension t name defn =
      let extensions = SMap.add name defn t.extensions in
      { t with extensions }

    let add_value t name defn =
      let values = SMap.add name defn t.values in
      { t with values }

    let add_class t name defn cl =
      let classes = SMap.add name (defn, cl) t.classes in
      { t with classes }

    let add_class_type t name defn =
      let class_types = SMap.add name defn t.class_types in
      { t with class_types }

    let remove_module t name =
      let modules = SMap.remove name t.modules in
      { t with modules }

    let remove_module_type t name =
      let module_types = SMap.remove name t.module_types in
      { t with module_types }

    let remove_type t name =
      let types = SMap.remove name t.types in
      { t with types }

    let remove_extension t name =
      let extensions = SMap.remove name t.extensions in
      { t with extensions }

    let remove_value t name =
      let values = SMap.remove name t.values in
      { t with values }

    let remove_class t name =
      let classes = SMap.remove name t.classes in
      { t with classes }

    let remove_class_type t name =
      let class_types = SMap.remove name t.class_types in
      { t with class_types }

    let project_module t name =
      match SMap.find name t.modules with
      | (defn, t) -> Some defn, t
      | exception Not_found -> None, empty

    let project_module_type t name =
      SMap.find_opt name t.module_types

    let project_type t name =
      match SMap.find name t.types with
      | (defn, t) -> Some defn, t
      | exception Not_found -> None, Type.empty

    let project_extension t name =
      SMap.find_opt name t.extensions

    let project_value t name =
      SMap.find_opt name t.values

    let project_class t name =
      match SMap.find name t.classes with
      | (defn, t) -> Some defn, t
      | exception Not_found -> None, Class.empty

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

    method virtual defn : 'a Defn.t -> 'a Defn.t

    method source_defn_map_module module_ =
      let open Module in
      let {modules; module_types; types;
           extensions; values; classes; class_types} =
        module_
      in
      let modules' =
        SMap.map
          (pair_map self#defn self#source_defn_map_module)
          modules
      in
      let module_types' = SMap.map self#defn module_types in
      let types' =
        SMap.map (pair_map self#defn self#source_defn_map_type) types
      in
      let extensions' = SMap.map self#defn extensions in
      let values' = SMap.map self#defn values in
      let classes' =
        SMap.map
          (pair_map self#defn self#source_defn_map_class)
          classes
      in
      let class_types' = SMap.map self#defn class_types in
        if
          modules != modules' || module_types != module_types'
          || types != types' || extensions != extensions' || values != values'
          || classes != classes' || class_types != class_types'
        then
          {modules = modules'; module_types = module_types';
           types = types'; extensions = extensions'; values = values';
           classes = classes'; class_types = class_types'}
        else
          module_

    method source_defn_map_class class_ =
      let open Class in
      let {methods; instance_variables} = class_ in
      let methods' = SMap.map self#defn methods in
      let instance_variables' = SMap.map self#defn instance_variables in
        if
          methods != methods' || instance_variables != instance_variables'
        then
          {methods = methods'; instance_variables = instance_variables'}
        else
          class_

    method source_defn_map_type type_ =
      let open Type in
      let {constructors; fields} = type_ in
      let constructors' = SMap.map self#defn constructors in
      let fields' = SMap.map self#defn fields in
        if constructors != constructors' || fields != fields' then
          {constructors = constructors'; fields = fields'}
        else type_

  end

end
