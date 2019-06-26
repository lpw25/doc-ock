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

module File : sig

  type t =
    { name: string;
      build_dir: string;
      digest: Digest.t; }

end

module Position : sig

  type t =
    { line: int;
      column: int; }

end

module Location : sig

  type t =
    { start: Position.t;
      finish: Position.t; }

end

module Use : sig

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

module Declaration : sig

  type t =
    { index: Decl.Index.t;
      location: Location.t; }

end

module Definition : sig

  type t =
    { index: Defn.Index.t;
      location: Location.t; }

end

module Interface : sig

  type 'a t =
    { file : File.t;
      uses : 'a Use.t list;
      declarations : Declaration.t list; }

end

module Implementation : sig

  type 'a t =
    { file : File.t;
      uses : 'a Use.t list;
      definitions : Definition.t list; }

end

type 'a t =
  { interface: 'a Interface.t option;
    implementation: 'a Implementation.t option; }

module Defn_map : sig

  module Type : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_constructor :
      'a t -> string -> 'a Defn.t -> 'a t

    val add_field :
      'a t -> string -> 'a Defn.t -> 'a t

    val project_constructor :
      'a t -> string -> 'a Defn.t option

    val project_field :
      'a t -> string -> 'a Defn.t option

  end

  module Class : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_method :
      'a t -> string -> 'a Defn.t -> 'a t

    val add_instance_variable :
      'a t -> string -> 'a Defn.t -> 'a t

    val project_method :
      'a t -> string -> 'a Defn.t option

    val project_instance_variable :
      'a t -> string -> 'a Defn.t option

  end

  module Module : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_module :
      'a t -> string -> 'a Defn.t -> 'a t -> 'a t

    val add_module_type :
      'a t -> string -> 'a Defn.t -> 'a t

    val add_type :
      'a t -> string -> 'a Defn.t -> 'a Type.t -> 'a t

    val add_extension :
      'a t -> string -> 'a Defn.t -> 'a t

    val add_value :
      'a t -> string -> 'a Defn.t -> 'a t

    val add_class :
      'a t -> string -> 'a Defn.t -> 'a Class.t -> 'a t

    val add_class_type :
      'a t -> string -> 'a Defn.t -> 'a t

    val remove_module :
      'a t -> string -> 'a t

    val remove_module_type :
      'a t -> string -> 'a t

    val remove_type :
      'a t -> string -> 'a t

    val remove_extension :
      'a t -> string -> 'a t

    val remove_value :
      'a t -> string -> 'a t

    val remove_class :
      'a t -> string -> 'a t

    val remove_class_type :
      'a t -> string -> 'a t

    val project_module :
      'a t -> string -> 'a Defn.t option * 'a t

    val project_module_type :
      'a t -> string -> 'a Defn.t option

    val project_type :
      'a t -> string -> 'a Defn.t option * 'a Type.t

    val project_extension :
      'a t -> string -> 'a Defn.t option

    val project_value :
      'a t -> string -> 'a Defn.t option

    val project_class :
      'a t -> string -> 'a Defn.t option * 'a Class.t

    val project_class_type :
      'a t -> string -> 'a Defn.t option

  end

  class virtual ['a] map : object

    method virtual defn : 'a Defn.t -> 'a Defn.t

    method source_defn_map_type : 'a Type.t -> 'a Type.t

    method source_defn_map_class : 'a Class.t -> 'a Class.t

    method source_defn_map_module : 'a Module.t -> 'a Module.t

  end

end
