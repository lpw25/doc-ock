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

module Decl_map : sig

  module Type : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_constructor :
      'a t -> string -> 'a Decl.t -> 'a t

    val add_field :
      'a t -> string -> 'a Decl.t -> 'a t

    val project_constructor :
      'a t -> string -> 'a Decl.t option

    val project_field :
      'a t -> string -> 'a Decl.t option

  end

  module Class_signature : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_method :
      'a t -> string -> 'a Decl.t -> 'a t

    val add_instance_variable :
      'a t -> string -> 'a Decl.t -> 'a t

    val project_method :
      'a t -> string -> 'a Decl.t option

    val project_instance_variable :
      'a t -> string -> 'a Decl.t option

  end

  module Signature : sig

    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add_module :
      'a t -> string -> 'a Decl.t -> 'a t -> 'a t

    val add_module_type :
      'a t -> string -> 'a Decl.t -> 'a t -> 'a t

    val add_type :
      'a t -> string -> 'a Decl.t -> 'a Type.t -> 'a t

    val add_extension :
      'a t -> string -> 'a Decl.t -> 'a t

    val add_class :
      'a t -> string -> 'a Decl.t -> 'a Class_signature.t -> 'a t

    val add_class_type :
      'a t -> string -> 'a Decl.t -> 'a Class_signature.t -> 'a t

    val project_module :
      'a t -> string -> ('a Decl.t * 'a t) option

    val project_module_type :
      'a t -> string -> ('a Decl.t * 'a t) option

    val project_type :
      'a t -> string -> ('a Decl.t * 'a Type.t) option

    val project_extension :
      'a t -> string -> 'a Decl.t option

    val project_class :
      'a t -> string -> ('a Decl.t * 'a Class_signature.t) option

    val project_class_type :
      'a t -> string -> ('a Decl.t * 'a Class_signature.t) option

  end

  class virtual ['a] map : object

    method virtual decl : 'a Decl.t -> 'a Decl.t

    method source_decl_map_type : 'a Type.t -> 'a Type.t

    method source_decl_map_class_signature :
      'a Class_signature.t -> 'a Class_signature.t

    method source_decl_map_signature : 'a Signature.t -> 'a Signature.t

  end

end
