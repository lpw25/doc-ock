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

module OCamlPath = Path

open DocOckPredef
open DocOckNames
open DocOckPaths.Identifier

type 'a t =
  { modules : 'a Path.Module.t Ident.tbl;
    module_types : 'a Path.ModuleType.t Ident.tbl;
    types : 'a Path.Type.t Ident.tbl;
    class_types : 'a Path.ClassType.t Ident.tbl; }

let empty =
  { modules = Ident.empty;
    module_types = Ident.empty;
    types = Ident.empty;
    class_types = Ident.empty; }

let builtin_idents = List.map snd Predef.builtin_idents

let add_module parent id env =
  let name = ModuleName.of_ident id in
  let identifier = Path.Module.Module(parent, name) in
  let modules = Ident.add id identifier env.modules in
    { env with modules }

let add_parameter parent id env =
  let name = FunctorParameterName.of_ident id in
  let identifier = Path.Module.FunctorParameter(parent, name) in
  let modules = Ident.add id identifier env.modules in
    { env with modules }

let add_module_type parent id env =
  let name = ModuleTypeName.of_ident id in
  let identifier = Path.ModuleType.ModuleType(parent, name) in
  let module_types = Ident.add id identifier env.module_types in
    { env with module_types }

let add_type parent id env =
  let name = TypeName.of_ident id in
  let identifier = Path.Type.Type(parent, name) in
  let types = Ident.add id identifier env.types in
    { env with types }

let add_class parent id ty_id obj_id cl_id env =
  let name = ClassName.of_ident id in
  let type_identifier = Path.Type.Class(parent, name) in
  let class_type_identifier = Path.ClassType.Class(parent, name) in
  let add_idents identifier tbl =
    Ident.add id identifier
      (Ident.add ty_id identifier
         (Ident.add obj_id identifier
            (Ident.add cl_id identifier tbl)))
  in
  let types = add_idents type_identifier env.types in
  let class_types = add_idents class_type_identifier env.class_types in
    { env with types; class_types }

let add_class_type parent id obj_id cl_id env =
  let name = ClassTypeName.of_ident id in
  let type_identifier = Path.Type.ClassType(parent, name) in
  let class_type_identifier = Path.ClassType.ClassType(parent, name) in
  let add_idents identifier tbl =
    Ident.add id identifier
         (Ident.add obj_id identifier
            (Ident.add cl_id identifier tbl))
  in
  let types = add_idents type_identifier env.types in
  let class_types = add_idents class_type_identifier env.class_types in
    { env with types; class_types }

let rec add_signature_type_items parent items env =
  let open Types in
    match items with
    | Sig_type(id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          if Btype.is_row_name (Ident.name id) then env
          else add_type parent id env
    | Sig_module(id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_module parent id env
    | Sig_modtype(id, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_module_type parent id env
    | Sig_class(id, _, _) :: Sig_class_type(ty_id, _, _)
        :: Sig_type(obj_id, _, _) :: Sig_type(cl_id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_class parent id ty_id obj_id cl_id env
    | Sig_class _ :: _ -> assert false
    | Sig_class_type(id, _, _) :: Sig_type(obj_id, _, _)
      :: Sig_type(cl_id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_class_type parent id obj_id cl_id env
    | Sig_class_type _ :: _ -> assert false
    | (Sig_value _ | Sig_typext _) :: rest ->
        add_signature_type_items parent rest env
    | [] -> env

let add_signature_tree_item parent item env =
  let open Typedtree in
    match item.sig_desc with
    | Tsig_type (_rec_flag, decls) -> (* TODO: remember rec_flag *)
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id env)
          decls env
    | Tsig_module md ->
        add_module parent md.md_id env
    | Tsig_recmodule mds ->
        List.fold_right
          (fun md env -> add_module parent md.md_id env)
          mds env
    | Tsig_modtype mtd ->
        add_module_type parent mtd.mtd_id env
    | Tsig_include incl ->
        add_signature_type_items parent incl.incl_type env
    | Tsig_class cls ->
        List.fold_right
          (fun cld env ->
             add_class parent cld.ci_id_class
               cld.ci_id_class_type cld.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp
#else
               cld.ci_id_typehash
#endif
               env)
          cls env
    | Tsig_class_type cltyps ->
        List.fold_right
          (fun clty env ->
             add_class_type parent clty.ci_id_class_type
               clty.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp
#else
               clty.ci_id_typehash
#endif
               env)
          cltyps env
    | Tsig_value _ | Tsig_typext _
    | Tsig_exception _ | Tsig_open _
    | Tsig_attribute _ -> env

let add_signature_tree_items parent sg env =
  let open Typedtree in
    List.fold_right
      (add_signature_tree_item parent)
      sg.sig_items env

let add_structure_tree_item parent item env =
  let open Typedtree in
    match item.str_desc with
    | Tstr_type (_rec_flag, decls) -> (* TODO: remember rec_flag *)
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id env)
          decls env
    | Tstr_module mb -> add_module parent mb.mb_id env
    | Tstr_recmodule mbs ->
        List.fold_right
          (fun mb env -> add_module parent mb.mb_id env)
          mbs env
    | Tstr_modtype mtd ->
        add_module_type parent mtd.mtd_id env
    | Tstr_include incl ->
        add_signature_type_items parent incl.incl_type env
    | Tstr_class cls ->
        List.fold_right
          (fun (cld, _) env ->
             add_class parent cld.ci_id_class
               cld.ci_id_class_type cld.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp
#else
               cld.ci_id_typehash
#endif
               env)
          cls env
    | Tstr_class_type cltyps ->
        List.fold_right
          (fun (_, _, clty) env ->
             add_class_type parent clty.ci_id_class_type
               clty.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp
#else
               clty.ci_id_typehash
#endif
               env)
          cltyps env
    | Tstr_eval _ | Tstr_value _
    | Tstr_primitive _ | Tstr_typext _
    | Tstr_exception _ | Tstr_open _
    | Tstr_attribute _ -> env

let add_structure_tree_items parent str env =
  let open Typedtree in
    List.fold_right
      (add_structure_tree_item parent)
      str.str_items env

let find_module env id =
  Ident.find_same id env.modules

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type env id =
  try
    Ident.find_same id env.types
  with Not_found ->
    if List.mem id builtin_idents then
        match core_type_identifier (Ident.name id) with
        | Some id -> Path.Type.of_type id
        | None -> raise Not_found
    else raise Not_found

let find_class_type env id =
  Ident.find_same id env.class_types

module Path = struct

  open DocOckPaths.Path.Resolved
  open DocOckPaths.Path

  let read_module_ident env id =
    if Ident.persistent id then Module.Root (Ident.name id)
    else
      try Module.Resolved (Identifier (find_module env id))
      with Not_found -> assert false

  let read_module_type_ident env id =
    try
      ModuleType.Resolved (Identifier (find_module_type env id))
    with Not_found -> assert false

  let read_type_ident env id =
    try
      Type.Resolved (Identifier (find_type env id))
    with Not_found -> assert false

  let read_class_type_ident env id =
    try
      ClassType.Resolved (Identifier (find_class_type env id))
    with Not_found ->
      ClassType.Dot(Root "*", (Ident.name id))
      (* TODO remove this hack once the fix for PR#6650
         is in the OCaml release *)

  let rec read_module env = function
    | OCamlPath.Pident id -> read_module_ident env id
    | OCamlPath.Pdot(p, s, _) -> Dot(read_module env p, s)
    | OCamlPath.Papply(p, arg) -> Apply(read_module env p, read_module env arg)

  let read_module_type env = function
    | OCamlPath.Pident id -> read_module_type_ident env id
    | OCamlPath.Pdot(p, s, _) -> Dot(read_module env p, s)
    | OCamlPath.Papply(_, _)-> assert false

  let read_class_type env = function
    | OCamlPath.Pident id -> read_class_type_ident env id
    | OCamlPath.Pdot(p, s, _) -> Dot(read_module env p, s)
    | OCamlPath.Papply(_, _)-> assert false

  let read_type env = function
    | OCamlPath.Pident id -> read_type_ident env id
    | OCamlPath.Pdot(p, s, _) -> Dot(read_module env p, s)
    | OCamlPath.Papply(_, _)-> assert false

end

module Fragment = struct

  open DocOckPaths.Fragment.Resolved
  open DocOckPaths.Fragment

  let rec read_module = function
    | Longident.Lident s -> Module.Dot(Resolved Base, s)
    | Longident.Ldot(p, s) -> Module.Dot(Signature.of_module (read_module p), s)
    | Longident.Lapply _ -> assert false

  let read_type = function
    | Longident.Lident s -> Type.Dot(Resolved Base, s)
    | Longident.Ldot(p, s) -> Type.Dot(Signature.of_module (read_module p), s)
    | Longident.Lapply _ -> assert false

end
