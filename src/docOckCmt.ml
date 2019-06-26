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

open Asttypes
open Types
open Typedtree

module OCamlPath = Path

open DocOckPaths
open DocOckTypes
open DocOckAttrs

module Env = DocOckIdentEnv
module Source = DocOckSource
module Cmti = DocOckCmti
module Cmi = DocOckCmi

let list_opt = function
  | None -> []
  | Some x -> [x]

let cons_opt xo xl =
  match xo with
  | None -> xl
  | Some x -> x :: xl

type 'a definition_allocator =
  { file : string;
    root : 'a;
    name : string;
    allocator : Defn.Index.allocator; }

let add_definition alloc loc defs =
  match Cmti.read_location alloc.file loc with
  | None -> defs, None
  | Some location ->
      let open Defn in
      let open Source.Definition in
      let index = Index.allocate alloc.allocator in
      let definition = { index; location } in
      let root = alloc.root in
      let name = alloc.name in
      let defn = { root; name; index } in
      definition :: defs, Some defn

let parenthesise name =
  match name with
  | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
  | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
    if (String.length name > 0) then
      match name.[0] with
      | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_'
      | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> name
      | _ -> "(" ^ name ^ ")"
    else name

let add_pattern_extra_uses file extra uses =
  match extra with
  | Tpat_constraint typ -> Cmti.add_core_type_uses file typ uses
  | Tpat_type _ -> uses
  | Tpat_open(path, lid, _) -> Cmti.add_module_use file lid path uses
  | Tpat_unpack -> uses

let add_pattern_extras_uses file extras uses =
  List.fold_left
    (fun uses (extra, _, _) -> add_pattern_extra_uses file extra uses)
    uses extras

let rec add_pattern_uses file pat uses =
  let uses = add_pattern_extras_uses file pat.pat_extra uses in
  match pat.pat_desc with
  | Tpat_any | Tpat_var _ | Tpat_constant _ -> uses
  | Tpat_alias(pat, id, str) -> add_pattern_uses file pat uses
  | Tpat_tuple pats -> add_patterns_uses file pats uses
  | Tpat_construct(lid, cstr, pats) ->
      let uses = Cmti.add_constructor_use file lid cstr uses in
      add_patterns_uses file pats uses
  | Tpat_variant(_, pato, _) -> add_pattern_option_uses file pato uses
  | Tpat_record(pats, _) -> add_record_patterns_uses file pats uses
  | Tpat_array pats -> add_patterns_uses file pats uses
  | Tpat_or(pat1, pat2, _) ->
      let uses = add_pattern_uses file pat1 uses in
      add_pattern_uses file pat2 uses
  | Tpat_lazy pat -> add_pattern_uses file pat uses

and add_patterns_uses file pats uses =
  List.fold_left
    (fun uses pat -> add_pattern_uses file pat uses)
    uses pats

and add_pattern_option_uses file pato uses =
  match pato with
  | None -> uses
  | Some pat -> add_pattern_uses file pat uses

and add_record_patterns_uses file pats uses =
  List.fold_left
    (fun uses (lid, lbl, pat) ->
       let uses = Cmti.add_field_use file lid lbl uses in
       add_pattern_uses file pat uses)
      uses pats

let add_expression_extra_uses file extra uses =
  match extra with
  | Texp_constraint typ ->
      Cmti.add_core_type_uses file typ uses
  | Texp_coerce(typ1, typ2) ->
      let uses = Cmti.add_core_type_option_uses file typ1 uses in
      Cmti.add_core_type_uses file typ2 uses
  | Texp_open(_, path, lid, _) ->
      Cmti.add_module_use file lid path uses
  | Texp_poly typo ->
      Cmti.add_core_type_option_uses file typo uses
  | Texp_newtype _ -> uses

let add_expression_extras_uses file extras uses =
  List.fold_left
    (fun uses (extra, _, _) -> add_expression_extra_uses file extra uses)
    uses extras

let rec add_expression_uses file exp uses =
  let uses = add_expression_extras_uses file exp.exp_extra uses in
  match exp.exp_desc with
  | Texp_ident(path, lid, _) ->
      Cmti.add_value_use file lid path uses
  | Texp_constant _ -> uses
  | Texp_let(_, vb, body) ->
      let uses = add_expression_uses file body uses in
      add_value_bindings_uses file vb uses
  | Texp_function { cases; _ } ->
      add_cases_uses file cases uses
  | Texp_apply(exp, args) ->
      let uses = add_expression_uses file exp uses in
      add_args_uses file args uses
  | Texp_match(exp, cases1, cases2, _) ->
      let uses = add_expression_uses file exp uses in
      let uses = add_cases_uses file cases1 uses in
      add_cases_uses file cases2 uses
  | Texp_try(exp, cases) ->
      let uses = add_expression_uses file exp uses in
      add_cases_uses file cases uses
  | Texp_tuple exps ->
      add_expressions_uses file exps uses
  | Texp_construct(lid, desc, args) ->
      let uses = Cmti.add_constructor_use file lid desc uses in
      add_expressions_uses file args uses
  | Texp_variant(_, arg) ->
      add_expression_option_uses file arg uses
  | Texp_record { fields; representation = _; extended_expression; } ->
      let uses = add_expression_option_uses file extended_expression uses in
      add_record_expressions_uses file fields uses
  | Texp_field(exp, lid, lbl) ->
      let uses = Cmti.add_field_use file lid lbl uses in
      add_expression_uses file exp uses
  | Texp_setfield(exp, lid, lbl, set) ->
      let uses = Cmti.add_field_use file lid lbl uses in
      let uses = add_expression_uses file exp uses in
      add_expression_uses file exp uses
  | Texp_array args ->
      add_expressions_uses file args uses
  | Texp_ifthenelse(cond, then_, else_) ->
      let uses = add_expression_uses file cond uses in
      let uses = add_expression_uses file then_ uses in
      add_expression_option_uses file else_ uses
  | Texp_sequence(exp1, exp2) ->
      let uses = add_expression_uses file exp1 uses in
      add_expression_uses file exp2 uses
  | Texp_while(cond, body) ->
      let uses = add_expression_uses file cond uses in
      add_expression_uses file body uses
  | Texp_for(_, _, first, last, _, body) ->
      let uses = add_expression_uses file first uses in
      let uses = add_expression_uses file last uses in
      add_expression_uses file body uses
  | Texp_send(obj, _, _) ->
      add_expression_uses file obj uses
  | Texp_new(p, lid, _) ->
      Cmti.add_class_use file lid p uses
  | Texp_instvar _ -> uses
  | Texp_setinstvar(_, _, _, exp) ->
      add_expression_uses file exp uses
  | Texp_override(_, overrides) ->
      add_override_expressions_uses file overrides uses
  | Texp_letmodule(_, _, mexpr, body) ->
      let uses = add_module_expression_uses file mexpr uses in
      add_expression_uses file body uses
  | Texp_letexception(ext, body) ->
      let uses = Cmti.add_extension_constructor_uses file ext uses in
      add_expression_uses file body uses
  | Texp_assert exp ->
      add_expression_uses file exp uses
  | Texp_lazy exp ->
      add_expression_uses file exp uses
  | Texp_object(cls, _) ->
      add_class_structure_uses file cls uses
  | Texp_pack mexpr ->
      add_module_expression_uses file mexpr uses
  | Texp_unreachable ->
      uses
  | Texp_extension_constructor _ ->
      uses

and add_expressions_uses file exps uses =
  List.fold_left
    (fun uses exp -> add_expression_uses file exp uses)
    uses exps

and add_expression_option_uses file expo uses =
  match expo with
  | None -> uses
  | Some exp -> add_expression_uses file exp uses

and add_args_uses file args uses =
  List.fold_left
    (fun uses (_, expo) -> add_expression_option_uses file expo uses)
    uses args

and add_record_expressions_uses file exps uses =
  Array.fold_left
    (fun uses (lbl, def) ->
       match def with
       | Kept _ -> uses
       | Overridden(lid, exp) ->
           let uses = Cmti.add_field_use file lid lbl uses in
           add_expression_uses file exp uses)
    uses exps

and add_override_expressions_uses file exps uses =
  List.fold_left
    (fun uses (_, _, exp) -> add_expression_uses file exp uses)
    uses exps

and add_value_bindings_uses file vbs uses =
  List.fold_left
    (fun uses vb ->
       let uses = add_pattern_uses file vb.vb_pat uses in
       add_expression_uses file vb.vb_expr uses)
    uses vbs

and add_cases_uses file cases uses =
  List.fold_left
    (fun uses case ->
       let uses = add_pattern_uses file case.c_lhs uses in
       let uses = add_expression_option_uses file case.c_guard uses in
       add_expression_uses file case.c_rhs uses)
    uses cases

and add_class_structure_uses file cstr uses =
  let uses = add_pattern_uses file cstr.cstr_self uses in
  add_class_fields_uses file cstr.cstr_fields uses

and add_class_field_uses file clfd uses =
  match clfd.cf_desc with
  | Tcf_inherit(_, cl, _, _, _) ->
      add_class_expr_uses file cl uses
  | Tcf_val(_, _, _, Tcfk_virtual ctyp, _) ->
      Cmti.add_core_type_uses file ctyp uses
  | Tcf_val(_, _, _, Tcfk_concrete(_, exp), _) ->
      add_expression_uses file exp uses
  | Tcf_method(_, _, Tcfk_virtual ctyp) ->
      Cmti.add_core_type_uses file ctyp uses
  | Tcf_method(_, _, Tcfk_concrete(_, exp)) ->
      add_expression_uses file exp uses
  | Tcf_constraint(ctyp1, ctyp2) ->
      let uses = Cmti.add_core_type_uses file ctyp1 uses in
      Cmti.add_core_type_uses file ctyp2 uses
  | Tcf_initializer exp ->
      add_expression_uses file exp uses
  | Tcf_attribute _ -> uses

and add_class_fields_uses file clfds uses =
  List.fold_left
    (fun uses clfd -> add_class_field_uses file clfd uses)
    uses clfds

and add_class_expr_uses file cl uses =
  match cl.cl_desc with
  | Tcl_ident(path, lid, ctyps) ->
      let uses = Cmti.add_class_use file lid path uses in
      Cmti.add_core_types_uses file ctyps uses
  | Tcl_structure cstr ->
      add_class_structure_uses file cstr uses
  | Tcl_fun(_, pat, _, cl, _) ->
      let uses = add_pattern_uses file pat uses in
      add_class_expr_uses file cl uses
  | Tcl_apply(cl, args) ->
      let uses = add_class_expr_uses file cl uses in
      add_args_uses file args uses
  | Tcl_let(_, vbs, _, cl) ->
      let uses = add_value_bindings_uses file vbs uses in
      add_class_expr_uses file cl uses
  | Tcl_constraint(cl, clto, _, _, _) ->
      let uses = add_class_expr_uses file cl uses in
      Cmti.add_class_type_option_uses file clto uses
  | Tcl_open(_, path, lid, _, cl) ->
      let uses = Cmti.add_module_use file lid path uses in
      add_class_expr_uses file cl uses

and add_class_declaration_uses file cd uses =
  let uses = Cmti.add_type_params_uses file cd.ci_params uses in
  add_class_expr_uses file cd.ci_expr uses

and add_class_declarations_uses file cds uses =
  List.fold_left
    (fun uses (cd, _) -> add_class_declaration_uses file cd uses)
    uses cds

and add_class_type_declarations_uses file cltds uses =
  List.fold_left
    (fun uses (_, _, cltd) ->
       Cmti.add_class_type_declaration_uses file cltd uses)
    uses cltds

and add_module_expression_uses file mexpr uses =
  match mexpr.mod_desc with
  | Tmod_ident(path, lid) -> Cmti.add_module_use file lid path uses
  | Tmod_structure str -> add_structure_uses file str uses
  | Tmod_functor(_, _, mtypo, mexpr) ->
    let uses = Cmti.add_module_type_option_uses file mtypo uses in
    add_module_expression_uses file mexpr uses
  | Tmod_apply(func, arg, _) ->
    let uses = add_module_expression_uses file func uses in
    add_module_expression_uses file arg uses
  | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
    add_module_expression_uses file mexpr uses
  | Tmod_constraint(mexpr, _, Tmodtype_explicit mtyp, _) ->
    let uses = Cmti.add_module_type_uses file mtyp uses in
    add_module_expression_uses file mexpr uses
  | Tmod_unpack(exp, _) ->
    add_expression_uses file exp uses

and add_structure_item_uses file item uses =
  match item.str_desc with
  | Tstr_eval(exp, _) -> add_expression_uses file exp uses
  | Tstr_value(_, vbs) -> add_value_bindings_uses file vbs uses
  | Tstr_primitive vd ->
      Cmti.add_value_description_uses file vd uses
  | Tstr_type(_, decls) ->
      Cmti.add_type_declarations_uses file decls uses
  | Tstr_typext ext ->
      Cmti.add_type_extension_uses file ext uses
  | Tstr_exception exn ->
      Cmti.add_extension_constructor_uses file exn uses
  | Tstr_module mb ->
      add_module_binding_uses file mb uses
  | Tstr_recmodule mbs ->
      add_module_bindings_uses file mbs uses
  | Tstr_modtype mtd ->
      Cmti.add_module_type_declaration_uses file mtd uses
  | Tstr_open opn ->
      Cmti.add_open_description_uses file opn uses
  | Tstr_class clds ->
      add_class_declarations_uses file clds uses
  | Tstr_class_type cltds ->
      add_class_type_declarations_uses file cltds uses
  | Tstr_include incl ->
      add_include_declaration_uses file incl uses
  | Tstr_attribute _ -> uses

and add_structure_uses file str uses =
  List.fold_left
    (fun uses item -> add_structure_item_uses file item uses)
    uses str.str_items

and add_module_binding_uses file mb uses =
  add_module_expression_uses file mb.mb_expr uses

and add_module_bindings_uses file mbs uses =
  List.fold_left
    (fun uses mb -> add_module_binding_uses file mb uses)
    uses mbs

and add_include_declaration_uses file incl uses =
  add_module_expression_uses file incl.incl_mod uses

let add_module_def alloc id name map defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_module
          def_map (Ident.name id) defn map
      in
      defs, def_map

let add_module_type_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_module_type
          def_map (Ident.name id) defn
      in
      defs, def_map

let add_type_def alloc id name map defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_type
          def_map (Ident.name id) defn map
      in
      defs, def_map

let add_extension_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_extension
          def_map (Ident.name id) defn
      in
      defs, def_map

let add_value_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_value
          def_map (Ident.name id) defn
      in
      defs, def_map

let add_class_def alloc id name map defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_class
          def_map (Ident.name id) defn map
      in
      defs, def_map

let add_class_type_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Module.add_class_type
          def_map (Ident.name id) defn
      in
      defs, def_map

let add_method_def alloc name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Class.add_method
          def_map name.txt defn
      in
      defs, def_map

let add_instance_variable_def alloc name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Class.add_instance_variable
          def_map name.txt defn
      in
      defs, def_map

let add_constructor_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Type.add_constructor
          def_map (Ident.name id) defn
      in
      defs, def_map

let add_field_def alloc id name defs def_map =
  let defs, defn = add_definition alloc name.loc defs in
  match defn with
  | None -> defs, def_map
  | Some defn ->
      let def_map =
        Source.Defn_map.Type.add_field
          def_map (Ident.name id) defn
      in
      defs, def_map

let remove_include_defs sg def_map =
  List.fold_left
    (fun def_map item ->
       let open Source.Defn_map.Module in
       match item with
       | Sig_value(id, _) -> remove_value def_map (Ident.name id)
       | Sig_type(id, _, _) -> remove_type def_map (Ident.name id)
       | Sig_typext(id, _, _) -> remove_extension def_map (Ident.name id)
       | Sig_module(id, _, _) -> remove_module def_map (Ident.name id)
       | Sig_modtype(id, _) -> remove_module_type def_map (Ident.name id)
       | Sig_class(id, _, _) -> remove_class def_map (Ident.name id)
       | Sig_class_type(id, _, _) -> remove_class_type def_map (Ident.name id))
    def_map sg

let rec add_pattern_defs_and_uses alloc pat defs uses def_map  =
  let uses = add_pattern_extras_uses alloc.file pat.pat_extra uses in
  match pat.pat_desc with
  | Tpat_any | Tpat_constant _ -> defs, uses, def_map
  | Tpat_var(id, name) ->
      let defs, def_map = add_value_def alloc id name defs def_map in
      defs, uses, def_map
  | Tpat_alias(pat, id, name) ->
      let defs, uses, def_map =
        add_pattern_defs_and_uses alloc pat defs uses def_map
      in
      let defs, def_map = add_value_def alloc id name defs def_map in
      defs, uses, def_map
  | Tpat_tuple pats ->
      add_patterns_defs_and_uses alloc pats defs uses def_map
  | Tpat_construct(lid, cstr, pats) ->
      let uses = Cmti.add_constructor_use alloc.file lid cstr uses in
      add_patterns_defs_and_uses alloc pats defs uses def_map
  | Tpat_variant(_, pato, _) ->
      add_pattern_option_defs_and_uses alloc pato defs uses def_map
  | Tpat_record(pats, _) ->
      add_record_patterns_defs_and_uses alloc pats defs uses def_map
  | Tpat_array pats ->
      add_patterns_defs_and_uses alloc pats defs uses def_map
  | Tpat_or(pat1, pat2, _) ->
      let defs, uses, def_map =
        add_pattern_defs_and_uses alloc pat1 defs uses def_map
      in
      let uses = add_pattern_uses alloc.file pat2 uses in
      defs, uses, def_map
  | Tpat_lazy pat ->
      add_pattern_defs_and_uses alloc pat defs uses def_map

and add_patterns_defs_and_uses alloc pats defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) pat ->
       add_pattern_defs_and_uses alloc pat defs uses def_map)
    (defs, uses, def_map) pats

and add_pattern_option_defs_and_uses alloc pato defs uses def_map =
  match pato with
  | None -> defs, uses, def_map
  | Some pat -> add_pattern_defs_and_uses alloc pat defs uses def_map

and add_record_patterns_defs_and_uses alloc pats defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) (lid, lbl, pat) ->
       let uses = Cmti.add_field_use alloc.file lid lbl uses in
       add_pattern_defs_and_uses alloc pat defs uses def_map)
      (defs, uses, def_map) pats

let add_value_bindings_defs_and_uses alloc vbs defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) vb ->
       let defs, uses, def_map =
         add_pattern_defs_and_uses alloc vb.vb_pat defs uses def_map
       in
       let uses = add_expression_uses alloc.file vb.vb_expr uses in
       defs, uses, def_map)
    (defs, uses, def_map) vbs

let add_value_description_defs_and_uses alloc vd defs uses def_map =
  let uses = Cmti.add_core_type_uses alloc.file vd.val_desc uses in
  let def, def_map = add_value_def alloc vd.val_id vd.val_name defs def_map in
  defs, uses, def_map

let add_constructor_declaration_defs_and_uses alloc cstr defs uses def_map =
  let uses =
    Cmti.add_constructor_arguments_uses alloc.file cstr.cd_args uses
  in
  let uses =
    Cmti.add_core_type_option_uses alloc.file cstr.cd_res uses
  in
  let defs, def_map =
    add_constructor_def alloc cstr.cd_id cstr.cd_name defs def_map
  in
  defs, uses, def_map

let add_constructor_declarations_defs_and_uses alloc cstrs defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) cstr ->
       add_constructor_declaration_defs_and_uses alloc cstr defs uses def_map)
    (defs, uses, def_map) cstrs

let add_label_declaration_defs_and_uses alloc lbl defs uses def_map =
  let uses =
    Cmti.add_core_type_uses alloc.file lbl.ld_type uses
  in
  let defs, def_map =
    add_field_def alloc lbl.ld_id lbl.ld_name defs def_map
  in
  defs, uses, def_map

let add_label_declarations_defs_and_uses alloc lbls defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) lbl ->
       add_label_declaration_defs_and_uses alloc lbl defs uses def_map)
    (defs, uses, def_map) lbls

let add_type_kind_defs_and_uses alloc knd defs uses def_map =
  match knd with
  | Ttype_abstract -> defs, uses, def_map
  | Ttype_variant cstrs ->
      add_constructor_declarations_defs_and_uses alloc cstrs defs uses def_map
  | Ttype_record lbls ->
      add_label_declarations_defs_and_uses alloc lbls defs uses def_map
  | Ttype_open -> defs, uses, def_map

let add_type_declaration_defs_and_uses alloc decl defs uses def_map =
  let uses =
    Cmti.add_type_params_uses alloc.file decl.typ_params uses
  in
  let uses =
    Cmti.add_core_type_option_uses alloc.file decl.typ_manifest uses
  in
  let child_map = Source.Defn_map.Type.empty in
  let defs, uses, child_map =
    add_type_kind_defs_and_uses alloc decl.typ_kind defs uses child_map
  in
  let uses =
    Cmti.add_type_constraints_uses alloc.file decl.typ_cstrs uses
  in
  let defs, def_map =
    add_type_def alloc decl.typ_id decl.typ_name child_map defs def_map
  in
  defs, uses, def_map

let add_type_declarations_defs_and_uses alloc decls defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) decl ->
       add_type_declaration_defs_and_uses alloc decl defs uses def_map)
    (defs, uses, def_map) decls

let add_extension_constructor_defs_and_uses alloc ext defs uses def_map =
  let uses =
    Cmti.add_extension_constructor_kind_uses alloc.file ext.ext_kind uses
  in
  let defs, def_map =
    add_extension_def alloc ext.ext_id ext.ext_name defs def_map
  in
  defs, uses, def_map

let add_extension_constructors_defs_and_uses alloc exts defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) ext ->
       add_extension_constructor_defs_and_uses alloc ext defs uses def_map)
    (defs, uses, def_map) exts

let add_type_extension_defs_and_uses alloc tyext defs uses def_map =
  let uses =
    Cmti.add_type_params_uses alloc.file tyext.tyext_params uses
  in
  let uses =
    Cmti.add_type_use alloc.file tyext.tyext_txt tyext.tyext_path uses
  in
  add_extension_constructors_defs_and_uses alloc
    tyext.tyext_constructors defs uses def_map

let rec add_class_structure_defs_and_uses alloc cstr defs uses def_map =
  let uses = add_pattern_uses alloc.file cstr.cstr_self uses in
  add_class_fields_defs_and_uses alloc cstr.cstr_fields defs uses def_map

and add_class_field_defs_and_uses alloc clfd defs uses def_map =
  match clfd.cf_desc with
  | Tcf_inherit(_, cl, _, _, _) ->
      add_class_expr_defs_and_uses alloc cl defs uses def_map
  | Tcf_val(_, _, _, Tcfk_virtual ctyp, _) ->
      let uses = Cmti.add_core_type_uses alloc.file ctyp uses in
      defs, uses, def_map
  | Tcf_val(name, _, _, Tcfk_concrete(_, exp), _) ->
      let defs, def_map = add_instance_variable_def alloc name defs def_map in
      let uses = add_expression_uses alloc.file exp uses in
      defs, uses, def_map
  | Tcf_method(_, _, Tcfk_virtual ctyp) ->
      let uses = Cmti.add_core_type_uses alloc.file ctyp uses in
      defs, uses, def_map
  | Tcf_method(name, _, Tcfk_concrete(_, exp)) ->
      let defs, def_map = add_method_def alloc name defs def_map in
      let uses = add_expression_uses alloc.file exp uses in
      defs, uses, def_map
  | Tcf_constraint(ctyp1, ctyp2) ->
      let uses = Cmti.add_core_type_uses alloc.file ctyp1 uses in
      let uses = Cmti.add_core_type_uses alloc.file ctyp2 uses in
      defs, uses, def_map
  | Tcf_initializer exp ->
      let uses = add_expression_uses alloc.file exp uses in
      defs, uses, def_map
  | Tcf_attribute _ -> defs, uses, def_map

and add_class_fields_defs_and_uses alloc clfds defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) clfd ->
       add_class_field_defs_and_uses alloc clfd defs uses def_map)
    (defs, uses, def_map) clfds

and add_class_expr_defs_and_uses alloc cl defs uses def_map =
  match cl.cl_desc with
  | Tcl_ident(path, lid, ctyps) ->
      let uses = Cmti.add_class_use alloc.file lid path uses in
      let uses = Cmti.add_core_types_uses alloc.file ctyps uses in
      defs, uses, def_map
  | Tcl_structure cstr ->
      add_class_structure_defs_and_uses alloc cstr defs uses def_map
  | Tcl_fun(_, pat, _, cl, _) ->
      let uses = add_pattern_uses alloc.file pat uses in
      add_class_expr_defs_and_uses alloc cl defs uses def_map
  | Tcl_apply(cl, args) ->
      let defs, uses, def_map =
        add_class_expr_defs_and_uses alloc cl defs uses def_map
      in
      let uses = add_args_uses alloc.file args uses in
      defs, uses, def_map
  | Tcl_let(_, vbs, _, cl) ->
      let uses = add_value_bindings_uses alloc.file vbs uses in
      add_class_expr_defs_and_uses alloc cl defs uses def_map
  | Tcl_constraint(cl, clto, _, _, _) ->
      let defs, uses, def_map =
        add_class_expr_defs_and_uses alloc cl defs uses def_map
      in
      let uses = Cmti.add_class_type_option_uses alloc.file clto uses in
      defs, uses, def_map
  | Tcl_open(_, path, lid, _, cl) ->
      let uses = Cmti.add_module_use alloc.file lid path uses in
      add_class_expr_defs_and_uses alloc cl defs uses def_map

let add_class_declaration_defs_and_uses alloc cd defs uses def_map =
  let uses = Cmti.add_type_params_uses alloc.file cd.ci_params uses in
  let child_map = Source.Defn_map.Class.empty in
  let defs, uses, child_map =
    add_class_expr_defs_and_uses alloc cd.ci_expr defs uses child_map
  in
  let defs, def_map =
    add_class_def alloc cd.ci_id_class cd.ci_id_name child_map defs def_map
  in
  defs, uses, def_map

let add_class_type_declaration_defs_and_uses alloc cltd defs uses def_map =
  let uses = Cmti.add_type_params_uses alloc.file cltd.ci_params uses in
  let uses = Cmti.add_class_type_uses alloc.file cltd.ci_expr uses in
  let defs, def_map =
    add_class_type_def alloc cltd.ci_id_class_type cltd.ci_id_name defs def_map
  in
  defs, uses, def_map

let add_class_declarations_defs_and_uses alloc cds defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) (cd, _) ->
       add_class_declaration_defs_and_uses alloc cd defs uses def_map)
    (defs, uses, def_map) cds

let add_class_type_declarations_defs_and_uses alloc cltds defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) (_, _, cltd) ->
       add_class_type_declaration_defs_and_uses alloc cltd defs uses def_map)
    (defs, uses, def_map) cltds

let rec add_module_expression_defs_and_uses alloc mexpr defs uses def_map =
  match mexpr.mod_desc with
  | Tmod_ident(path, lid) ->
      let uses = Cmti.add_module_use alloc.file lid path uses in
      defs, uses, def_map
  | Tmod_structure str ->
      add_structure_defs_and_uses alloc str defs uses def_map
  | Tmod_functor(_, _, mtypo, mexpr) ->
      let uses =
        Cmti.add_module_type_option_uses alloc.file mtypo uses
      in
      add_module_expression_defs_and_uses alloc mexpr defs uses def_map
  | Tmod_apply(func, arg, _) ->
      let uses = add_module_expression_uses alloc.file func uses in
      let uses = add_module_expression_uses alloc.file arg uses in
      defs, uses, def_map
  | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      add_module_expression_defs_and_uses alloc mexpr defs uses def_map
  | Tmod_constraint(mexpr, _, Tmodtype_explicit mtyp, _) ->
      let uses = Cmti.add_module_type_uses alloc.file mtyp uses in
      add_module_expression_defs_and_uses alloc mexpr defs uses def_map
  | Tmod_unpack(exp, _) ->
      let uses = add_expression_uses alloc.file exp uses in
      defs, uses, def_map

and add_structure_item_defs_and_uses alloc item defs uses def_map =
  match item.str_desc with
  | Tstr_eval(exp, _) ->
      let uses = add_expression_uses alloc.file exp uses in
      defs, uses, def_map
  | Tstr_value(_, vbs) ->
      add_value_bindings_defs_and_uses alloc vbs defs uses def_map
  | Tstr_primitive vd ->
      add_value_description_defs_and_uses alloc vd defs uses def_map
  | Tstr_type(_, decls) ->
      add_type_declarations_defs_and_uses alloc decls defs uses def_map
  | Tstr_typext ext ->
      add_type_extension_defs_and_uses alloc ext defs uses def_map
  | Tstr_exception exn ->
      add_extension_constructor_defs_and_uses alloc exn defs uses def_map
  | Tstr_module mb ->
      add_module_binding_defs_and_uses alloc mb defs uses def_map
  | Tstr_recmodule mbs ->
      add_module_bindings_defs_and_uses alloc mbs defs uses def_map
  | Tstr_modtype mtd ->
      add_module_type_declaration_defs_and_uses alloc mtd defs uses def_map
  | Tstr_open opn ->
      let uses = Cmti.add_open_description_uses alloc.file opn uses in
      defs, uses, def_map
  | Tstr_class clds ->
      add_class_declarations_defs_and_uses alloc clds defs uses def_map
  | Tstr_class_type cltds ->
      add_class_type_declarations_defs_and_uses alloc cltds defs uses def_map
  | Tstr_include incl ->
      add_include_declaration_defs_and_uses alloc incl defs uses def_map
  | Tstr_attribute _ -> defs, uses, def_map

and add_structure_defs_and_uses alloc str defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) item ->
       add_structure_item_defs_and_uses alloc item defs uses def_map)
    (defs, uses, def_map) str.str_items

and add_module_binding_defs_and_uses alloc mb defs uses def_map =
  let child_map = Source.Defn_map.Module.empty in
  let defs, uses, child_map =
    add_module_expression_defs_and_uses alloc mb.mb_expr defs uses child_map
  in
  let defs, def_map =
    add_module_def alloc mb.mb_id mb.mb_name child_map defs def_map
  in
  defs, uses, def_map

and add_module_bindings_defs_and_uses alloc mbs defs uses def_map =
  List.fold_left
    (fun (defs, uses, def_map) mb ->
       add_module_binding_defs_and_uses alloc mb defs uses def_map)
    (defs, uses, def_map) mbs

and add_module_type_declaration_defs_and_uses alloc mtd defs uses def_map =
  let uses =
    Cmti.add_module_type_option_uses alloc.file mtd.mtd_type uses
  in
  let defs, def_map =
    add_module_type_def alloc mtd.mtd_id mtd.mtd_name defs def_map
  in
  defs, uses, def_map

and add_include_declaration_defs_and_uses alloc incl defs uses def_map =
  let uses = add_module_expression_uses alloc.file incl.incl_mod uses in
  let def_map = remove_include_defs incl.incl_type def_map in
  defs, uses, def_map

let read_implementation_defs_and_uses ~root ~name ~file impl =
  let allocator = Defn.Index.allocator () in
  let alloc = { file; root; name; allocator; } in
  let def_map = Source.Defn_map.Module.empty in
  add_structure_defs_and_uses alloc impl [] [] def_map

let read_core_type env file ctyp uses =
  let uses = Cmti.add_core_type_uses file ctyp uses in
  let cty = Cmi.read_type_expr env ctyp.ctyp_type in
  cty, uses

let rec read_pattern env parent alloc doc pat items defs uses =
  let open Signature in
    let uses = add_pattern_extras_uses alloc.file pat.pat_extra uses in
    match pat.pat_desc with
    | Tpat_any -> items, defs, uses
    | Tpat_var(id, str) ->
        let open Value in
        let name = parenthesise (Ident.name id) in
        let id = Identifier.Value(parent, name) in
        Cmi.mark_type_expr pat.pat_type;
        let type_ = Cmi.read_type_expr env pat.pat_type in
        let defs, defn = add_definition alloc str.loc defs in
        let decl = None in
        let item = Value {id; doc; decl; defn; type_} in
        item :: items, defs, uses
    | Tpat_alias(pat, id, str) ->
        let open Value in
        let name = parenthesise (Ident.name id) in
        let id = Identifier.Value(parent, name) in
        Cmi.mark_type_expr pat.pat_type;
        let type_ = Cmi.read_type_expr env pat.pat_type in
        let defs, defn = add_definition alloc str.loc defs in
        let decl = None in
        let item = Value {id; doc; decl; defn; type_} in
        read_pattern env parent alloc doc pat (item::items) defs uses
    | Tpat_constant _ -> items, defs, uses
    | Tpat_tuple pats ->
        read_patterns env parent alloc doc pats items defs uses
    | Tpat_construct(lid, cstr, pats) ->
        let uses = Cmti.add_constructor_use alloc.file lid cstr uses in
        read_patterns env parent alloc doc pats items defs uses
    | Tpat_variant(_, pato, _) ->
        read_pattern_option env parent alloc doc pato items defs uses
    | Tpat_record(pats, _) ->
        read_record_patterns env parent alloc doc pats items defs uses
    | Tpat_array pats ->
        read_patterns env parent alloc doc pats items defs uses
    | Tpat_or(pat1, pat2, _) ->
        let uses = add_pattern_uses alloc.file pat2 uses in
        read_pattern env parent alloc doc pat1 items defs uses
    | Tpat_lazy pat ->
        read_pattern env parent alloc doc pat items defs uses

and read_patterns env parent alloc doc pats items defs uses =
  List.fold_left
    (fun (items, defs, uses) pat ->
       read_pattern env parent alloc doc pat items defs uses)
    (items, defs, uses) pats

and read_pattern_option env parent alloc doc pato items defs uses =
  match pato with
  | None -> items, defs, uses
  | Some pat -> read_pattern env parent alloc doc pat items defs uses

and read_record_patterns env parent alloc doc pats items defs uses =
  List.fold_left
      (fun (items, defs, uses) (lid, lbl, pat) ->
         let uses = Cmti.add_field_use alloc.file lid lbl uses in
         read_pattern env parent alloc doc pat items defs uses)
      (items, defs, uses) pats

let read_value_binding env parent alloc vb items defs uses =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let doc = read_attributes container parent vb.vb_attributes in
  let items, defs, uses =
    read_pattern env parent alloc doc vb.vb_pat items defs uses
  in
  let uses = add_expression_uses alloc.file vb.vb_expr uses in
  items, defs, uses

let read_value_bindings env parent alloc vbs items defs uses =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  List.fold_left
    (fun (items, defs, uses) vb ->
       let open Signature in
       let comments = read_comments container vb.vb_attributes in
       let items =
         List.fold_left
           (fun items com -> Comment com :: items)
           items comments
       in
       read_value_binding env parent alloc vb items defs uses)
    (items, defs, uses) vbs

let read_type_declarations env parent alloc decls items defs uses =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  List.fold_left
      (fun (items, defs, uses) decl ->
         let open Signature in
         let comments = read_comments container decl.typ_attributes in
         let comments =
           List.fold_left
             (fun items com -> Comment com :: items)
             items comments
         in
         let defs, defn = add_definition alloc decl.typ_name.loc defs in
         let items, uses =
           Cmti.read_type_declaration env parent defn decl items uses
         in
         items, defs, uses)
      (items, defs, uses) decls

let read_type_extension env parent alloc tyext items defs uses =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let doc = read_attributes container parent tyext.tyext_attributes in
  let type_params =
    List.map (fun (ctyp, _) -> ctyp.ctyp_type) tyext.tyext_params
  in
  let constructors =
    List.map (fun ext -> ext.ext_type) tyext.tyext_constructors
  in
  let type_params =
    Cmi.mark_type_extension type_params constructors
  in
  let type_params =
    List.map
      (Cmi.read_type_parameter false Variance.null)
      type_params
  in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map
      (fun ext ->
         Cmi.read_extension_constructor
           env parent ext.ext_id ext.ext_type)
      tyext.tyext_constructors
  in
    { type_path; doc; type_params; private_; constructors; }

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_class_signature parent)
  in
  let doc = read_attributes container parent ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let name = parenthesise name in
      let id = Identifier.InstanceVariable(parent, name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let name = parenthesise name in
      let id = Identifier.Method(parent, name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(_, _) -> None
  | Tctf_inherit cltyp ->
      Some (Inherit (read_class_signature env parent [] cltyp))
  | Tctf_attribute attr ->
      match read_comment container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_signature env parent params cltyp =
  let open ClassType in
    match cltyp.cltyp_desc with
    | Tcty_constr(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env) params in
          Constr(p, params)
    | Tcty_signature csig ->
        let open ClassSignature in
        let self =
          Cmi.read_self_type csig.csig_self.ctyp_type
        in
        let constraints =
          Cmi.read_type_constraints env params
        in
        let constraints =
          List.map
            (fun (typ1, typ2) -> Constraint(typ1, typ2))
            constraints
        in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = constraints @ List.rev items in
          Signature {self; items}
    | Tcty_arrow _ -> assert false
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
    | Tcty_open _ -> assert false
#endif

let read_class_type_declarations env parent cltds =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let items =
    List.fold_left
      (fun acc (_, _, cltd) ->
         let open Signature in
         let comments = read_comments container cltd.ci_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let cltd = Cmti.read_class_type_declaration env parent cltd in
           (ClassType cltd) :: (List.rev_append comments acc))
      [] cltds
  in
    List.rev items

let rec read_class_type env parent params cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
      ClassType (read_class_signature env parent params cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = Cmi.read_label lbl in
      let arg = read_core_type env arg in
      let res = read_class_type env parent params res in
        Arrow(lbl, arg, res)
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
  | Tcty_open (_, _, _, _, cty) -> read_class_type env parent params cty
#endif

let rec read_class_field env parent cf =
  let open ClassSignature in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_class_signature parent)
  in
  let doc = read_attributes container parent (cf.cf_attributes) in
  match cf.cf_desc with
  | Tcf_val({txt = name; _}, mutable_, _, kind, _) ->
      let open InstanceVariable in
      let name = parenthesise name in
      let id = Identifier.InstanceVariable(parent, name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            false, Cmi.read_type_expr env expr.exp_type
      in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tcf_method({txt = name; _}, private_, kind) ->
      let open Method in
      let name = parenthesise name in
      let id = Identifier.Method(parent, name) in
      let private_ = (private_ = Private) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            false, Cmi.read_type_expr env expr.exp_type
      in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tcf_constraint(_, _) -> None
  | Tcf_inherit(_, cl, _, _, _) ->
      Some (Inherit (read_class_structure env parent [] cl))
  | Tcf_initializer _ -> None
  | Tcf_attribute attr ->
      match read_comment container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_structure env parent params cl =
  let open ClassType in
    match cl.cl_desc with
    | Tcl_ident _ | Tcl_apply _ ->
        Cmi.read_class_signature env parent params cl.cl_type
    | Tcl_structure cstr ->
        let open ClassSignature in
        let self = Cmi.read_self_type cstr.cstr_self.pat_type in
        let constraints =
          Cmi.read_type_constraints env params
        in
        let constraints =
          List.map
            (fun (typ1, typ2) -> Constraint(typ1, typ2))
            constraints
        in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] cstr.cstr_fields
        in
        let items = constraints @ List.rev items in
          Signature {self; items}
    | Tcl_fun _ -> assert false
    | Tcl_let(_, _, _, cl) -> read_class_structure env parent params cl
    | Tcl_constraint(cl, None, _, _, _) -> read_class_structure env parent params cl
    | Tcl_constraint(_, Some cltyp, _, _, _) ->
        read_class_signature env parent params cltyp
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
    | Tcl_open (_, _, _, _, cl) -> read_class_structure env parent params cl
#endif

let rec read_class_expr env parent params cl =
  let open Class in
  match cl.cl_desc with
  | Tcl_ident _ | Tcl_apply _ ->
      Cmi.read_class_type env parent params cl.cl_type
  | Tcl_structure _ ->
      ClassType (read_class_structure env parent params cl)
  | Tcl_fun(lbl, arg, _, res, _) ->
      let lbl = Cmi.read_label lbl in
      let arg = Cmi.read_type_expr env arg.pat_type in
      let res = read_class_expr env parent params res in
        Arrow(lbl, arg, res)
  | Tcl_let(_, _, _, cl) ->
      read_class_expr env parent params cl
  | Tcl_constraint(cl, None, _, _, _) ->
      read_class_expr env parent params cl
  | Tcl_constraint(_, Some cltyp, _, _, _) ->
      read_class_type env parent params cltyp
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 06
    | Tcl_open (_, _, _, _, cl) -> read_class_expr env parent params cl
#endif

let read_class_declaration env parent cld =
  let open Class in
  let name = parenthesise (Ident.name cld.ci_id_class) in
  let id = Identifier.Class(parent, name) in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let doc = read_attributes container id cld.ci_attributes in
    Cmi.mark_class_declaration cld.ci_decl;
    let virtual_ = (cld.ci_virt = Virtual) in
    let clparams =
      List.map (fun (ctyp, _) -> ctyp.ctyp_type) cld.ci_params
    in
    let params =
      List.map
        (Cmi.read_type_parameter false Variance.null)
        clparams
    in
    let type_ = read_class_expr env id clparams cld.ci_expr in
      { id; doc; virtual_; params; type_; expansion = None }

let read_class_declarations env parent clds =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let items =
    List.fold_left
      (fun acc (cld, _) ->
         let open Signature in
         let comments = read_comments container cld.ci_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let cld = read_class_declaration env parent cld in
           (Class cld) :: (List.rev_append comments acc))
      [] clds
  in
    List.rev items

let rec read_module_expr env parent pos mexpr =
  let open ModuleType in
    match mexpr.mod_desc with
    | Tmod_ident _ ->
        Cmi.read_module_type env parent pos mexpr.mod_type
    | Tmod_structure str -> Signature (read_structure env parent str)
    | Tmod_functor(id, _, arg, res) ->
        let arg =
          match arg with
          | None -> None
          | Some arg ->
              let name = parenthesise (Ident.name id) in
              let id = Identifier.Argument(parent, pos, name) in
              let arg = Cmti.read_module_type env id 1 arg in
              let expansion =
                match arg with
                | Signature _ -> Some Module.AlreadyASig
                | _ -> None
              in
                Some { FunctorArgument. id; expr = arg; expansion }
        in
        let env = Env.add_argument parent pos id env in
        let res = read_module_expr env parent (pos + 1) res in
          Functor(arg, res)
    | Tmod_apply _ ->
        Cmi.read_module_type env parent pos mexpr.mod_type
    | Tmod_constraint(_, _, Tmodtype_explicit mty, _) ->
        Cmti.read_module_type env parent pos mty
    | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
        read_module_expr env parent pos mexpr
    | Tmod_unpack(_, mty) ->
        Cmi.read_module_type env parent pos mty

and unwrap_module_expr_desc = function
  | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
  | desc -> desc

and read_module_binding env parent mb =
  let open Module in
  let name = parenthesise (Ident.name mb.mb_id) in
  let id = Identifier.Module(parent, name) in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let doc = read_attributes container id mb.mb_attributes in
  let canonical =
    let open Documentation in
    match doc with
    | Ok { tags; _ } ->
      begin match List.find (function Canonical _ -> true | _ -> false) tags with
      | exception Not_found -> None
      | Canonical(p, r) -> Some (p, r)
      | _ -> None
      end
    | _ -> None
  in
  let type_ =
    match unwrap_module_expr_desc mb.mb_expr.mod_desc with
    | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_expr env id 1 mb.mb_expr)
  in
  let hidden =
    match canonical with
    | Some _ -> false
    | None -> contains_double_underscore (Ident.name mb.mb_id)
  in
  let expansion =
    match type_ with
    | ModuleType (ModuleType.Signature _) -> Some AlreadyASig
    | _ -> None
  in
    {id; doc; type_; expansion; canonical; hidden; display_type = None}

and read_module_bindings env parent mbs =
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let items =
    List.fold_left
      (fun acc mb ->
         let open Signature in
         let comments = read_comments container mb.mb_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let mb = read_module_binding env parent mb in
           (Module mb) :: (List.rev_append comments acc))
      [] mbs
  in
    List.rev items

and read_structure_item env parent alloc item =
  let open Signature in
    match item.str_desc with
    | Tstr_eval(expr, _) ->
        let uses = read_expression_uses env expr in
        [], uses, []
    | Tstr_value(_, vbs) ->
        read_value_bindings env parent alloc vbs
    | Tstr_primitive vd ->
        let definitions, defn = allocate_definition alloc vd.val_name.loc in
        let item, uses = Cmti.read_value_description env parent defn vd in
        [item], uses, definitions
    | Tstr_type (_rec_flag, tdecls) -> (* TODO: handle rec_flag *)
        read_type_definitions env parent alloc tdecls
    | Tstr_typext tyext ->
        let tyext, uses, definitions =
          read_type_extension env parent alloc tyext
        in
        [TypExt tyext], uses, definitions
    | Tstr_exception ext ->
        let definitions, defn = allocate_definition alloc ext.ext_name.loc in
        let ext, uses =
          Cmi.read_exception env parent defn ext.ext_id ext.ext_type
        in
        [Exception ext], uses, definitions
    | Tstr_module mb ->
        let md, uses, definitions = read_module_binding env parent alloc mb in
        [Module md], uses, definitions
    | Tstr_recmodule mbs ->
        read_module_bindings env parent alloc mbs
    | Tstr_modtype mtd ->
        let definitions, defn = allocate_definition alloc mtd.mtd_name.loc in
        let mtd, uses =
          Cmti.read_module_type_definition env parent defn mtd
        in
        [ModuleType mtd], uses, definitions
    | Tstr_open opn ->
        let uses = read_module_path_uses opn.open_txt.loc opn.open_path in
        [], uses, []
    | Tstr_include incl ->
        let incl, uses = read_include env parent incl in
        [Include incl], uses, []
    | Tstr_class cls ->
        read_class_definitions env parent cls
    | Tstr_class_type cltyps ->
        read_class_type_definitions env parent cltyps
    | Tstr_attribute attr ->
        let items =
          let container =
            Identifier.label_parent_of_parent
              (Identifier.parent_of_signature parent)
          in
          match read_comment container attr with
          | None -> []
          | Some doc -> [Comment doc]
        in
        items, [], []

and read_include env parent incl =
  let open Include in
  let container =
    Identifier.label_parent_of_parent (Identifier.parent_of_signature parent)
  in
  let doc = read_attributes container parent incl.incl_attributes in
  let mtype, uses = read_module_expr env parent 1 incl.incl_mod in
  let expr =
    let open Module in
    match unwrap_module_expr_desc incl.incl_mod.mod_desc with
    | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType mtype
  in
  let content = Cmi.read_signature env parent incl.incl_type in
  let expansion = { content; resolved = false } in
    {parent; doc; expr; expansion}, uses

and read_structure env parent alloc str =
  let env = Env.add_structure_tree_items parent str env in
  let items, uses, definitions =
    List.fold_left
      (fun (items, uses, definitions) item ->
         let new_items, new_uses, new_definitions =
           read_structure_item env parent alloc item
         in
         let items = List.rev_append new_items items in
         let uses = List.rev_append new_uses uses in
         let definitions = List.rev_append new_definitions definitions in
         items, uses, definitions)
      ([], [], []) str.str_items
  in
  List.rev items, List.rev uses, List.rev definitions

let read_implementation root name file impl =
  let id = Identifier.Root(root, name) in
  let allocator = Defn.Index.allocator () in
  let alloc = { file; root; name; allocator; } in
  let items, uses, definitions = read_structure Env.empty id alloc impl in
  let doc, items =
    let open Signature in
    let open Documentation in
    match items with
    | Comment (Documentation doc) :: items -> doc, items
    | _ -> empty, items
  in
    (id, doc, items, uses, definitionss)
