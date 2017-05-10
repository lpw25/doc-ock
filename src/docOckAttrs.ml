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

open Octavius.Types
open DocOckNames
open DocOckPaths
open DocOckTypes.Documentation

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let read_style = function
  | SK_bold -> Bold
  | SK_italic -> Italic
  | SK_emphasize -> Emphasize
  | SK_center -> Center
  | SK_left -> Left
  | SK_right -> Right
  | SK_superscript -> Superscript
  | SK_subscript -> Subscript
  | SK_custom s -> Custom s

exception InvalidReference of string

let read_reference_string s =
  let open DocOckPaths.Reference in
  let qualified parent s =
    match String.rindex s '-' with
    | exception Not_found -> Unknown(parent, s)
    | idx ->
      let qualifier = String.sub s 0 idx in
      let name = String.sub s (idx + 1) (String.length s - idx - 1) in
      match qualifier with
      | "module" -> Module(parent, name)
      |  "module-type" -> ModuleType(parent, name)
      |  "type" -> Type(parent, s)
      | "const" | "constructor" -> Constructor(parent, name)
      | "recfield" | "field" -> Field(parent, name)
      | "extension" -> Extension(parent, name)
      | "exn" | "exception" -> Exception(parent, name)
      | "val" | "value" -> Value(parent, name)
      | "class" -> Class(parent, name)
      | "classtype" | "class-type" -> ClassType(parent, name)
      | "method" -> Method(parent, name)
      | "instance-variable" -> InstanceVariable(parent, name)
      | "section" | "label" -> Label(parent, name)
      | "page" -> Page(parent, name)
      | _ -> raise (InvalidReference ("unknown qualifier `" ^ qualifier ^ "'"))
  in
  let rec loop s pos =
    let parent, start, length =
      match String.rindex_from s pos '.' with
      | exception Not_found ->
          None, 0, pos + 1
      | idx ->
          Some (loop s (idx - 1)), idx + 1, pos + 1
    in
    let maybe_qualified = String.sub s 0 (pos + 1) in
    if String.length maybe_qualified = 0 then raise (InvalidReference s)
    else qualified parent maybe_qualified
  in
  loop s (String.length s - 1)

let read_module_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Module(parent, s)
  | Module _ as r -> r
  | _ -> raise (InvalidReference ("invalid module reference " ^ s))

let read_module_type_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> ModuleType(parent, s)
  | ModuleType _ as r -> r
  | _ -> raise (InvalidReference ("invalid module type reference " ^ s))

let read_type_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Type(parent, s)
  | (Type _ | Class _ | ClassType _) as r -> r
  | _ -> raise (InvalidReference ("invalid type reference " ^ s))

let read_constructor_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Constructor(parent, s)
  | (Constructor _ | Exception _ | Extension _) as r -> r
  | _ -> raise (InvalidReference ("invalid constructor reference " ^ s))

let read_field_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Field(parent, s)
  | Field _ as r -> r
  | _ -> raise (InvalidReference ("invalid field reference " ^ s))

let read_exception_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) | Constructor(parent, s) | Extension(parent, s) ->
      Exception(parent, s)
  | Exception _ as r -> r
  | _ -> raise (InvalidReference ("invalid exception reference " ^ s))

let read_value_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Value(parent, s)
  | Value _ as r -> r
  | _ -> raise (InvalidReference ("invalid value reference " ^ s))

let read_class_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) | Type(parent, s) | ClassType(parent, s) ->
      Class(parent, s)
  | Class _ as r -> r
  | _ -> raise (InvalidReference ("invalid class reference " ^ s))

let read_class_type_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) | Type(parent, s) -> ClassType(parent, s)
  | (ClassType _ | Class _) as r -> r
  | _ -> raise (InvalidReference ("invalid class type reference " ^ s))

let read_method_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Method(parent, s)
  | Method _ as r -> r
  | _ -> raise (InvalidReference ("invalid method reference " ^ s))

let read_instance_variable_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> InstanceVariable(parent, s)
  | InstanceVariable _ as r -> r
  | _ -> raise (InvalidReference ("invalid instance variable reference " ^ s))

let read_label_reference_string s =
  let open DocOckPaths.Reference in
  match read_reference_string s with
  | Unknown(parent, s) -> Label(parent, s)
  | Label _ as r -> r
  | _ -> raise (InvalidReference ("invalid label reference " ^ s))

exception InvalidPath of string

let read_simple_string s =
  let open DocOckPaths.Simple.Module in
  let rec loop s pos =
    match String.rindex_from s pos '.' with
    | exception Not_found ->
        let name = String.sub s 0 (pos + 1) in
        if String.length name = 0 then raise (InvalidPath s)
        else Root name
    | idx ->
        let name = String.sub s (idx + 1) (pos - idx) in
        if String.length name = 0 then raise (InvalidPath s)
        else Dot(loop s (idx - 1), name)
  in
  loop s (String.length s - 1)

let read_reference rk s =
  match rk with
  | RK_link -> Link s
  | RK_custom k -> Custom(k, s)
  | RK_element -> Element (read_reference_string s)
  | RK_module -> Element (read_module_reference_string s)
  | RK_module_type -> Element (read_module_type_reference_string s)
  | RK_type -> Element (read_type_reference_string s)
  | RK_const -> Element (read_constructor_reference_string s)
  | RK_recfield -> Element (read_field_reference_string s)
  | RK_exception -> Element (read_exception_reference_string s)
  | RK_value -> Element (read_value_reference_string s)
  | RK_class -> Element (read_class_reference_string s)
  | RK_class_type -> Element (read_class_type_reference_string s)
  | RK_method -> Element (read_method_reference_string s)
  | RK_attribute -> Element (read_instance_variable_reference_string s)
  | RK_section -> Element (read_label_reference_string s)

let read_special_reference = function
  | SRK_module_list mds ->
      Modules (List.map (fun lid -> read_simple_string lid, []) mds)
  | SRK_index_list -> Index

let rec read_text_element parent
  : Octavius.Types.text_element -> 'a text_element =
  function
  | Raw s -> Raw s
  | Code s -> Code s
  | PreCode s -> PreCode s
  | Verbatim s -> Verbatim s
  | Style(sk, txt) -> Style(read_style sk, read_text parent txt)
  | List l -> List (List.map (read_text parent) l)
  | Enum l -> Enum (List.map (read_text parent) l)
  | Newline -> Newline
  | Title(i, l, txt) -> begin
      let txt = read_text parent txt in
        match l with
        | None -> Title(i, None, txt)
        | Some name ->
            let name = LabelName.of_string name in
            let id = DocOckPaths.Identifier.Label.Label(parent, name) in
              Title(i, Some id, txt)
    end
  | Ref(rk, s, txt) ->
      Reference(read_reference rk s, opt_map (read_text parent) txt)
  | Special_ref srk -> Special (read_special_reference srk)
  | Target (target, code) -> Target (target, code)

and read_text parent txt = List.map (read_text_element parent) txt

let read_see = function
  | See_url s -> Url s
  | See_file s -> File s
  | See_doc s -> Doc s


let read_tag parent : Octavius.Types.tag -> 'a tag = function
  | Author s -> Author s
  | Version v -> Version v
  | See (r, t) -> See (read_see r, read_text parent t)
  | Since s -> Since s
  | Before (s, t) -> Before (s, read_text parent t)
  | Deprecated t -> Deprecated (read_text parent t)
  | Param (s, t) -> Param (s, read_text parent t)
  | Raised_exception (s, t) -> Raise (s, read_text parent t)
  | Return_value t -> Return (read_text parent t)
  | Inline -> Inline
  | Custom (s, t) -> Tag (s, read_text parent t)
  | Canonical p -> Canonical (read_simple_string p)

let empty_body = { text = []; tags = []; }

let empty = Ok empty_body

let read_payload =
  let open Parsetree in function
  | PStr[{ pstr_desc =
             Pstr_eval({ pexp_desc =
                           Pexp_constant( Parsetree.Pconst_string(str, _));
                         pexp_loc = loc;
                         _
                       }, _)
         ; _ }] -> Some(str, loc)
  | _ -> None

let read_offset err =
  let open Octavius.Errors in
  let loc = err.location in
  let start =
    { Error.Position.
        line = loc.start.line;
        column = loc.start.column; }
  in
  let finish =
    { Error.Position.
        line = loc.finish.line;
        column = loc.finish.column; }
  in
    { Error.Offset.start; finish; }

let read_position offset pos =
  let open Lexing in
  let open Error in
  let off_line = offset.Position.line in
  let off_column = offset.Position.column in
  let line = pos.pos_lnum + off_line - 1 in
  let column =
    if off_line = 1 then
      (pos.pos_cnum - pos.pos_bol) + off_column + 3
    else off_column
  in
  { Position.line; column }

let read_location offset pos =
  let open Lexing in
  let open Error in
  if pos.pos_cnum >= 0 then begin
    let filename = pos.pos_fname in
    let start = read_position offset.Offset.start pos in
    let finish = read_position offset.Offset.finish pos in
    Some { Location.filename; start; finish }
  end else None

let read_error origin err pos =
  let open Error in
  let offset = read_offset err in
  let location = read_location offset pos in
  let message = Octavius.Errors.message err.Octavius.Errors.error in
    { origin; offset; location; message }

let attribute_location loc =
  let open Lexing in
  let open Location in
  let open Error in
  let start = loc.loc_start in
  let finish = loc.loc_end in
  if start.pos_cnum >= 0 && finish.pos_cnum >= 0 then begin
    let filename = start.pos_fname in
    let read_pos pos =
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      { Position.line; column }
    in
    let start = read_pos start in
    let finish = read_pos finish in
    Some { Location.filename; start; finish }
  end else None

let invalid_attribute_error origin loc =
  let open Error in
  let offset =
    let zero_pos = { Position.line = 0; column = 0 } in
    { Offset.start = zero_pos; finish = zero_pos }
  in
  let location = attribute_location loc in
  let message = "Invalid documentation attribute" in
    { origin; offset; location; message }

let invalid_reference_error origin loc s =
  let open Error in
  (* TODO get an actual offset *)
  let dummy = { Position.line = 0; column = 0} in
  let offset = { Offset.start = dummy; finish = dummy } in
  (* TODO get an accurate location *)
  let location = attribute_location loc in
  let message = "Invalid reference: \"" ^ s ^ "\"" in
    {origin; offset; location; message}

let invalid_path_error origin loc s =
  let open Error in
  (* TODO get an actual offset *)
  let dummy = { Position.line = 0; column = 0} in
  let offset = { Offset.start = dummy; finish = dummy } in
  (* TODO get an accurate location *)
  let location = attribute_location loc in
  let message = "Invalid path: \"" ^ s ^ "\"" in
    {origin; offset; location; message}

let several_deprecated_error origin loc =
  let open Error in
  (* TODO get an actual offset *)
  let dummy = { Position.line = 0; column = 0} in
  let offset = { Offset.start = dummy; finish = dummy } in
  (* TODO get an accurate location *)
  let location = attribute_location loc in
  let message = "Several deprecation tags are attached to this item" in
    {origin; offset; location; message}

let read_attributes parent id attrs =
  let ocaml_deprecated = ref None in
  let rec loop first nb_deprecated acc : _ -> 'a t = function
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc}, payload) :: rest -> begin
        match read_payload payload with
        | Some (str, loc) -> begin
            let start_pos = loc.Location.loc_start in
            let lexbuf = Lexing.from_string str in
            match Octavius.parse lexbuf with
            | Octavius.Ok (text, tags) -> begin
                try
                  let text = read_text parent text in
                  let text = if first then text else Newline :: text in
                  let tags = List.map (read_tag parent) tags in
                  let nb_deprecated =
                    List.fold_right (function
                      | Deprecated _ -> (+) 1
                      | _ -> fun x -> x
                    ) tags nb_deprecated
                  in
                  if nb_deprecated > 1 then
                    Error (several_deprecated_error id loc)
                  else
                    let acc =
                      { text = acc.text @ text;
                        tags = acc.tags @ tags; }
                    in
                    loop false nb_deprecated acc rest
                with
                | InvalidReference s ->
                    Error (invalid_reference_error id loc s)
                | InvalidPath s ->
                    Error (invalid_path_error id loc s)
              end
            | Octavius.Error err -> Error (read_error id err start_pos)
          end
        | None -> Error (invalid_attribute_error id loc)
      end
    | ({Location.txt =
          ("deprecated" | "ocaml.deprecated"); _}, payload) :: rest -> begin
        match read_payload payload with
        | Some (str, _) ->
          (* Not parsing with octavius here, we take the string verbatim. *)
          let deprecated_tag = Deprecated [Raw str] in
          ocaml_deprecated := Some deprecated_tag;
          loop first nb_deprecated acc rest
        | None ->
          (* The compiler just ignores deprecated attributes whose payload is
             not a string, we do the same. *)
          loop first nb_deprecated acc rest
      end
    | _ :: rest -> loop first nb_deprecated acc rest
    | [] -> begin
        match nb_deprecated, !ocaml_deprecated with
        | 0, Some tag -> Ok { acc with tags = acc.tags @ [tag] }
        | _, _ -> Ok acc
      end
  in
    loop true 0 empty_body attrs

let read_module_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_module id in
  read_attributes parent id attrs

let read_module_type_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_module_type id in
  read_attributes parent id attrs

let read_type_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_type id in
  read_attributes parent id attrs

let read_constructor_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_constructor id in
  read_attributes parent id attrs

let read_field_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_field id in
  read_attributes parent id attrs

let read_extension_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_extension id in
  read_attributes parent id attrs

let read_exception_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_exception id in
  read_attributes parent id attrs

let read_value_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_value id in
  read_attributes parent id attrs

let read_class_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_class id in
  read_attributes parent id attrs

let read_class_type_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  let id = Identifier.of_class_type id in
  read_attributes parent id attrs

let read_method_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_class_signature parent in
  let id = Identifier.of_method id in
  read_attributes parent id attrs

let read_instance_variable_attributes parent id attrs =
  let parent = Identifier.LabelParent.of_class_signature parent in
  let id = Identifier.of_instance_variable id in
  read_attributes parent id attrs

let read_signature_attributes parent attrs =
  let id = Identifier.of_signature parent in
  let parent = Identifier.LabelParent.of_signature parent in
  read_attributes parent id attrs

let read_class_signature_attributes parent attrs =
  let id = Identifier.of_class_signature parent in
  let parent = Identifier.LabelParent.of_class_signature parent in
  read_attributes parent id attrs

let read_string parent loc str : 'a comment =
  let origin = Identifier.of_label_parent parent in
  let lexbuf = Lexing.from_string str in
  let start_pos = loc.Location.loc_start in
  let doc =
    match Octavius.parse lexbuf with
    | Octavius.Ok(text, tags) -> begin
        try
          let text = read_text parent text in
          let tags = List.map (read_tag parent) tags in
          Ok {text; tags}
        with
        | InvalidReference s ->
            Error (invalid_reference_error origin loc s)
        | InvalidPath s ->
            Error (invalid_path_error origin loc s)
      end
    | Octavius.Error err ->
        Error (read_error origin err start_pos)
  in
  Documentation doc

let read_comment parent : Parsetree.attribute -> 'a comment option =
  function
  | ({Location.txt =
        ("text" | "ocaml.text"); loc}, payload) -> begin
      match read_payload payload with
      | Some ("/*", _loc) -> Some Stop
      | Some (str, loc) -> Some (read_string parent loc str)
      | None ->
          let origin = Identifier.of_label_parent parent in
          let doc = Error (invalid_attribute_error origin loc) in
            Some (Documentation doc)
    end
  | _ -> None

let read_signature_comment parent attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  read_comment parent attrs

let read_class_signature_comment parent attrs =
  let parent = Identifier.LabelParent.of_class_signature parent in
  read_comment parent attrs

let read_comments parent attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
         match read_comment parent attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms

let read_signature_comments parent attrs =
  let parent = Identifier.LabelParent.of_signature parent in
  read_comments parent attrs

let read_class_signature_comments parent attrs =
  let parent = Identifier.LabelParent.of_class_signature parent in
  read_comments parent attrs
