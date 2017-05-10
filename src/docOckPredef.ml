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

open DocOckNames
open DocOckPaths
open DocOckTypes

let empty_doc =
  let open Documentation in
    {text = []; tags = [];}

let nullary_equation =
  let open TypeDecl.Equation in
  let params = [] in
  let private_ = false in
  let manifest = None in
  let constraints = [] in
    {params; private_; manifest; constraints}

let covariant_equation =
  let open TypeDecl in
  let open TypeDecl.Equation in
  let params = [Var "'a", Some Pos] in
  let private_ = false in
  let manifest = None in
  let constraints = [] in
    {params; private_; manifest; constraints}

let invariant_equation =
  let open TypeDecl in
  let open TypeDecl.Equation in
  let params = [Var "'a", None] in
  let private_ = false in
  let manifest = None in
  let constraints = [] in
    {params; private_; manifest; constraints}

let bool_name = TypeName.of_string "bool"
let int_name = TypeName.of_string "int"
let char_name = TypeName.of_string "char"
let bytes_name = TypeName.of_string "bytes"
let string_name = TypeName.of_string "string"
let float_name = TypeName.of_string "float"
let bool_name = TypeName.of_string "bool"
let unit_name = TypeName.of_string "unit"
let exn_name = TypeName.of_string "exn"
let array_name = TypeName.of_string "array"
let list_name = TypeName.of_string "list"
let option_name = TypeName.of_string "option"
let int32_name = TypeName.of_string "int32"
let int64_name = TypeName.of_string "int64"
let nativeint_name = TypeName.of_string "nativeint"
let lazy_t_name = TypeName.of_string "lazy_t"
let extension_constructor_name = TypeName.of_string "extension_constructor"

let bool_identifier = Identifier.Type.CoreType bool_name
let int_identifier = Identifier.Type.CoreType int_name
let char_identifier = Identifier.Type.CoreType char_name
let bytes_identifier = Identifier.Type.CoreType bytes_name
let string_identifier = Identifier.Type.CoreType string_name
let float_identifier = Identifier.Type.CoreType float_name
let bool_identifier = Identifier.Type.CoreType bool_name
let unit_identifier = Identifier.Type.CoreType unit_name
let exn_identifier = Identifier.Type.CoreType exn_name
let array_identifier = Identifier.Type.CoreType array_name
let list_identifier = Identifier.Type.CoreType list_name
let option_identifier = Identifier.Type.CoreType option_name
let int32_identifier = Identifier.Type.CoreType int32_name
let int64_identifier = Identifier.Type.CoreType int64_name
let nativeint_identifier = Identifier.Type.CoreType nativeint_name
let lazy_t_identifier = Identifier.Type.CoreType lazy_t_name
let extension_constructor_identifier =
  Identifier.Type.CoreType extension_constructor_name

(* These are needed due to the value restriction.
   If variance wasn't broken with recursive modules then the
   relaxed value restriction would save us. *)
let bool_path_identifier = Identifier.Path.Type.CoreType bool_name
let int_path_identifier = Identifier.Path.Type.CoreType int_name
let char_path_identifier = Identifier.Path.Type.CoreType char_name
let bytes_path_identifier = Identifier.Path.Type.CoreType bytes_name
let string_path_identifier = Identifier.Path.Type.CoreType string_name
let float_path_identifier = Identifier.Path.Type.CoreType float_name
let bool_path_identifier = Identifier.Path.Type.CoreType bool_name
let unit_path_identifier = Identifier.Path.Type.CoreType unit_name
let exn_path_identifier = Identifier.Path.Type.CoreType exn_name
let array_path_identifier = Identifier.Path.Type.CoreType array_name
let list_path_identifier = Identifier.Path.Type.CoreType list_name
let option_path_identifier = Identifier.Path.Type.CoreType option_name
let int32_path_identifier = Identifier.Path.Type.CoreType int32_name
let int64_path_identifier = Identifier.Path.Type.CoreType int64_name
let nativeint_path_identifier = Identifier.Path.Type.CoreType nativeint_name
let lazy_t_path_identifier = Identifier.Path.Type.CoreType lazy_t_name
let extension_constructor_path_identifier =
  Identifier.Path.Type.CoreType extension_constructor_name

(* These are needed due to the value restriction.
   If variance wasn't broken with recursive modules then the
   relaxed value restriction would save us. *)
let bool_reference_identifier = Identifier.CoreType bool_name
let int_reference_identifier = Identifier.CoreType int_name
let char_reference_identifier = Identifier.CoreType char_name
let bytes_reference_identifier = Identifier.CoreType bytes_name
let string_reference_identifier = Identifier.CoreType string_name
let float_reference_identifier = Identifier.CoreType float_name
let bool_reference_identifier = Identifier.CoreType bool_name
let unit_reference_identifier = Identifier.CoreType unit_name
let exn_reference_identifier = Identifier.CoreType exn_name
let array_reference_identifier = Identifier.CoreType array_name
let list_reference_identifier = Identifier.CoreType list_name
let option_reference_identifier = Identifier.CoreType option_name
let int32_reference_identifier = Identifier.CoreType int32_name
let int64_reference_identifier = Identifier.CoreType int64_name
let nativeint_reference_identifier = Identifier.CoreType nativeint_name
let lazy_t_reference_identifier = Identifier.CoreType lazy_t_name
let extension_constructor_reference_identifier =
  Identifier.CoreType extension_constructor_name

let false_name = ConstructorName.of_string "false"
let true_name = ConstructorName.of_string "true"
let void_name = ConstructorName.of_string "()"
let nil_name = ConstructorName.of_string "([])"
let cons_name = ConstructorName.of_string "(::)"
let none_name = ConstructorName.of_string "None"
let some_name = ConstructorName.of_string "Some"

let false_identifier =
  Identifier.Constructor.Constructor(bool_identifier, false_name)
let true_identifier =
  Identifier.Constructor.Constructor(bool_identifier, true_name)
let void_identifier =
  Identifier.Constructor.Constructor(unit_identifier, void_name)
let nil_identifier =
  Identifier.Constructor.Constructor(list_identifier, nil_name)
let cons_identifier =
  Identifier.Constructor.Constructor(list_identifier, cons_name)
let none_identifier =
  Identifier.Constructor.Constructor(option_identifier, none_name)
let some_identifier =
  Identifier.Constructor.Constructor(option_identifier, some_name)

(* These are needed due to the value restriction.
   If variance wasn't broken with recursive modules then the
   relaxed value restriction would save us. *)
let false_reference_identifier =
  Identifier.Constructor(bool_identifier, false_name)
let true_reference_identifier =
  Identifier.Constructor(bool_identifier, true_name)
let void_reference_identifier =
  Identifier.Constructor(unit_identifier, void_name)
let nil_reference_identifier =
  Identifier.Constructor(list_identifier, nil_name)
let cons_reference_identifier =
  Identifier.Constructor(list_identifier, cons_name)
let none_reference_identifier =
  Identifier.Constructor(option_identifier, none_name)
let some_reference_identifier =
  Identifier.Constructor(option_identifier, some_name)

let match_failure_name = ExceptionName.of_string "Match_failure"
let assert_failure_name = ExceptionName.of_string "Assert_failure"
let invalid_argument_name = ExceptionName.of_string "Invalid_argument"
let failure_name = ExceptionName.of_string "Failure"
let not_found_name = ExceptionName.of_string "Not_found"
let out_of_memory_name = ExceptionName.of_string "Out_of_memory"
let stack_overflow_name = ExceptionName.of_string "Stack_overflow"
let sys_error_name = ExceptionName.of_string "Sys_error"
let end_of_file_name = ExceptionName.of_string "End_of_file"
let division_by_zero_name = ExceptionName.of_string "Division_by_zero"
let sys_blocked_io_name = ExceptionName.of_string "Sys_blocked_io"
let undefined_recursive_module_name =
  ExceptionName.of_string "Undefined_recursive_module"

let match_failure_identifier =
  Identifier.Exception.CoreException match_failure_name
let assert_failure_identifier =
  Identifier.Exception.CoreException assert_failure_name
let invalid_argument_identifier =
  Identifier.Exception.CoreException invalid_argument_name
let failure_identifier =
  Identifier.Exception.CoreException failure_name
let not_found_identifier =
  Identifier.Exception.CoreException not_found_name
let out_of_memory_identifier =
  Identifier.Exception.CoreException out_of_memory_name
let stack_overflow_identifier =
  Identifier.Exception.CoreException stack_overflow_name
let sys_error_identifier =
  Identifier.Exception.CoreException sys_error_name
let end_of_file_identifier =
  Identifier.Exception.CoreException end_of_file_name
let division_by_zero_identifier =
  Identifier.Exception.CoreException division_by_zero_name
let sys_blocked_io_identifier =
  Identifier.Exception.CoreException sys_blocked_io_name
let undefined_recursive_module_identifier =
  Identifier.Exception.CoreException undefined_recursive_module_name

(* These are needed due to the value restriction.
   If variance wasn't broken with recursive modules then the
   relaxed value restriction would save us. *)
let match_failure_reference_identifier =
  Identifier.CoreException match_failure_name
let assert_failure_reference_identifier =
  Identifier.CoreException assert_failure_name
let invalid_argument_reference_identifier =
  Identifier.CoreException invalid_argument_name
let failure_reference_identifier =
  Identifier.CoreException failure_name
let not_found_reference_identifier =
  Identifier.CoreException not_found_name
let out_of_memory_reference_identifier =
  Identifier.CoreException out_of_memory_name
let stack_overflow_reference_identifier =
  Identifier.CoreException stack_overflow_name
let sys_error_reference_identifier =
  Identifier.CoreException sys_error_name
let end_of_file_reference_identifier =
  Identifier.CoreException end_of_file_name
let division_by_zero_reference_identifier =
  Identifier.CoreException division_by_zero_name
let sys_blocked_io_reference_identifier =
  Identifier.CoreException sys_blocked_io_name
let undefined_recursive_module_reference_identifier =
  Identifier.CoreException undefined_recursive_module_name

let core_type_identifier = function
  | "int" -> Some int_identifier
  | "char" -> Some char_identifier
  | "bytes" -> Some bytes_identifier
  | "string" -> Some string_identifier
  | "float" -> Some float_identifier
  | "bool" -> Some bool_identifier
  | "unit" -> Some unit_identifier
  | "exn" -> Some exn_identifier
  | "array" -> Some array_identifier
  | "list" -> Some list_identifier
  | "option" -> Some option_identifier
  | "int32" -> Some int32_identifier
  | "int64" -> Some int64_identifier
  | "nativeint" -> Some nativeint_identifier
  | "lazy_t" -> Some lazy_t_identifier
  | "extension_constructor" -> Some extension_constructor_identifier
  | _ -> None

let core_exception_identifier = function
  | "Match_failure" -> Some match_failure_identifier
  | "Out_of_memory" -> Some out_of_memory_identifier
  | "Invalid_argument" -> Some invalid_argument_identifier
  | "Failure" -> Some failure_identifier
  | "Not_found" -> Some not_found_identifier
  | "Sys_error" -> Some sys_error_identifier
  | "End_of_file" -> Some end_of_file_identifier
  | "Division_by_zero" -> Some division_by_zero_identifier
  | "Stack_overflow" -> Some stack_overflow_identifier
  | "Sys_blocked_io" -> Some sys_blocked_io_identifier
  | "Assert_failure" -> Some assert_failure_identifier
  | "Undefined_recursive_module" -> Some undefined_recursive_module_identifier
  | _ -> None

let core_constructor_identifier = function
  | "false" -> Some false_identifier
  | "true" -> Some true_identifier
  | "()" -> Some void_identifier
  | "[]" -> Some nil_identifier
  | "([])" -> Some nil_identifier
  | "::" -> Some cons_identifier
  | "(::)" -> Some cons_identifier
  | "None" -> Some none_identifier
  | "Some" -> Some some_identifier
  | _ -> None

let bool_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier bool_path_identifier)
let int_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier int_path_identifier)
let char_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier char_path_identifier)
let bytes_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier bytes_path_identifier)
let string_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier string_path_identifier)
let float_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier float_path_identifier)
let unit_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier unit_path_identifier)
let exn_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier exn_path_identifier)
let array_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier array_path_identifier)
let list_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier list_path_identifier)
let option_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier option_path_identifier)
let int32_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier int32_path_identifier)
let int64_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier int64_path_identifier)
let nativeint_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier nativeint_path_identifier)
let lazy_t_path =
  Path.Type.Resolved (Path.Resolved.Type.Identifier lazy_t_path_identifier)
let extension_constructor_path =
  Path.Type.Resolved
    (Path.Resolved.Type.Identifier extension_constructor_path_identifier)

let bool_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier bool_reference_identifier)
let int_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier int_reference_identifier)
let char_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier char_reference_identifier)
let bytes_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier bytes_reference_identifier)
let string_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier string_reference_identifier)
let float_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier float_reference_identifier)
let unit_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier unit_reference_identifier)
let exn_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier exn_reference_identifier)
let array_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier array_reference_identifier)
let list_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier list_reference_identifier)
let option_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier option_reference_identifier)
let int32_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier int32_reference_identifier)
let int64_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier int64_reference_identifier)
let nativeint_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier nativeint_reference_identifier)
let lazy_t_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier lazy_t_reference_identifier)
let extension_constructor_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier extension_constructor_reference_identifier)

let false_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier false_reference_identifier)
let true_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier true_reference_identifier)
let void_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier void_reference_identifier)
let nil_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier nil_reference_identifier)
let cons_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier cons_reference_identifier)
let none_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier none_reference_identifier)
let some_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier some_reference_identifier)

let match_failure_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier match_failure_reference_identifier)
let assert_failure_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier assert_failure_reference_identifier)
let invalid_argument_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier invalid_argument_reference_identifier)
let failure_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier failure_reference_identifier)
let not_found_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier not_found_reference_identifier)
let out_of_memory_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier out_of_memory_reference_identifier)
let stack_overflow_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier stack_overflow_reference_identifier)
let sys_error_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier sys_error_reference_identifier)
let end_of_file_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier end_of_file_reference_identifier)
let division_by_zero_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier division_by_zero_reference_identifier)
let sys_blocked_io_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier sys_blocked_io_reference_identifier)
let undefined_recursive_module_reference =
  Reference.Resolved
    (Reference.Resolved.Identifier
       undefined_recursive_module_reference_identifier)

let false_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let args = Tuple [] in
  let res = None in
    {id = false_identifier; doc; args; res}

let true_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let args = Tuple [] in
  let res = None in
    {id = true_identifier; doc; args; res}

let void_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let args = Tuple [] in
  let res = None in
    {id = void_identifier; doc; args; res}

let nil_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let args = Tuple [] in
  let res = None in
    {id = nil_identifier; doc; args; res}

let cons_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let head = TypeExpr.Var "'a" in
  let tail = TypeExpr.(Constr(list_path, [head])) in
  let args = Tuple [head; tail] in
  let res = None in
    {id = cons_identifier; doc; args; res}

let none_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let args = Tuple [] in
  let res = None in
    {id = none_identifier; doc; args; res}

let some_decl =
  let open TypeDecl.Constructor in
  let open Documentation in
  let doc = Ok empty_doc in
  let var = TypeExpr.Var "'a" in
  let args = Tuple [var] in
  let res = None in
    {id = some_identifier; doc; args; res}


let int_decl =
  let open TypeDecl in
  let open Documentation in
  let id = int_identifier in
  let text = [Raw "The type of integer numbers."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let char_decl =
  let open TypeDecl in
  let open Documentation in
  let id = char_identifier in
  let text = [Raw "The type of characters."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let bytes_decl =
  let open TypeDecl in
  let open Documentation in
  let id = bytes_identifier in
  let text = [Raw "The type of (writable) byte sequences."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let string_decl =
  let open TypeDecl in
  let open Documentation in
  let id = string_identifier in
  let text = [Raw "The type of (read-only) character strings."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let float_decl =
  let open TypeDecl in
  let open Documentation in
  let id = float_identifier in
  let text = [Raw "The type of floating-point numbers."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let bool_decl =
  let open TypeDecl in
  let open Representation in
  let open Documentation in
  let id = bool_identifier in
  let text = [Raw "The type of booleans (truth values)."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = Some (Variant [false_decl; true_decl]) in
    {id; doc; equation; representation}

let unit_decl =
  let open TypeDecl in
  let open Representation in
  let open Documentation in
  let id = unit_identifier in
  let text = [Raw "The type of the unit value."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = Some (Variant [void_decl]) in
    {id; doc; equation; representation}

let exn_decl =
  let open TypeDecl in
  let open Representation in
  let open Documentation in
  let id = exn_identifier in
  let text = [Raw "The type of exception values."] in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = Some Extensible in
    {id; doc; equation; representation}

let array_decl =
  let open TypeDecl in
  let open Documentation in
  let id = array_identifier in
  let text =
    [Raw "The type of arrays whose elements have type ";
     Code "'a";
     Raw "."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = invariant_equation in
  let representation = None in
    {id; doc; equation; representation}

let list_decl =
  let open TypeDecl in
  let open Representation in
  let open Documentation in
  let id = list_identifier in
  let text =
    [Raw "The type of lists whose elements have type ";
     Code "'a";
     Raw "."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = covariant_equation in
  let representation = Some (Variant [nil_decl; cons_decl]) in
    {id; doc; equation; representation}

let option_decl =
  let open TypeDecl in
  let open Representation in
  let open Documentation in
  let id = option_identifier in
  let text =
    [Raw "The type of optional values of type ";
     Code "'a";
     Raw "."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = covariant_equation in
  let representation = Some (Variant [none_decl; some_decl]) in
    {id; doc; equation; representation}

let int32_decl =
  let open TypeDecl in
  let open Documentation in
  let id = int32_identifier in
  let text =
    [Raw "The type of signed 32-bit integers. See the ";
     Reference(Element(Module(None, "Int32")), None);
     Raw " module."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let int64_decl =
  let open TypeDecl in
  let open Documentation in
  let id = int64_identifier in
  let text =
    [Raw "The type of signed 64-bit integers. See the ";
     Reference(Element(Module(None, "Int64")), None);
     Raw " module."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let nativeint_decl =
  let open TypeDecl in
  let open Documentation in
  let id = nativeint_identifier in
  let text =
    [Raw "The type of signed, platform-native integers (32 bits on \
          32-bit processors, 64 bits on 64-bit processors). See the ";
     Reference(Element(Module(None, "Nativeint")), None);
     Raw " module."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = nullary_equation in
  let representation = None in
    {id; doc; equation; representation}

let lazy_t_decl =
  let open TypeDecl in
  let open Documentation in
  let id = lazy_t_identifier in
  let text =
    [Raw "This type is used to implement the ";
     Reference(Element(Module(None, "Lazy")), None);
     Raw " module. It should not be used directly."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = covariant_equation in
  let representation = None in
    {id; doc; equation; representation}

let extension_constructor_decl =
  let open TypeDecl in
  let open Documentation in
  let id = extension_constructor_identifier in
  let text =
    [Raw "cf. ";
     Reference(Element(Module(None, "Obj")), None);
     Raw " module. It should not be used directly."]
  in
  let doc = Ok {empty_doc with text} in
  let equation = covariant_equation in
  let representation = None in
    {id; doc; equation; representation}

let match_failure_decl =
  let open Exception in
  let open Documentation in
  let id = match_failure_identifier in
  let text =
    [Raw "Exception raised when none of the cases of a pattern matching apply. \
          The arguments are the location of the ";
     Code "match";
     Raw " keyword in the source code (file name, line number, column number)."]
  in
  let doc = Ok {empty_doc with text} in
  let string_expr = TypeExpr.Constr(string_path, []) in
  let int_expr = TypeExpr.Constr(int_path, []) in
  let args =
    TypeDecl.Constructor.Tuple [TypeExpr.Tuple[string_expr; int_expr; int_expr]]
  in
  let res = None in
    {id; doc; args; res}

let assert_failure_decl =
  let open Exception in
  let open Documentation in
  let id = assert_failure_identifier in
  let text =
    [Raw "Exception raised when and assertion fails. \
          The arguments are the location of the ";
     Code "assert";
     Raw " keyword in the source code (file name, line number, column number)."]
  in
  let doc = Ok {empty_doc with text} in
  let string_expr = TypeExpr.Constr(string_path, []) in
  let int_expr = TypeExpr.Constr(int_path, []) in
  let args =
    TypeDecl.Constructor.Tuple [TypeExpr.Tuple[string_expr; int_expr; int_expr]]
  in
  let res = None in
    {id; doc; args; res}

let invalid_argument_decl =
  let open Exception in
  let open Documentation in
  let id = invalid_argument_identifier in
  let text =
    [Raw "Exception raised by library functions to signal that the given \
          arguments do not make sense."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [TypeExpr.Constr(string_path, [])] in
  let res = None in
    {id; doc; args; res}

let failure_decl =
  let open Exception in
  let open Documentation in
  let id = failure_identifier in
  let text =
    [Raw "Exception raised by library functions to signal that they are \
          undefined on the given arguments."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [TypeExpr.Constr(string_path, [])] in
  let res = None in
    {id; doc; args; res}

let not_found_decl =
  let open Exception in
  let open Documentation in
  let id = not_found_identifier in
  let text =
    [Raw "Exception raised by search functions when the desired object \
          could not be found."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

let out_of_memory_decl =
  let open Exception in
  let open Documentation in
  let id = out_of_memory_identifier in
  let text =
    [Raw "Exception raised by the garbage collector when there is \
          insufficient memory to complete the computation."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

(* TODO: Provide reference to the OCaml manual *)
let stack_overflow_decl =
  let open Exception in
  let open Documentation in
  let id = stack_overflow_identifier in
  let text =
    [Raw "Exception raised by the bytecode interpreter when the evaluation \
          stack reaches its maximal size. This often indicates infinite or \
          excessively deep recursion in the user's program. (Not fully \
          implemented by the native-code compiler; see section 11.5 of \
          the OCaml manual.)"]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

let sys_error_decl =
  let open Exception in
  let open Documentation in
  let id = sys_error_identifier in
  let text =
    [Raw "Exception raised by the input/output functions to report an \
          operating system error."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [TypeExpr.Constr(string_path, [])] in
  let res = None in
    {id; doc; args; res}

let end_of_file_decl =
  let open Exception in
  let open Documentation in
  let id = end_of_file_identifier in
  let text =
    [Raw "Exception raised by input functions to signal that the end of \
          file has been reached."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

let division_by_zero_decl =
  let open Exception in
  let open Documentation in
  let id = division_by_zero_identifier in
  let text =
    [Raw "Exception raised by integer division and remainder operations \
          when their second argument is zero."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

let sys_blocked_io_decl =
  let open Exception in
  let open Documentation in
  let id = sys_blocked_io_identifier in
  let text =
    [Raw "A special case of ";
     Reference(Element sys_error_reference, None);
     Raw " raised when no I/O is possible on a non-blocking I/O channel."]
  in
  let doc = Ok {empty_doc with text} in
  let args = TypeDecl.Constructor.Tuple [] in
  let res = None in
    {id; doc; args; res}

(* TODO: Provide reference to the OCaml manual *)
let undefined_recursive_module_decl =
  let open Exception in
  let open Documentation in
  let id = undefined_recursive_module_identifier in
  let text =
    [Raw "Exception raised when an ill-founded recursive module definition \
          is evaluated. (See section 7.8 of the OCaml manual.) The arguments \
          are the location of the definition in the source code \
          (file name, line number, column number)."]
  in
  let doc = Ok {empty_doc with text} in
  let string_expr = TypeExpr.Constr(string_path, []) in
  let int_expr = TypeExpr.Constr(int_path, []) in
  let args =
    TypeDecl.Constructor.Tuple [TypeExpr.Tuple[string_expr; int_expr; int_expr]]
  in
  let res = None in
    {id; doc; args; res}

let core_types =
  [int_decl; char_decl; bytes_decl; string_decl; float_decl; bool_decl;
   unit_decl; exn_decl; array_decl; list_decl; option_decl; int32_decl;
   int64_decl; nativeint_decl; lazy_t_decl]

let core_exceptions =
  [match_failure_decl; assert_failure_decl; invalid_argument_decl;
   failure_decl; not_found_decl; out_of_memory_decl; stack_overflow_decl;
   sys_error_decl; end_of_file_decl; division_by_zero_decl;
   sys_blocked_io_decl; undefined_recursive_module_decl]
