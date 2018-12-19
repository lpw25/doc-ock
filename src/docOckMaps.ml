
open DocOckPaths
open DocOckTypes

let rec list_map f l =
  match l with
  | [] -> l
  | x :: r ->
      let x' = f x in
        if x != x' then x' :: List.map f r
        else
          let r' = list_map f r in
            if r != r' then x' :: r'
            else l

let option_map f o =
  match o with
  | None -> o
  | Some x ->
      let x' = f x in
        if x != x' then Some x'
        else o

let pair_map f g p =
  let (a, b) = p in
  let a' = f a in
  let b' = g b in
    if a != a' || b != b' then (a', b')
    else p

class virtual ['a] identifier = object (self)

  method virtual root : 'a -> 'a

  method identifier : type k . ('a, k) Identifier.t -> ('a, k) Identifier.t =
    fun id ->
      let open Identifier in
        match id with
        | Root(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_root_name name in
              if root != root' || name != name' then Root(root', name')
              else id
        | Page(root, name) ->
            let root' = self#root root in
            let name' = self#identifier_page_name name in
              if root != root' || name != name' then Page(root', name')
              else id
        | Module(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else id
        | Argument(parent, pos, name) ->
            let parent' = self#identifier_signature parent in
            let pos' = self#identifier_argument_position pos in
            let name' = self#identifier_argument_name name in
              if parent != parent' || pos != pos' || name != name' then
                Argument(parent', pos', name')
              else id
        | ModuleType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else id
        | Type(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_type_name name in
              if parent != parent' || name != name' then
                Type(parent', name')
              else id
        | CoreType name ->
            let name' = self#identifier_core_type_name name in
              if name != name' then CoreType name'
              else id
        | Constructor(parent, name) ->
            let parent' = self#identifier parent in
            let name' = self#identifier_constructor_name name in
              if parent != parent' || name != name' then
                Constructor(parent', name')
              else id
        | Field(parent, name) ->
            let parent' = self#identifier parent in
            let name' = self#identifier_field_name name in
              if parent != parent' || name != name' then
                Field(parent', name')
              else id
        | Extension(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_extension_name name in
              if parent != parent' || name != name' then
                Extension(parent', name')
              else id
        | Exception(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_exception_name name in
              if parent != parent' || name != name' then
                Exception(parent', name')
              else id
        | CoreException name ->
            let name' = self#identifier_core_exception_name name in
              if name != name' then CoreException name'
              else id
        | Value(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_value_name name in
              if parent != parent' || name != name' then
                Value(parent', name')
              else id
        | Class(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_name name in
              if parent != parent' || name != name' then
                Class(parent', name')
              else id
        | ClassType(parent, name) ->
            let parent' = self#identifier_signature parent in
            let name' = self#identifier_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else id
        | Method(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_method_name name in
              if parent != parent' || name != name' then
                Method(parent', name')
              else id
        | InstanceVariable(parent, name) ->
            let parent' = self#identifier_class_signature parent in
            let name' = self#identifier_instance_variable_name name in
              if parent != parent' || name != name' then
                InstanceVariable(parent', name')
              else id
        | Label(parent, name) ->
            let parent' =
              match parent with
              | (Root _ | Module _ | Argument _ | ModuleType _) as parent ->
                label_parent_of_parent
                  (parent_of_signature
                     (self#identifier_signature parent))
              | (Class _ | ClassType _) as parent ->
                label_parent_of_parent
                  (parent_of_class_signature
                     (self#identifier_class_signature parent))
              | Type _ | CoreType _ as parent ->
                label_parent_of_parent
                  (parent_of_datatype
                    (self#identifier_datatype parent))
              | Page _ as parent ->
                label_parent_of_page
                  (self#identifier_page parent)
            in
            let name' = self#identifier_label_name name in
              if parent != parent' || name != name' then
                Label(parent', name')
              else id

  method identifier_root_name name = name

  method identifier_page_name name = name

  method identifier_module_name name = name

  method identifier_argument_position pos = pos

  method identifier_argument_name name = name

  method identifier_module_type_name name = name

  method identifier_type_name name = name

  method identifier_core_type_name name = name

  method identifier_constructor_name name = name

  method identifier_field_name name = name

  method identifier_extension_name name = name

  method identifier_exception_name name = name

  method identifier_core_exception_name name = name

  method identifier_value_name name = name

  method identifier_class_name name = name

  method identifier_class_type_name name = name

  method identifier_method_name name = name

  method identifier_instance_variable_name name = name

  method identifier_label_name name = name

  method identifier_page (id : 'a Identifier.page) =
    self#identifier id

  method identifier_signature (id : 'a Identifier.signature) =
    self#identifier id

  method identifier_class_signature (id : 'a Identifier.class_signature) =
    self#identifier id

  method identifier_datatype (id : 'a Identifier.datatype) =
    self#identifier id

  method identifier_module (id : 'a Identifier.module_) =
    self#identifier id

  method identifier_module_type (id : 'a Identifier.module_type) =
    self#identifier id

  method identifier_type (id : 'a Identifier.type_) =
    self#identifier id

  method identifier_constructor (id : 'a Identifier.constructor) =
    self#identifier id

  method identifier_field (id : 'a Identifier.field) =
    self#identifier id

  method identifier_extension (id : 'a Identifier.extension) =
    self#identifier id

  method identifier_exception (id : 'a Identifier.exception_) =
    self#identifier id

  method identifier_value (id : 'a Identifier.value) =
    self#identifier id

  method identifier_class (id : 'a Identifier.class_) =
    self#identifier id

  method identifier_class_type (id : 'a Identifier.class_type) =
    self#identifier id

  method identifier_method (id : 'a Identifier.method_) =
    self#identifier id

  method identifier_instance_variable (id : 'a Identifier.instance_variable) =
    self#identifier id

  method identifier_label (id : 'a Identifier.label) =
    self#identifier id

end

class virtual ['a] path = object (self)

  method virtual identifier : 'k . ('a, 'k) Identifier.t ->
    ('a, 'k) Identifier.t

  method path_resolved : type k. ('a, k) Path.Resolved.t ->
                                   ('a, k) Path.Resolved.t =
    fun p ->
      let open Path.Resolved in
        match p with
        | Identifier id ->
            let id' = self#identifier id in
              if id != id' then Identifier id'
              else p
        | Subst(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#path_resolved orig in
              if sub != sub' || orig != orig' then Subst(sub', orig')
              else p
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#path_resolved orig in
              if sub != sub' || orig != orig' then SubstAlias(sub', orig')
              else p
        | Hidden hp ->
            let hp' = self#path_resolved hp in
              if hp != hp' then Hidden hp'
              else p
        | Module(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else p
        | Canonical(orig, cano) ->
            let orig' = self#path_resolved orig in
            let cano' = self#path cano in
              if orig != orig' || cano != cano' then Canonical(orig', cano')
              else p
        | Apply(fn, arg) ->
            let fn' = self#path_resolved fn in
            let arg' = self#path arg in
              if fn != fn' || arg != arg' then Apply(fn', arg')
              else p
        | ModuleType(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else p
        | Type(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else p
        | Class(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else p
        | ClassType(parent, name) ->
            let parent' = self#path_resolved parent in
            let name' = self#path_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else p

  method path_resolved_module_name name = name

  method path_resolved_module_type_name name = name

  method path_resolved_type_name name = name

  method path_resolved_class_name name = name

  method path_resolved_class_type_name name = name

  method path_resolved_module (p : 'a Path.Resolved.module_) =
    self#path_resolved p

  method path_resolved_module_type (p : 'a Path.Resolved.module_type) =
    self#path_resolved p

  method path_resolved_type (p : 'a Path.Resolved.type_) =
    self#path_resolved p

  method path_resolved_class_type (p : 'a Path.Resolved.class_type) =
    self#path_resolved p

  method path : type k . ('a, k) Path.t -> ('a, k) Path.t =
    fun p ->
      let open Path in
        match p with
        | Resolved res ->
            let res' = self#path_resolved res in
              if res != res' then Resolved res'
              else p
        | Root name ->
            let name' = self#path_root_name name in
              if name != name' then Root name'
              else p
        | Forward name ->
            let name' = self#path_root_name name in
              if name != name' then Forward name'
              else p
        | Dot(parent, name) ->
            let parent' = self#path parent in
            let name' = self#path_dot_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else p
        | Apply(fn, arg) ->
            let fn' = self#path fn in
            let arg' = self#path arg in
              if fn != fn' || arg != arg' then Apply(fn', arg')
              else p

  method path_root_name name = name

  method path_dot_name name = name

  method path_module (p : 'a Path.module_) =
    self#path p

  method path_module_type (p : 'a Path.module_type) =
    self#path p

  method path_type (p : 'a Path.type_) =
    self#path p

  method path_class_type (p : 'a Path.class_type) =
    self#path p

end

class virtual ['a] fragment = object (self)

  method virtual path_resolved : 'k. ('a, 'k) Path.Resolved.t ->
                                   ('a, 'k) Path.Resolved.t

  method fragment_resolved : type k s. ('a, k, s) Fragment.Resolved.raw ->
                                         ('a, k, s) Fragment.Resolved.raw =
    fun p ->
      let open Fragment.Resolved in
        match p with
        | Root -> p
        | Subst(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#fragment_resolved orig in
              if sub != sub' || orig != orig' then Subst(sub', orig')
              else p
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#fragment_resolved orig in
              if sub != sub' || orig != orig' then SubstAlias(sub', orig')
              else p
        | Module(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_module_name name in
              if parent != parent' || name != name' then Module(parent', name')
              else p
        | Type(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else p
        | Class(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else p
        | ClassType(parent, name) ->
            let parent' = self#fragment_resolved parent in
            let name' = self#fragment_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else p

  method fragment_resolved_module_name name = name

  method fragment_resolved_type_name name = name

  method fragment_resolved_class_name name = name

  method fragment_resolved_class_type_name name = name

  method fragment_resolved_module (p : 'a Fragment.Resolved.module_) =
    self#fragment_resolved p

  method fragment_resolved_type (p : 'a Fragment.Resolved.type_) =
    self#fragment_resolved p

  method fragment : type k s. ('a, k, s) Fragment.raw ->
                                     ('a, k, s) Fragment.raw =
    fun p ->
      let open Fragment in
        match p with
        | Resolved res ->
            let res' = self#fragment_resolved res in
              if res != res' then Resolved res'
              else p
        | Dot(parent, name) ->
            let parent' = self#fragment parent in
            let name' = self#fragment_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else p

  method fragment_name name = name

  method fragment_module (p : 'a Fragment.module_) =
    self#fragment p

  method fragment_type (p : 'a Fragment.type_) =
    self#fragment p

end

class virtual ['a] reference = object (self)

  method virtual identifier : 'k . ('a, 'k) Identifier.t ->
    ('a, 'k) Identifier.t

  method virtual path_resolved : 'k. ('a, 'k) Path.Resolved.t ->
    ('a, 'k) Path.Resolved.t

  method reference_resolved : type k. ('a, k) Reference.Resolved.t ->
                                        ('a, k) Reference.Resolved.t =
    fun r ->
      let open Reference.Resolved in
        match r with
        | Identifier id ->
            let id' = self#identifier id in
              if id != id' then Identifier id'
              else r
        | SubstAlias(sub, orig) ->
            let sub' = self#path_resolved sub in
            let orig' = self#reference_resolved orig in
              if sub != sub' || orig != orig' then
                SubstAlias(sub', orig')
              else r
        | Module(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_module_name name in
              if parent != parent' || name != name' then
                Module(parent', name')
              else r
        | Canonical(orig, cano) ->
            let orig' = self#reference_resolved orig in
            let cano' = self#reference cano in
              if orig != orig' || cano != cano' then
                Canonical(orig', cano')
              else r
        | ModuleType(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_module_type_name name in
              if parent != parent' || name != name' then
                ModuleType(parent', name')
              else r
        | Type(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_type_name name in
              if parent != parent' || name != name' then
                Type(parent', name')
              else r
        | Constructor(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_constructor_name name in
              if parent != parent' || name != name' then
                Constructor(parent', name')
              else r
        | Field(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_field_name name in
              if parent != parent' || name != name' then
                Field(parent', name')
              else r
        | Extension(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_extension_name name in
              if parent != parent' || name != name' then
                Extension(parent', name')
              else r
        | Exception(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_exception_name name in
              if parent != parent' || name != name' then
                Exception(parent', name')
              else r
        | Value(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_value_name name in
              if parent != parent' || name != name' then
                Value(parent', name')
              else r
        | Class(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_class_name name in
              if parent != parent' || name != name' then
                Class(parent', name')
              else r
        | ClassType(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_class_type_name name in
              if parent != parent' || name != name' then
                ClassType(parent', name')
              else r
        | Method(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_method_name name in
              if parent != parent' || name != name' then
                Method(parent', name')
              else r
        | InstanceVariable(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_instance_variable_name name in
              if parent != parent' || name != name' then
                InstanceVariable(parent', name')
              else r
        | Label(parent, name) ->
            let parent' = self#reference_resolved parent in
            let name' = self#reference_resolved_label_name name in
              if parent != parent' || name != name' then
                Label(parent', name')
              else r

  method reference_resolved_module_name name = name

  method reference_resolved_module_type_name name = name

  method reference_resolved_type_name name = name

  method reference_resolved_class_name name = name

  method reference_resolved_class_type_name name = name

  method reference_resolved_constructor_name name = name

  method reference_resolved_extension_name name = name

  method reference_resolved_exception_name name = name

  method reference_resolved_field_name name = name

  method reference_resolved_value_name name = name

  method reference_resolved_method_name name = name

  method reference_resolved_instance_variable_name name = name

  method reference_resolved_label_name name = name

  method reference_resolved_module (r : 'a Reference.Resolved.module_) =
    self#reference_resolved r

  method reference_resolved_module_type
           (r : 'a Reference.Resolved.module_type) =
    self#reference_resolved r

  method reference_resolved_type (r : 'a Reference.Resolved.type_) =
    self#reference_resolved r

  method reference_resolved_constructor (r : 'a Reference.Resolved.constructor) =
    self#reference_resolved r

  method reference_resolved_field (r : 'a Reference.Resolved.field) =
    self#reference_resolved r

  method reference_resolved_extension (r : 'a Reference.Resolved.extension) =
    self#reference_resolved r

  method reference_resolved_exception (r : 'a Reference.Resolved.exception_) =
    self#reference_resolved r

  method reference_resolved_value (r : 'a Reference.Resolved.value) =
    self#reference_resolved r

  method reference_resolved_class (r : 'a Reference.Resolved.class_) =
    self#reference_resolved r

  method reference_resolved_class_type (r : 'a Reference.Resolved.class_type) =
    self#reference_resolved r

  method reference_resolved_method (r : 'a Reference.Resolved.method_) =
    self#reference_resolved r

  method reference_resolved_instance_variable
           (r : 'a Reference.Resolved.instance_variable) =
    self#reference_resolved r

  method reference_resolved_label (r : 'a Reference.Resolved.label) =
    self#reference_resolved r

  method reference_resolved_any (r : 'a Reference.Resolved.any) =
    self#reference_resolved r

  method reference : type k . ('a, k) Reference.t -> ('a, k) Reference.t =
    fun r ->
      let open Reference in
        match r with
        | Resolved res ->
            let res' = self#reference_resolved res in
              if res != res' then Resolved res'
              else r
        | Root (name, kind) ->
            let name' = self#reference_root_name name in
              if name != name' then Root (name', kind)
              else r
        | Dot(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_dot_name name in
              if parent != parent' || name != name' then Dot(parent', name')
              else r
        | Module(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_module_name name in
              if parent != parent' || name != name' then Module(parent', name')
              else r
        | ModuleType(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_module_type_name name in
              if parent != parent' || name != name' then ModuleType(parent', name')
              else r
        | Type(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_type_name name in
              if parent != parent' || name != name' then Type(parent', name')
              else r
        | Constructor(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_constructor_name name in
              if parent != parent' || name != name' then Constructor(parent', name')
              else r
        | Extension(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_extension_name name in
              if parent != parent' || name != name' then Extension(parent', name')
              else r
        | Exception(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_exception_name name in
              if parent != parent' || name != name' then Exception(parent', name')
              else r
        | Field(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_field_name name in
              if parent != parent' || name != name' then Field(parent', name')
              else r
        | Value(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_value_name name in
              if parent != parent' || name != name' then Value(parent', name')
              else r
        | Class(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_class_name name in
              if parent != parent' || name != name' then Class(parent', name')
              else r
        | ClassType(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_class_type_name name in
              if parent != parent' || name != name' then ClassType(parent', name')
              else r
        | Method(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_method_name name in
              if parent != parent' || name != name' then Method(parent', name')
              else r
        | InstanceVariable(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_instance_variable_name name in
              if parent != parent' || name != name' then InstanceVariable(parent', name')
              else r
        | Label(parent, name) ->
            let parent' = self#reference parent in
            let name' = self#reference_label_name name in
              if parent != parent' || name != name' then Label(parent', name')
              else r

  method reference_root_name name = name

  method reference_dot_name name = name

  method reference_module_name name = name

  method reference_module_type_name name = name

  method reference_type_name name = name

  method reference_constructor_name name = name

  method reference_field_name name = name

  method reference_extension_name name = name

  method reference_exception_name name = name

  method reference_value_name name = name

  method reference_class_name name = name

  method reference_class_type_name name = name

  method reference_method_name name = name

  method reference_instance_variable_name name = name

  method reference_label_name name = name

  method reference_module (r : 'a Reference.module_) =
    self#reference r

  method reference_module_type (r : 'a Reference.module_type) =
    self#reference r

  method reference_type (r : 'a Reference.type_) =
    self#reference r

  method reference_constructor (r : 'a Reference.constructor) =
    self#reference r

  method reference_field (r : 'a Reference.field) =
    self#reference r

  method reference_extension (r : 'a Reference.extension) =
    self#reference r

  method reference_exception (r : 'a Reference.exception_) =
    self#reference r

  method reference_value (r : 'a Reference.value) =
    self#reference r

  method reference_class (r : 'a Reference.class_) =
    self#reference r

  method reference_class_type (r : 'a Reference.class_type) =
    self#reference r

  method reference_method (r : 'a Reference.method_) =
    self#reference r

  method reference_instance_variable (r : 'a Reference.instance_variable) =
    self#reference r

  method reference_label (r : 'a Reference.label) =
    self#reference r

  method reference_any (r : 'a Reference.any) =
    self#reference r

end

class virtual ['a] defn = object (self)

  method virtual root : 'a -> 'a

  method defn_index idx = idx

  method defn_name s = s

  method defn defn =
    let open Defn in
    let {root; name; index} = defn in
    let root' = self#root root in
    let name' = self#defn_name name in
    let index' = self#defn_index index in
    if root != root' || name != name' || index != index' then
      {root = root'; name = name'; index = index'}
    else
      defn

end

class virtual ['a] decl = object (self)

  method virtual root : 'a -> 'a

  method decl_index idx = idx

  method decl_name s = s

  method decl decl =
    let open Decl in
    let {root; name; index} = decl in
    let root' = self#root root in
    let name' = self#decl_name name in
    let index' = self#decl_index index in
    if root != root' || name != name' || index != index' then
      {root = root'; name = name'; index = index'}
    else
      decl

end

class virtual ['a] paths = object
  inherit ['a] identifier
  inherit ['a] path
  inherit ['a] fragment
  inherit ['a] reference
  inherit ['a] defn
  inherit ['a] decl
end

class virtual ['a] documentation = object (self)

  method virtual identifier_label :
    'a Identifier.label -> 'a Identifier.label

  method virtual identifier :
    'k. ('a, 'k) Identifier.t -> ('a, 'k) Identifier.t

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual reference_module :
    'a Reference.module_ -> 'a Reference.module_

  method virtual reference_module_type :
    'a Reference.module_type -> 'a Reference.module_type

  method virtual reference_type :
    'a Reference.type_ -> 'a Reference.type_

  method virtual reference_constructor :
    'a Reference.constructor -> 'a Reference.constructor

  method virtual reference_field :
    'a Reference.field -> 'a Reference.field

  method virtual reference_extension :
    'a Reference.extension -> 'a Reference.extension

  method virtual reference_exception :
    'a Reference.exception_ -> 'a Reference.exception_

  method virtual reference_value :
    'a Reference.value -> 'a Reference.value

  method virtual reference_class :
    'a Reference.class_ -> 'a Reference.class_

  method virtual reference_class_type :
    'a Reference.class_type -> 'a Reference.class_type

  method virtual reference_method :
    'a Reference.method_ -> 'a Reference.method_

  method virtual reference_instance_variable :
    'a Reference.instance_variable -> 'a Reference.instance_variable

  method virtual reference_label :
    'a Reference.label -> 'a Reference.label

  method virtual reference_any :
    'a Reference.any -> 'a Reference.any

  method documentation_style ds =
    let open Documentation in
      match ds with
      | Bold | Italic | Emphasize
      | Center | Left | Right
      | Superscript | Subscript -> ds
      | Custom custom ->
          let custom' = self#documentation_style_custom custom in
            if custom != custom' then Custom custom'
            else ds

  method documentation_style_custom custom = custom

  method documentation_reference drf =
    let open Documentation in
      match drf with
      | Element rf ->
          let rf' = self#reference_any rf in
            if rf != rf' then Element rf'
            else drf
      | Link link ->
          let link' = self#documentation_reference_link link in
            if link != link' then Link link'
            else drf
      | Custom(custom, body) ->
          let custom' = self#documentation_reference_custom custom in
          let body' = self#documentation_reference_custom_body body in
            if custom != custom' || body != body' then Custom(custom', body')
            else drf

  method documentation_reference_link link = link
  method documentation_reference_custom custom = custom
  method documentation_reference_custom_body body = body

  method documentation_special_modules (rf, txt as pair) =
    let rf' = self#reference_module rf in
    let txt' = self#documentation_text txt in
    if rf != rf' || txt != txt' then (rf', txt')
    else pair

  method documentation_special sr =
    let open Documentation in
      match sr with
      | Modules rfs ->
        let rfs' = list_map self#documentation_special_modules rfs in
        if rfs != rfs' then Modules rfs'
        else sr
      | Index -> sr

  method documentation_see see =
    let open Documentation in
      match see with
      | Url url ->
          let url' = self#documentation_see_url url in
            if url != url' then Url url'
            else see
      | File file ->
          let file' = self#documentation_see_file file in
            if file != file' then File file'
            else see
      | Doc doc ->
          let doc' = self#documentation_see_doc doc in
            if doc != doc' then Doc doc'
            else see

  method documentation_see_url url = url
  method documentation_see_file file = file
  method documentation_see_doc doc = doc

  method documentation_text_element elem =
    let open Documentation in
      match elem with
      | Raw raw ->
          let raw' = self#documentation_text_raw raw in
            if raw != raw' then Raw raw'
            else elem
      | Code code ->
          let code' = self#documentation_text_code code in
            if code != code' then Code code'
            else elem
      | PreCode precode ->
          let precode' = self#documentation_text_precode precode in
            if precode != precode' then PreCode precode'
            else elem
      | Verbatim verbatim ->
          let verbatim' = self#documentation_text_verbatim verbatim in
            if verbatim != verbatim' then Verbatim verbatim'
            else elem
      | Style(style, text) ->
          let style' = self#documentation_style style in
          let text' = self#documentation_text text in
            if style != style' || text != text' then Style(style', text')
            else elem
      | List texts ->
          let texts' = list_map self#documentation_text texts in
            if texts != texts' then List texts'
            else elem
      | Newline -> elem
      | Enum texts ->
          let texts' = list_map self#documentation_text texts in
            if texts != texts' then Enum texts'
            else elem
      | Title(level, label, text) ->
          let level' = self#documentation_text_title_level level in
          let label' = option_map self#identifier_label label in
          let text' = self#documentation_text text in
            if level != level' || label != label' || text != text' then
              Title(level', label', text')
            else elem
      | Reference(rf, text) ->
          let rf' = self#documentation_reference rf in
          let text' = option_map self#documentation_text text in
            if rf != rf' || text != text' then Reference(rf', text')
            else elem
      | Target(target, body) ->
          let target' = self#documentation_text_target target in
          let body' = self#documentation_text_target_body body in
            if target != target' || body != body' then Target(target', body')
            else elem
      | Special sr ->
          let sr' = self#documentation_special sr in
            if sr != sr' then Special sr'
            else elem

  method documentation_text_raw raw = raw
  method documentation_text_code code = code
  method documentation_text_precode precode = precode
  method documentation_text_verbatim verbatim = verbatim
  method documentation_text_title_level level = level
  method documentation_text_target target = target
  method documentation_text_target_body body = body

  method documentation_text text =
    list_map self#documentation_text_element text

  method documentation_tag tag =
    let open Documentation in
      match tag with
      | Author author ->
          let author' = self#documentation_tag_author author in
            if author != author' then Author author'
            else tag
      | Version version ->
          let version' = self#documentation_tag_version version in
            if version != version' then Version version'
            else tag
      | See(see, txt) ->
          let see' = self#documentation_see see in
          let txt' = self#documentation_text txt in
            if see != see' || txt != txt' then See(see', txt')
            else tag
      | Since since ->
          let since' = self#documentation_tag_since since in
            if since != since' then Since since'
            else tag
      | Before(before, txt) ->
          let before' = self#documentation_tag_before before in
          let txt' = self#documentation_text txt in
            if before != before' || txt != txt' then Before(before', txt')
            else tag
      | Deprecated txt ->
          let txt' = self#documentation_text txt in
            if txt != txt' then Deprecated txt'
            else tag
      | Param(param, txt) ->
          let param' = self#documentation_tag_param param in
          let txt' = self#documentation_text txt in
            if param != param' || txt != txt' then Param(param', txt')
            else tag
      | Raise(raise, txt) ->
          let raise' = self#documentation_tag_raise raise in
          let txt' = self#documentation_text txt in
            if raise != raise' || txt != txt' then Raise(raise', txt')
            else tag
      | Inline -> tag
      | Return txt ->
          let txt' = self#documentation_text txt in
            if txt != txt' then Return txt'
            else tag
      | Tag(name, txt) ->
          let name' = self#documentation_tag_name name in
          let txt' = self#documentation_text txt in
            if name != name' || txt != txt' then Tag(name', txt')
            else tag
      | Canonical(path, reference) ->
          let path' = self#path_module path in
          let reference' = self#reference_module reference in
            if path != path' || reference != reference' then
              Canonical (path', reference')
            else tag

  method documentation_tag_author author = author
  method documentation_tag_version version = version
  method documentation_tag_since since = since
  method documentation_tag_before before = before
  method documentation_tag_param param = param
  method documentation_tag_raise raise = raise
  method documentation_tag_name tag = tag

  method documentation_tags tags =
    list_map self#documentation_tag tags

  method documentation_error_position pos =
    let open Documentation.Error.Position in
    let {line; column} = pos in
    let line' = self#documentation_error_position_line line in
    let column' = self#documentation_error_position_column column in
      if line != line' || column != column' then
        {line = line'; column = column'}
      else pos

  method documentation_error_position_line line = line
  method documentation_error_position_column column = column

  method documentation_error_offset offset =
    let open Documentation.Error.Offset in
    let {start; finish} = offset in
    let start' = self#documentation_error_position start in
    let finish' = self#documentation_error_position finish in
      if start != start' || finish != finish' then
        {start = start'; finish = finish'}
      else offset

  method documentation_error_location loc =
    let open Documentation.Error.Location in
    let {filename; start; finish} = loc in
    let filename' = self#documentation_error_location_filename filename in
    let start' = self#documentation_error_position start in
    let finish' = self#documentation_error_position finish in
      if filename != filename' || start != start' || finish != finish' then
        {filename = filename'; start = start'; finish = finish'}
      else loc

  method documentation_error_location_filename line = line

  method documentation_error err =
    let open Documentation.Error in
    let {origin; offset; location; message} = err in
    let origin' = self#identifier origin in
    let offset' = self#documentation_error_offset offset in
    let location' = option_map self#documentation_error_location location in
    let message' = self#documentation_error_message message in
    if origin != origin' || offset != offset'
       || location != location' || message != message'
    then
      {origin = origin'; offset = offset';
       location = location'; message = message' }
    else err

  method documentation_error_message message = message

  method documentation_body body =
    let open Documentation in
    let {text; tags} = body in
    let text' = self#documentation_text text in
    let tags' = self#documentation_tags tags in
      if text != text' || tags != tags' then {text = text'; tags = tags'}
      else body

  method documentation doc =
    let open Documentation in
    match doc with
    | Ok body ->
        let body' = self#documentation_body body in
          if body != body' then Ok body'
          else doc
    | Error err ->
        let err' = self#documentation_error err in
          if err != err' then Error err'
          else doc

  method documentation_comment comment =
    let open Documentation in
      match comment with
      | Documentation doc ->
          let doc' = self#documentation doc in
            if doc != doc' then Documentation doc'
            else comment
      | Stop -> comment

end

class virtual ['a] module_ = object (self)

  method virtual identifier_module :
    'a Identifier.module_ -> 'a Identifier.module_

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual reference_module :
    'a Reference.module_ -> 'a Reference.module_

  method virtual decl :
    'a Decl.t -> 'a Decl.t

  method virtual defn :
    'a Defn.t -> 'a Defn.t

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual module_type_expr :
    'a ModuleType.expr -> 'a ModuleType.expr

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method virtual module_type_functor_arg :
    'a FunctorArgument.t option -> 'a FunctorArgument.t option

  method virtual source_decl_map_signature :
    'a Source.Decl_map.Signature.t -> 'a Source.Decl_map.Signature.t

  method module_hidden h = h

  method module_expansion expn =
    let open Module in
    match expn with
    | Not_yet_expanded decl_map ->
        let decl_map' = self#source_decl_map_signature decl_map in
        if decl_map != decl_map' then Not_yet_expanded decl_map'
        else expn
    | AlreadyASig -> AlreadyASig
    | Signature sg ->
        let sg' = self#signature sg in
        if sg != sg' then Signature sg'
        else expn
    | Functor (args, sg) ->
        let args' = list_map self#module_type_functor_arg args in
        let sg' = self#signature sg in
        if args != args' || sg != sg' then Functor(args', sg')
        else expn

  method module_decl decl =
    let open Module in
      match decl with
      | Alias p ->
          let p' = self#path_module p in
            if p != p' then Alias p'
            else decl
      | ModuleType expr ->
          let expr' = self#module_type_expr expr in
            if expr != expr' then ModuleType expr'
            else decl

  method module_ md =
    let open Module in
    let {id; doc; decl; defn; type_;
         expansion; canonical; hidden; display_type} =
      md
    in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let decl' = option_map self#decl decl in
    let defn' = option_map self#defn defn in
    let type' = self#module_decl type_ in
    let expansion' = self#module_expansion expansion in
    let canonical' =
      option_map (pair_map self#path_module self#reference_module) canonical
    in
    let hidden' = self#module_hidden hidden in
    let display_type' = option_map self#module_decl display_type in
      if id != id' || doc != doc' || decl != decl'  || defn != defn'
         || type_ != type' || expansion != expansion'
         || canonical != canonical' || hidden != hidden'
         || display_type != display_type'
      then
        {id = id'; doc = doc'; type_ = type'; decl = decl'; defn = defn';
         expansion = expansion'; canonical = canonical'; hidden = hidden';
         display_type = display_type'}
      else md

  method module_equation eq =
    self#module_decl eq

end

class virtual ['a] module_type = object (self)

  method virtual identifier_module :
    'a Identifier.module_ -> 'a Identifier.module_

  method virtual identifier_module_type :
    'a Identifier.module_type -> 'a Identifier.module_type

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual path_module_type :
    'a Path.module_type -> 'a Path.module_type

  method virtual path_type :
    'a Path.type_ -> 'a Path.type_

  method virtual fragment_module :
    'a Fragment.module_ -> 'a Fragment.module_

  method virtual fragment_type :
    'a Fragment.type_ -> 'a Fragment.type_

  method virtual decl :
    'a Decl.t -> 'a Decl.t

  method virtual defn :
    'a Defn.t -> 'a Defn.t

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual module_decl :
    'a Module.decl -> 'a Module.decl

  method virtual module_equation :
    'a Module.Equation.t -> 'a Module.Equation.t

  method virtual signature :
    'a Signature.t -> 'a Signature.t

  method virtual type_decl_equation :
    'a TypeDecl.Equation.t -> 'a TypeDecl.Equation.t

  method virtual type_decl_param_name :
    string -> string

  method virtual module_expansion :
    'a Module.expansion -> 'a Module.expansion

  method module_type_substitution subst =
    let open ModuleType in
      match subst with
      | ModuleEq(frag, eq) ->
          let frag' = self#fragment_module frag in
          let eq' = self#module_equation eq in
            if frag != frag' || eq != eq' then ModuleEq(frag', eq')
            else subst
      | TypeEq(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeEq(frag', eq')
            else subst
      | ModuleSubst(frag, p) ->
          let frag' = self#fragment_module frag in
          let p' = self#path_module p in
            if frag != frag' || p != p' then
              ModuleSubst(frag', p')
            else subst
      | TypeSubst(frag, eq) ->
          let frag' = self#fragment_type frag in
          let eq' = self#type_decl_equation eq in
            if frag != frag' || eq != eq' then TypeSubst(frag', eq')
            else subst

  method module_type_expr expr =
    let open ModuleType in
      match expr with
      | Path p ->
          let p' = self#path_module_type p in
            if p != p' then Path p'
            else expr
      | Signature sg ->
          let sg' = self#signature sg in
            if sg != sg' then Signature sg'
            else expr
      | Functor(arg, res) ->
          let arg' = self#module_type_functor_arg arg in
          let res' = self#module_type_expr res in
            if arg != arg' || res != res' then Functor(arg', res')
            else expr
      | With(body, substs) ->
          let body' = self#module_type_expr body in
          let substs' = list_map self#module_type_substitution substs in
            if body != body' || substs != substs' then With(body', substs')
            else expr
      | TypeOf decl ->
          let decl' = self#module_decl decl in
            if decl != decl' then TypeOf decl'
            else expr

  method module_type_functor_arg arg =
    match arg with
    | None -> arg
    | Some { FunctorArgument. id; expr; expansion } ->
        let id' = self#identifier_module id in
        let expr' = self#module_type_expr expr in
        let expansion' = self#module_expansion expansion in
          if id != id' || expr != expr' || expansion != expansion' then
            Some {FunctorArgument. id = id'; expr = expr'; expansion = expansion'}
          else arg

  method module_type mty =
    let open ModuleType in
    let {id; doc; decl; defn; expr; expansion} = mty in
    let id' = self#identifier_module_type id in
    let doc' = self#documentation doc in
    let decl' = option_map self#decl decl in
    let defn' = option_map self#defn defn in
    let expr' = option_map self#module_type_expr expr in
    let expansion' = self#module_expansion expansion in
      if id != id' || doc != doc' || decl != decl' || defn != defn'
         || expr != expr' || expansion != expansion'
      then
        {id = id'; doc = doc'; decl = decl'; defn = defn';
         expr = expr'; expansion = expansion'}
      else
        mty
end

class virtual ['a] signature = object (self)

  method virtual documentation_comment :
    'a Documentation.comment -> 'a Documentation.comment

  method virtual module_ :
    'a Module.t -> 'a Module.t

  method virtual module_type :
    'a ModuleType.t -> 'a ModuleType.t

  method virtual type_decl :
    'a TypeDecl.t -> 'a TypeDecl.t

  method virtual extension :
    'a Extension.t -> 'a Extension.t

  method virtual exception_ :
    'a Exception.t -> 'a Exception.t

  method virtual value :
    'a Value.t -> 'a Value.t

  method virtual external_ :
    'a External.t -> 'a External.t

  method virtual class_ :
    'a Class.t -> 'a Class.t

  method virtual class_type :
    'a ClassType.t -> 'a ClassType.t

  method virtual include_:
    'a Include.t -> 'a Include.t

  method signature_item item =
    let open Signature in
      match item with
      | Value v ->
          let v' = self#value v in
            if v != v' then Value v'
            else item
      | External ve ->
          let ve' = self#external_ ve in
            if ve != ve' then External ve'
            else item
      | Type decl ->
          let decl' = self#type_decl decl in
            if decl != decl' then Type decl'
            else item
      | TypExt ext ->
          let ext' = self#extension ext in
            if ext != ext' then TypExt ext'
            else item
      | Exception exn ->
          let exn' = self#exception_ exn in
            if exn != exn' then Exception exn'
            else item
      | Class cls ->
          let cls' = self#class_ cls in
            if cls != cls' then Class cls'
            else item
      | ClassType clty ->
          let clty' = self#class_type clty in
            if clty != clty' then ClassType clty'
            else item
      | Module md ->
          let md' = self#module_ md in
            if md != md' then Module md'
            else item
      | ModuleType mty ->
          let mty' = self#module_type mty in
            if mty != mty' then ModuleType mty'
            else item
      | Include incl ->
          let incl' = self#include_ incl in
            if incl != incl' then Include incl'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method signature sg =
    list_map self#signature_item sg

end

class virtual ['a] include_ = object (self)

  method virtual module_decl :
    'a Module.decl -> 'a Module.decl

  method virtual identifier_signature :
    'a Identifier.signature -> 'a Identifier.signature

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual signature : 'a Signature.t -> 'a Signature.t

  method include_expansion_resolved resolved =
    resolved

  method include_expansion expn =
    let open Include in
    let {resolved; content; decl_map} = expn in
    let resolved' = self#include_expansion_resolved resolved in
    let content' = self#signature content in
      if content != content' || resolved != resolved' then
        {resolved = resolved'; content = content'}
      else expn

  method include_ incl =
    let open Include in
    let {parent; doc; decl; expansion} = incl in
    let parent' = self#identifier_signature parent in
    let doc' = self#documentation doc in
    let decl' = self#module_decl decl in
    let expansion' = self#include_expansion expansion in
      if parent != parent' || doc != doc' || decl != decl' || expansion != expansion' then
        {parent = parent'; doc = doc'; decl = decl'; expansion = expansion'}
      else incl

end

class virtual ['a] type_decl = object (self)

  method virtual identifier_type :
    'a Identifier.type_ -> 'a Identifier.type_

  method virtual identifier_constructor :
    'a Identifier.constructor -> 'a Identifier.constructor

  method virtual identifier_field :
    'a Identifier.field -> 'a Identifier.field

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method type_decl_constructor_argument arg =
    let open TypeDecl.Constructor in
    match arg with
    | Tuple args ->
        let args' = list_map self#type_expr args in
          if args != args' then Tuple args'
          else arg
    | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else arg

  method type_decl_constructor cstr =
    let open TypeDecl.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_constructor id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method type_decl_field field =
    let open TypeDecl.Field in
    let {id; doc; mutable_; type_} = field in
    let id' = self#identifier_field id in
    let doc' = self#documentation doc in
    let mutable' = self#type_decl_field_mutable mutable_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc'
         || mutable_ != mutable' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable'; type_ = type'}
      else field

  method type_decl_field_mutable mutable_ = mutable_

  method type_decl_representation repr =
    let open TypeDecl.Representation in
      match repr with
      | Variant cstrs ->
          let cstrs' = list_map self#type_decl_constructor cstrs in
            if cstrs != cstrs' then Variant cstrs'
            else repr
      | Record fields ->
          let fields' = list_map self#type_decl_field fields in
            if fields != fields' then Record fields'
            else repr
      | Extensible -> repr

  method type_decl_variance variance = variance

  method type_decl_param_desc desc =
    let open TypeDecl in
      match desc with
      | Any -> desc
      | Var name ->
          let name' = self#type_decl_param_name name in
            if name != name' then Var name'
            else desc

  method type_decl_param_name name = name

  method type_decl_param param =
    let desc, var = param in
    let desc' = self#type_decl_param_desc desc in
    let var' = option_map self#type_decl_variance var in
      if desc != desc' || var != var' then (desc', var')
      else param

  method type_decl_equation eq =
    let open TypeDecl.Equation in
    let {params; private_; manifest; constraints} = eq in
    let params' = list_map self#type_decl_param params in
    let private' = self#type_decl_private private_ in
    let manifest' = option_map self#type_expr manifest in
    let constraints' = list_map self#type_decl_constraint constraints in
      if params != params' || private_ != private'
         || manifest != manifest' || constraints != constraints'
      then
        {params = params'; private_ = private';
         manifest = manifest'; constraints = constraints'}
      else eq

  method type_decl_private priv = priv

  method type_decl_constraint cstr =
    let typ1, typ2 = cstr in
    let typ1' = self#type_expr typ1 in
    let typ2' = self#type_expr typ2 in
      if typ1 != typ1' || typ1 != typ1' then (typ1', typ2')
      else cstr

  method type_decl decl =
    let open TypeDecl in
    let {id; doc; equation; representation = repr} = decl in
    let id' = self#identifier_type id in
    let doc' = self#documentation doc in
    let equation' = self#type_decl_equation equation in
    let repr' =
      option_map self#type_decl_representation repr
    in
      if id != id' || doc != doc'
         || equation != equation' || repr != repr'
      then
        {id = id'; doc = doc';
         equation = equation'; representation = repr'}
      else decl

end

class virtual ['a] extension = object (self)

  method virtual identifier_extension :
    'a Identifier.extension -> 'a Identifier.extension

  method virtual path_type :
    'a Path.type_ -> 'a Path.type_

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual type_decl_private :
    bool -> bool

  method virtual type_decl_constructor_argument :
    'a TypeDecl.Constructor.argument -> 'a TypeDecl.Constructor.argument

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method extension_constructor cstr =
    let open Extension.Constructor in
    let {id; doc; args; res} = cstr in
    let id' = self#identifier_extension id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else cstr

  method extension ext =
    let open Extension in
    let {type_path; doc; type_params; private_; constructors} = ext in
    let type_path' = self#path_type type_path in
    let doc' = self#documentation doc in
    let type_params' = list_map self#type_decl_param type_params in
    let private' = self#type_decl_private private_ in
    let constructors' = list_map self#extension_constructor constructors in
      if type_path != type_path' || doc != doc' || type_params != type_params'
         || private_ != private' || constructors != constructors'
      then
        {type_path = type_path'; doc = doc'; type_params = type_params';
         private_ = private'; constructors = constructors'}
      else ext

end

class virtual ['a] exception_ = object (self)

  method virtual identifier_exception :
    'a Identifier.exception_ -> 'a Identifier.exception_

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method virtual type_decl_constructor_argument :
    'a TypeDecl.Constructor.argument -> 'a TypeDecl.Constructor.argument

  method exception_ exn =
    let open Exception in
    let {id; doc; args; res} = exn in
    let id' = self#identifier_exception id in
    let doc' = self#documentation doc in
    let args' = self#type_decl_constructor_argument args in
    let res' = option_map self#type_expr res in
      if id != id' || doc != doc' || args != args' || res != res' then
        {id = id'; doc = doc'; args = args'; res = res'}
      else exn

end

class virtual ['a] value = object (self)

  method virtual identifier_value :
    'a Identifier.value -> 'a Identifier.value

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method value v =
    let open Value in
    let {id; doc; type_} = v in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || type_ != type' then
        {id = id'; doc = doc'; type_ = type'}
      else v

end

class virtual ['a] external_ = object (self)

  method virtual identifier_value :
    'a Identifier.value -> 'a Identifier.value

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method external_ ve =
    let open External in
    let {id; doc; type_; primitives} = ve in
    let id' = self#identifier_value id in
    let doc' = self#documentation doc in
    let type' = self#type_expr type_ in
    let primitives' = list_map self#external_primitive primitives in
      if id != id' || doc != doc'
         || type_ != type' || primitives != primitives'
      then
        {id = id'; doc = doc'; type_ = type'; primitives = primitives'}
      else ve

  method external_primitive prim = prim

end

class virtual ['a] class_ = object (self)

  method virtual identifier_class :
    'a Identifier.class_ -> 'a Identifier.class_

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_type_expr :
    'a ClassType.expr -> 'a ClassType.expr

  method virtual type_expr_label :
    TypeExpr.label -> TypeExpr.label

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method virtual class_signature :
    'a ClassSignature.t -> 'a ClassSignature.t

  method class_decl decl =
    let open Class in
      match decl with
      | ClassType expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then ClassType expr'
            else decl
      | Arrow(lbl, typ, body) ->
          let lbl' = option_map self#type_expr_label lbl in
          let typ' = self#type_expr typ in
          let body' = self#class_decl body in
            if lbl != lbl' || typ != typ' || body != body' then
              Arrow(lbl', typ', body')
            else decl

  method class_ cls =
    let open Class in
    let {id; doc; virtual_; params; type_; expansion} = cls in
    let id' = self#identifier_class id in
    let doc' = self#documentation doc in
    let virtual' = self#class_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let type' = self#class_decl type_ in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || type_ != type' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; type_ = type'; expansion = expansion'}
      else cls

  method class_virtual virt = virt

end

class virtual ['a] class_type = object (self)

  method virtual identifier_class_type :
    'a Identifier.class_type -> 'a Identifier.class_type

  method virtual path_class_type :
    'a Path.class_type -> 'a Path.class_type

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_decl_param :
    TypeDecl.param -> TypeDecl.param

  method virtual class_signature :
    'a ClassSignature.t -> 'a ClassSignature.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method class_type_expr expr =
    let open ClassType in
      match expr with
      | Constr(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else expr
      | Signature csig ->
          let csig' = self#class_signature csig in
            if csig != csig' then Signature csig'
            else expr

  method class_type clty =
    let open ClassType in
    let {id; doc; virtual_; params; expr; expansion} = clty in
    let id' = self#identifier_class_type id in
    let doc' = self#documentation doc in
    let virtual' = self#class_type_virtual virtual_ in
    let params' = list_map self#type_decl_param params in
    let expr' = self#class_type_expr expr in
    let expansion' = option_map self#class_signature expansion in
      if id != id' || doc != doc' || virtual_ != virtual'
         || params != params' || expr != expr' || expansion != expansion'
      then
        {id = id'; doc = doc'; virtual_ = virtual';
         params = params'; expr = expr'; expansion = expansion'}
      else clty

  method class_type_virtual virt = virt

end

class virtual ['a] class_signature = object (self)

  method virtual documentation_comment :
    'a Documentation.comment -> 'a Documentation.comment

  method virtual class_type_expr :
    'a ClassType.expr -> 'a ClassType.expr

  method virtual method_ :
    'a Method.t -> 'a Method.t

  method virtual instance_variable :
    'a InstanceVariable.t -> 'a InstanceVariable.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method class_signature_item item =
    let open ClassSignature in
      match item with
      | InstanceVariable inst ->
          let inst' = self#instance_variable inst in
            if inst != inst' then InstanceVariable inst'
            else item
      | Method meth ->
          let meth' = self#method_ meth in
            if meth != meth' then Method meth'
            else item
      | Constraint(typ1, typ2) ->
          let typ1' = self#type_expr typ1 in
          let typ2' = self#type_expr typ2 in
            if typ1 != typ1' || typ1 != typ1' then Constraint(typ1', typ2')
            else item
      | Inherit expr ->
          let expr' = self#class_type_expr expr in
            if expr != expr' then Inherit expr'
            else item
      | Comment com ->
          let com' = self#documentation_comment com in
            if com != com' then Comment com'
            else item

  method class_signature csig =
    let open ClassSignature in
    let {self = slf; items} = csig in
    let slf' = option_map self#type_expr slf in
    let items' = list_map self#class_signature_item items in
      if slf != slf' || items != items' then
        {self = slf'; items = items'}
      else csig

end

class virtual ['a] method_ = object (self)

  method virtual identifier_method :
    'a Identifier.method_ -> 'a Identifier.method_

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method method_ meth =
    let open Method in
    let {id; doc; private_; virtual_; type_} = meth in
    let id' = self#identifier_method id in
    let doc' = self#documentation doc in
    let private' = self#method_private private_ in
    let virtual' = self#method_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || private_ != private'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; private_ = private';
         virtual_ = virtual'; type_ = type'}
      else meth

  method method_private priv = priv

  method method_virtual virt = virt

end

class virtual ['a] instance_variable = object (self)

  method virtual identifier_instance_variable :
    'a Identifier.instance_variable -> 'a Identifier.instance_variable

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual type_expr :
    'a TypeExpr.t -> 'a TypeExpr.t

  method instance_variable meth =
    let open InstanceVariable in
    let {id; doc; mutable_; virtual_; type_} = meth in
    let id' = self#identifier_instance_variable id in
    let doc' = self#documentation doc in
    let mutable' = self#instance_variable_mutable mutable_ in
    let virtual' = self#instance_variable_virtual virtual_ in
    let type' = self#type_expr type_ in
      if id != id' || doc != doc' || mutable_ != mutable'
         || virtual_ != virtual' || type_ != type'
      then
        {id = id'; doc = doc'; mutable_ = mutable';
         virtual_ = virtual'; type_ = type'}
      else meth

  method instance_variable_mutable mut = mut

  method instance_variable_virtual virt = virt

end

class virtual ['a] type_expr = object (self)

  method virtual path_module_type :
    'a Path.module_type -> 'a Path.module_type

  method virtual path_type :
    'a Path.type_ -> 'a Path.type_

  method virtual path_class_type :
    'a Path.class_type -> 'a Path.class_type

  method virtual fragment_type :
    'a Fragment.type_ -> 'a Fragment.type_

  method type_expr_variant_kind kind = kind

  method type_expr_variant_element elem =
    let open TypeExpr.Variant in
      match elem with
      | Type typ ->
          let typ' = self#type_expr typ in
            if typ != typ' then Type typ'
            else elem
      | Constructor(name, const, args) ->
          let name' = self#type_expr_variant_constructor_name name in
          let const' = self#type_expr_variant_constructor_const const in
          let args' = list_map self#type_expr args in
            if name != name' || const != const' || args != args' then
              Constructor(name', const', args')
            else elem

  method type_expr_variant_constructor_name name = name

  method type_expr_variant_constructor_const const = const

  method type_expr_variant var =
    let open TypeExpr.Variant in
    let {kind; elements} = var in
    let kind' = self#type_expr_variant_kind kind in
    let elements' = list_map self#type_expr_variant_element elements in
      if kind != kind' || elements != elements' then
        {kind = kind'; elements = elements'}
      else var

  method type_expr_object_method meth =
    let open TypeExpr.Object in
    let {name; type_} = meth in
    let name' = self#type_expr_object_method_name name in
    let type' = self#type_expr type_ in
      if name != name' || type_ != type' then
        {name = name'; type_ = type'}
      else meth

  method type_expr_object_method_name name = name

  method type_expr_object_field fld =
    let open TypeExpr.Object in
    match fld with
    | Method meth ->
        let meth' = self#type_expr_object_method meth in
          if meth != meth' then Method meth' else fld
    | Inherit typ ->
        let typ' = self#type_expr typ in
          if typ != typ' then Inherit typ' else fld

  method type_expr_object obj =
    let open TypeExpr.Object in
    let {fields; open_} = obj in
    let fields' = list_map self#type_expr_object_field fields in
    let open' = self#type_expr_object_open open_ in
      if fields != fields' || open_ != open' then
        {fields = fields'; open_ = open'}
      else obj

  method type_expr_object_open opn = opn

  method type_expr_package_substitution subst =
    let frag, typ = subst in
    let frag' = self#fragment_type frag in
    let typ' = self#type_expr typ in
      if frag != frag' || typ != typ' then (frag', typ')
      else subst

  method type_expr_package pkg =
    let open TypeExpr.Package in
    let {path; substitutions = substs} = pkg in
    let path' = self#path_module_type path in
    let substs' = list_map self#type_expr_package_substitution substs in
      if path != path' || substs != substs' then
        {path = path'; substitutions = substs'}
      else pkg

  method type_expr_label lbl =
    let open TypeExpr in
      match lbl with
      | Label name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Label name'
            else lbl
      | Optional name ->
          let name' = self#type_expr_label_name name in
            if name != name' then Optional name'
            else lbl

  method type_expr_label_name name = name

  method type_expr typ =
    let open TypeExpr in
      match typ with
      | Var name ->
          let name' = self#type_expr_var_name name in
            if name != name' then Var name'
            else typ
      | Any -> typ
      | Alias(body, name) ->
          let body' = self#type_expr body in
          let name' = self#type_expr_var_name name in
            if body != body' || name != name' then Alias(body', name')
            else typ
      | Arrow(lbl, arg, res) ->
          let lbl' = option_map self#type_expr_label lbl in
          let arg' = self#type_expr arg in
          let res' = self#type_expr res in
            if lbl != lbl' || arg != arg' || res != res' then Arrow(lbl', arg', res')
            else typ
      | Tuple typs ->
          let typs' = list_map self#type_expr typs in
            if typs != typs' then Tuple typs'
            else typ
      | Constr(p, params) ->
          let p' = self#path_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Constr(p', params')
            else typ
      | Variant var ->
          let var' = self#type_expr_variant var in
            if var != var' then Variant var'
            else typ
      | Object obj ->
          let obj' = self#type_expr_object obj in
            if obj != obj' then Object obj'
            else typ
      | Class(p, params) ->
          let p' = self#path_class_type p in
          let params' = list_map self#type_expr params in
            if p != p' || params != params' then Class(p', params')
            else typ
      | Poly(vars, body) ->
          let vars' = list_map self#type_expr_var_name vars in
          let body' = self#type_expr body in
            if vars != vars' || body != body' then Poly(vars', body')
            else typ
      | Package pkg ->
          let pkg' = self#type_expr_package pkg in
            if pkg != pkg' then Package pkg'
            else typ

  method type_expr_var_name name = name

end

class virtual ['a] unit = object (self)

  method virtual root : 'a -> 'a

  method virtual identifier_module :
    'a Identifier.module_ -> 'a Identifier.module_

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method virtual signature :
    'a Signature.t -> 'a Signature.t

  method unit_import import =
    let open Unit.Import in
      match import with
      | Unresolved(name, digest) ->
          let name' = self#unit_import_name name in
          let digest' = option_map self#unit_import_digest digest in
            if name != name' || digest != digest' then
              Unresolved(name', digest')
            else import
      | Resolved r ->
          let r' = self#root r in
            if r != r' then Resolved r'
            else import

  method unit_import_name name = name

  method unit_import_digest digest = digest

  method unit_source source =
    let open Unit.Source in
    let {file; build_dir; digest} = source in
    let file' = self#unit_source_file file in
    let build_dir' = self#unit_source_build_dir build_dir in
    let digest' = self#unit_source_digest digest in
      if file != file' || build_dir != build_dir' || digest != digest' then
        {file = file'; build_dir = build_dir'; digest = digest'}
      else source

  method unit_source_file file = file

  method unit_source_build_dir build_dir = build_dir

  method unit_source_digest digest = digest

  method unit_packed_item item =
    let open Unit.Packed in
    let {id; path} = item in
    let id' = self#identifier_module id in
    let path' = self#path_module path in
      if id != id' || path != path' then { id = id'; path = path' }
      else item

  method unit_packed items =
    list_map self#unit_packed_item items

  method unit_content content =
    let open Unit in
      match content with
      | Module items ->
          let items' = self#signature items in
            if items != items' then Module items'
            else content
      | Pack items ->
          let items' = self#unit_packed items in
            if items' != items then Pack items'
            else content

  method unit unit =
    let open Unit in
    let {id; doc; digest; imports;
         source; interface; hidden; content; expansion} = unit
    in
    let id' = self#identifier_module id in
    let doc' = self#documentation doc in
    let digest' = self#unit_digest digest in
    let imports' = list_map self#unit_import imports in
    let source' = option_map self#unit_source source in
    let interface' = self#unit_interface interface in
    let hidden' = self#unit_hidden hidden in
    let content' = self#unit_content content in
    let expansion' = option_map self#signature expansion in
      if id != id' || doc != doc' || digest != digest'
         || imports != imports' || source != source'
         || interface != interface' || hidden != hidden'
         || content != content' || expansion != expansion'
      then
        {id = id'; doc = doc'; digest = digest';
         imports = imports'; source = source';
         interface = interface'; hidden = hidden';
         content = content'; expansion = expansion'}
      else unit

  method unit_digest digest = digest

  method unit_interface intf = intf

  method unit_hidden hidden = hidden

end

class virtual ['a] source = object

  method virtual path_module :
    'a Path.module_ -> 'a Path.module_

  method virtual path_module_type :
    'a Path.module_type -> 'a Path.module_type

  method virtual path_type :
    'a Path.type_ -> 'a Path.type_

  method virtual path_class_type :
    'a Path.class_type -> 'a Path.class_type

  method virtual reference_constructor :
    'a Reference.constructor -> 'a Reference.constructor

  method virtual reference_field :
    'a Reference.field -> 'a Reference.field

  method virtual reference_value :
    'a Reference.value -> 'a Reference.value

  method virtual reference_class :
    'a Reference.class_ -> 'a Reference.class_

  method virtual reference_method :
    'a Reference.method_ -> 'a Reference.method_

  method virtual reference_instance_variable :
    'a Reference.instance_variable -> 'a Reference.instance_variable

  method virtual declaration :
    'a Declaration.t -> 'a Declaration.t

  method virtual definition :
    'a Definition.t -> 'a Definition.t

  method source_file_name name = name

  method source_file_build_dir dir = dir

  method source_file_digest digest = digest

  method source_file file =
    let open Source.File in
    let {name; build_dir; digest} = file in
    let name' = self#source_file_name name in
    let build_dir' = self#source_file_build_dir build_dir in
    let digest' = self#source_file_digest digest in
      if name != name' || build_dir != build_dir' || digest != digest' then
        {name = name'; build_dir = build_dir'; digest = digest'}
      else file

  method source_position_line line = line

  method source_position_column column = column

  method source_position position =
    let open Source.Position in
    let {line; column} = position in
    let line' = self#source_position_line line in
    let column' = self#source_position_column column in
      if line != line' || column != column' then
        {line = line'; column = column'}
      else position

  method source_location location =
    let open Source.Location in
    let {start; finish} = location in
    let start' = self#source_position start in
    let finish' = self#source_position finish in
      if start != start' || finish != finish' then
        {start = start'; finish = finish'}
      else location

  method source_use_path use_path =
     let open Source.Use in
      match use_path with
      | Module p ->
          let p' = self#path_module p in
          if p != p' then Module p'
          else use_path
      | Module_type p ->
          let p' = self#path_module_type p in
          if p != p' then Module_type p'
          else use_path
      | Type p ->
          let p' = self#path_type p in
          if p != p' then Type p'
          else use_path
      | Constructor r ->
          let r' = self#reference_constructor r in
          if r != r' then Constructor r'
          else use_path
      | Field r ->
          let p' = self#reference_field p in
          if p != p' then Field p'
          else use_path
      | Value r ->
          let p' = self#reference_value p in
          if p != p' then Value p'
          else use_path
      | Class r ->
          let p' = self#reference_class p in
          if p != p' then Class p'
          else use_path
      | Class_type p ->
          let p' = self#path_class_type p in
          if p != p' then Class_type p'
          else use_path
      | Method r ->
          let r' = self#reference_method r in
          if r != r' then Method r'
          else use_path
      | Instance_variable r ->
          let r' = self#reference_instance_variable r in
          if r != r' then Instance_variable r'
          else use_path

  method source_use use =
    let open Source.Use in
    let {path; location} = use in
    let path' = self#source_use_path path in
    let location' = self#source_location location in
      if path != path' || location != location' then
        {path = path'; location = location'}
      else use

  method source_declaration declaration =
    let open Source.Declaration in
    let {index; location} = declaration in
    let index' = self#declaration_index index in
    let location' = self#source_location location in
      if index != index' || location != location' then
        {index = index'; location = location'}
      else declaration

  method source_definition definition =
    let open Source.Definition in
    let {index; location} = definition in
    let index' = self#definition_index index in
    let location' = self#source_location location in
      if index != index' || location != location' then
        {index = index'; location = location'}
      else definition

  method source_interface interface =
    let open Source.Interface in
    let {file; uses; declarations} = interface in
    let file' = self#source_file file in
    let uses' = list_map self#source_use uses in
    let declarations' = list_map self#source_declaration uses in
      if file != file' || uses != uses' || declarations != declarations' then
        {file = file'; uses = uses'; declarations = declarations'}
      else interface

  method source_implementation implementation =
    let open Source.Implementation in
    let {file; uses; definitions} = implementation in
    let file' = self#source_file file in
    let uses' = list_map self#source_use uses in
    let definitions' = list_map self#source_definition uses in
      if file != file' || uses != uses' || definitions != definitions' then
        {file = file'; uses = uses'; definitions = definitions'}
      else implementation

  method source source =
    let open Source in
    let {interface; implementation} = source in
    let interface' = option_map self#source_interface interface in
    let implementation' =
      option_map self#source_implementation implementation
    in
      if interface != interface' || implementation != implementation' then
        {interface = interface'; implementation = implementation'}
      else source

  inherit ['a] Source.Decl.map

end

class virtual ['a] page = object (self)

  method virtual identifier_page :
    'a Identifier.page -> 'a Identifier.page

  method virtual documentation :
    'a Documentation.t -> 'a Documentation.t

  method page page =
    let open Page in
    let {name; content; digest} = page in
    let name' = self#identifier_page name in
    let content' = self#documentation content in
    let digest' = self#page_digest digest in
    if name != name' || content != content' || digest != digest' then
      {name = name'; content = content'; digest = digest'}
    else
      page

  method page_digest digest = digest

end

class virtual ['a] types = object
  inherit ['a] documentation
  inherit ['a] module_
  inherit ['a] module_type
  inherit ['a] signature
  inherit ['a] include_
  inherit ['a] type_decl
  inherit ['a] extension
  inherit ['a] exception_
  inherit ['a] value
  inherit ['a] external_
  inherit ['a] class_
  inherit ['a] class_type
  inherit ['a] class_signature
  inherit ['a] method_
  inherit ['a] instance_variable
  inherit ['a] type_expr
  inherit ['a] unit
  inherit ['a] source
  inherit ['a] page
end
