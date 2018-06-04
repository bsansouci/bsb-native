(* Copyright (C) 2017 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let derivingName = "abstract"
module U = Ast_derive_util
open Ast_helper
type tdcls = Parsetree.type_declaration list

let handle_config (config : Parsetree.expression option) =
  match config with
  | Some config ->
    U.invalid_config config
  | None -> ()

(* see #2337
   TODO: relax it to allow (int -> int [@bs])
*)
let rec checkNotFunciton (ty : Parsetree.core_type) =
  match ty.ptyp_desc with
  | Ptyp_poly (_,ty) -> checkNotFunciton ty
  | Ptyp_alias (ty,_) -> checkNotFunciton ty
  | Ptyp_arrow _ ->
    Location.raise_errorf
      ~loc:ty.ptyp_loc
      "syntactic function type is not allowed when working with abstract bs.deriving, create a named type as work around"
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_tuple _
  | Ptyp_constr _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _ -> ()


let handleTdcl (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      List.exists (fun ({pld_type; pld_attributes} : Parsetree.label_declaration) ->
          Belt_ast_attributes.has_bs_optional pld_attributes
        ) label_declarations in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          let () = checkNotFunciton pld_type in
          (* TODO: explain why *)
          let prim, newLabel =
            match Belt_ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              [label_name], pld_name
            | Some new_name ->
              [new_name], {pld_name with txt = new_name}
          in
          let is_option = Belt_ast_attributes.has_bs_optional pld_attributes in
          (* if is_option then begin print_endline "FUCK"; assert false end; *)
          
          let maker, getter_type =
            if is_option then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              Ast_core_type.opt_arrow pld_loc label_name optional_type maker,
              Typ.arrow ~loc "" core_type optional_type
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
               Typ.arrow ~loc "" core_type pld_type
          in
          let getter = 
              Str.value Nonrecursive [
                Vb.mk 
                  (Pat.var pld_name) 
                  (Exp.constraint_ (Exp.fun_ "" None 
                      (Pat.var {Location.txt = "o"; loc = !default_loc})
                      (Exp.field (Exp.ident {Location.txt = Longident.Lident "o"; loc = !default_loc}) {txt = Longident.Lident pld_name.Location.txt; loc = !default_loc})) getter_type)]
            in
          let acc =
            getter :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              let setter = Str.value Nonrecursive [
                Vb.mk 
                  (Pat.var {loc = label_loc; txt = label_name ^ "Set"}) 
                  (Exp.constraint_ (Exp.fun_ "" None 
                      (Pat.var {Location.txt = "o"; loc = !default_loc})
                      (Exp.fun_ "" None 
                        (Pat.var {Location.txt = "v"; loc = !default_loc})
                        (Exp.setfield 
                          (Exp.ident {Location.txt = Longident.Lident "o"; loc = !default_loc}) 
                          {txt = Longident.Lident pld_name.Location.txt; loc = !default_loc} 
                          (Exp.ident {Location.txt = Longident.Lident "v"; loc = !default_loc}))))
                      setter_type)
                  ] 
                in
              setter :: acc
            else acc in
          acc,
          maker,
          (is_option, newLabel)::labels
        ) label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       (* let myPrims =
        External_process.pval_prim_of_option_labels
          labels
          has_optional_field
        in *)
       let myMaker =
         (* Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:myPrims makeType *)
        (* if has_optional_field then begin print_endline "FUCKFCUK"; assert false end; *)
        Str.value Nonrecursive [
          Vb.mk 
            (Pat.var {loc; txt = type_name}) 
            (Exp.constraint_ (
              Ext_list.fold_right
                (fun (_is_option, arg_name) rest ->
                  (Exp.fun_ arg_name.Asttypes.txt None 
                    (Pat.var arg_name) rest))
                labels
                (Exp.record (Ext_list.fold_right (fun (_, { Asttypes.txt }) rest ->
                  let field_name = {Asttypes.txt = Longident.Lident txt; loc = !default_loc} in
                  (field_name, Exp.ident field_name) :: rest
                ) labels []) None)) makeType)
        ]
        in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclSig (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      List.exists (fun ({pld_type; pld_attributes} : Parsetree.label_declaration) ->
          Belt_ast_attributes.has_bs_optional pld_attributes
        ) label_declarations in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          let () = checkNotFunciton pld_type in
          (* TODO: explain why *)
          let prim, newLabel =
            match Belt_ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              [label_name], pld_name
            | Some new_name ->
              [new_name], {pld_name with txt = new_name}
          in
          let is_option = Belt_ast_attributes.has_bs_optional pld_attributes in
          (* if is_option then begin print_endline "FUCK"; assert false end; *)
          
          let maker, getter_type =
            if is_option then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              Ast_core_type.opt_arrow pld_loc label_name optional_type maker,
              Typ.arrow ~loc "" core_type optional_type
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
               Typ.arrow ~loc "" core_type pld_type
          in
          let getter = 
            Val.mk pld_name
              ~attrs:[]
              ~prim:[] getter_type 
              (* Str.value Nonrecursive [
                Vb.mk 
                  (Pat.var pld_name) 
                  (Exp.constraint_ (Exp.fun_ "" None 
                      (Pat.var {Location.txt = "o"; loc = !default_loc})
                      (Exp.field (Exp.ident {Location.txt = Longident.Lident "o"; loc = !default_loc}) {txt = Longident.Lident pld_name.Location.txt; loc = !default_loc})) getter_type)] *)
            in
          let acc =
            getter :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              Val.mk
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:[]
                ~prim:[] setter_type
              :: acc
            else acc in
          acc,
          maker,
          (is_option, newLabel)::labels
        ) label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       (* let myPrims =
        External_process.pval_prim_of_option_labels
          labels
          has_optional_field
        in *)
       let myMaker =
         Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:[] makeType in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclsInStr tdcls =
  let tdcls, tdcls_sig, code, code_sig =
    List.fold_right (fun tdcl (tdcls, tdcls_sig, sts, code_sig)  ->
        match handleTdcl tdcl with
        (* Don't use the new type declaration `_ntdcl` here because that's an abstract type, which is hiding the implementation
           and we can't hide the implementation because the constructor, getters and setters need to know the implementation.
           We'll hide all of that with a type signature on the whole module being included.
           
                    Ben - June 1st 2018
            *)
          ntdcl, value_descriptions ->
          let open Parsetree in
          (
            tdcl::tdcls,
            ntdcl::tdcls_sig,
            Ext_list.map_append (fun x -> x) value_descriptions sts,
            Ext_list.map_append (function
              | {pstr_loc; pstr_desc = 
                  Pstr_value (_, (({
                    pvb_pat = {ppat_desc = Ppat_var name}; 
                    pvb_expr = {pexp_desc = Pexp_constraint (_, typ)}
                  } as _makerVb) :: []))
                } -> 
                Sig.value (Val.mk ~loc:pstr_loc name typ)
              | _ -> Sig.type_ []
              ) value_descriptions code_sig
          )
      ) tdcls ([],[], [], [])  in
  
  (Str.type_ tdcls :: code, Sig.type_ tdcls_sig :: code_sig)
(* still need perform transformation for non-abstract type*)

let handleTdclsInSig tdcls =
  let tdcls, code =
    List.fold_right (fun tdcl (tdcls, sts)  ->
        match handleTdclSig tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append (fun x -> Sig.value x) value_descriptions sts

      ) tdcls ([],[])  in
  Sig.type_ tdcls :: code
