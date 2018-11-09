(* Copyright (C) 2018 Authors of BuckleScript
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

open Ast_helper
type exp = Parsetree.expression

let rec no_need_bound (exp : exp) =
  match exp.pexp_desc with
  | Pexp_ident { txt = Lident _} -> true
  | Pexp_constraint(e,_) -> no_need_bound e
  | _ -> false

let ocaml_obj_id = "__ocaml_internal_obj"

let bound (e : exp) (cb : exp -> _) =
  if no_need_bound e then cb e
  else
    let loc = e.pexp_loc in
    Exp.let_ ~loc Nonrecursive
      [ Vb.mk ~loc (Pat.var ~loc {txt = ocaml_obj_id; loc}) e ]
      (cb (Exp.ident ~loc {txt = Lident ocaml_obj_id; loc}))

let handle_exp_apply
    (e  : exp)
    (self : Bs_ast_mapper.mapper)
    (fn : exp)
    (args : (Ast_compatible.arg_label * Parsetree.expression) list)
  =
  let loc = e.pexp_loc in
  begin match fn.pexp_desc with
    | Pexp_ident {txt = Lident "|."} ->
      (*
        a |. f
        a |. f b c [@bs]  --> f a b c [@bs]
      *)
      begin match args with
        | [ 
#if OCAML_VERSION =~ ">4.03.0" then 
          Nolabel, obj_arg ;
          Nolabel, fn
#else
          "", obj_arg ;
          "", fn
#end            
          ] ->
          let new_obj_arg = self.expr self obj_arg in
          begin match fn with
            | {pexp_desc = Pexp_apply (fn, args); pexp_loc; pexp_attributes} ->
              let fn = self.expr self fn in
              let args = Ext_list.map  args (fun (lab,exp) -> lab, self.expr self exp ) in
              Bs_ast_invariant.warn_unused_attributes pexp_attributes;
              { Parsetree.pexp_desc = Pexp_apply(fn, (Ast_compatible.no_label, new_obj_arg) :: args);
                pexp_attributes = [];
                pexp_loc = pexp_loc}
            | {pexp_desc = Pexp_construct(ctor,None); pexp_loc; pexp_attributes} -> 
              {fn with pexp_desc = Pexp_construct(ctor, Some new_obj_arg)}
            | _ ->
              let try_dispatch_by_tuple =
                Ast_tuple_pattern_flatten.map_open_tuple fn (fun xs tuple_attrs ->
                    bound new_obj_arg @@  fun bounded_obj_arg ->
                    {
                      Parsetree.pexp_desc =
                        Pexp_tuple (
                          Ext_list.map xs (fun (fn : Parsetree.expression) ->
                              match fn with
                              | {pexp_desc = Pexp_apply (fn,args); pexp_loc; pexp_attributes }
                                ->
                                let fn = self.expr self fn in
                                let args = Ext_list.map  args (fun (lab,exp) -> lab, self.expr self exp ) in
                                Bs_ast_invariant.warn_unused_attributes pexp_attributes;
                                { Parsetree.pexp_desc = Pexp_apply(fn, (Ast_compatible.no_label, bounded_obj_arg) :: args);
                                  pexp_attributes = [];
                                  pexp_loc = pexp_loc}
                              | {pexp_desc = Pexp_construct(ctor,None); pexp_loc; pexp_attributes}    
                                -> 
                                {fn with pexp_desc = Pexp_construct(ctor, Some bounded_obj_arg)}
                              | _ ->
                                Ast_compatible.app1 ~loc:fn.pexp_loc
                                  (self.expr self fn )
                                   bounded_obj_arg
                            ));
                      pexp_attributes = tuple_attrs;
                      pexp_loc = fn.pexp_loc;
                    }
                  ) in
              begin match try_dispatch_by_tuple  with
                | Some x -> x
                | None ->
                  Ast_compatible.app1 ~loc (self.expr self fn) new_obj_arg
              end
          end
        | _ ->
          Location.raise_errorf ~loc
            "invalid |. syntax "
      end

    (* | Pexp_ident  {txt = Lident "##" ; loc}
      ->
      begin match args with
        | [
#if OCAML_VERSION =~ ">4.03.0" then 
           (Nolabel, obj) ;
           (Nolabel, {pexp_desc = Pexp_apply(
                {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                args
              ); pexp_attributes = attrs }
           (* we should warn when we discard attributes *)
           )
#else
           ("", obj) ;
           ("", {pexp_desc = Pexp_apply(
                {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                args
              ); pexp_attributes = attrs }
           (* we should warn when we discard attributes *)
           )
#end           
          ] -> (* f##(paint 1 2 ) *)
          (* gpr#1063 foo##(bar##baz) we should rewrite (bar##baz)
             first  before pattern match.
             currently the pattern match is written in a top down style.
             Another corner case: f##(g a b [@bs])
          *)
          Bs_ast_invariant.warn_unused_attributes attrs ;
          {e with pexp_desc = Ast_util.method_apply loc self obj name args}
        | [
#if OCAML_VERSION =~ ">4.03.0" then           
          (Nolabel, obj) ;
          (Nolabel,
            {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
           )  (* f##paint  *)
#else
          ("", obj) ;
           ("",
            {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
           )  (* f##paint  *)
#end           
          ] ->
          { e with pexp_desc =
                     Ast_util.js_property loc (self.expr self obj) name
          }

        | _ ->
          Location.raise_errorf ~loc
            "Js object ## expect syntax like obj##(paint (a,b)) "
      end
    (* we can not use [:=] for precedece cases
       like {[i @@ x##length := 3 ]}
       is parsed as {[ (i @@ x##length) := 3]}
       since we allow user to create Js objects in OCaml, it can be of
       ref type
       {[
         let u = object (self)
           val x = ref 3
           method setX x = self##x := 32
           method getX () = !self##x
         end
       ]}
    *)
    | Pexp_ident {txt = Lident "#=" } ->
      begin match args with
        | [
#if OCAML_VERSION =~ ">4.03.0" then           
          Nolabel,
           {pexp_desc =
              Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "##"}},
                          [Nolabel, obj;
                           Nolabel, {pexp_desc = Pexp_ident {txt = Lident name}}
                          ]
                         )};
           Nolabel, arg
#else
          "",
           {pexp_desc =
              Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "##"}},
                          ["", obj;
                           "", {pexp_desc = Pexp_ident {txt = Lident name}}
                          ]
                         )};
           "", arg
#end           
          ] ->
          Exp.constraint_ ~loc
            { e with
              pexp_desc =
                Ast_util.method_apply loc self obj
                  (name ^ Literals.setter_suffix) [Ast_compatible.no_label, arg ]  }
            (Ast_literal.type_unit ~loc ())
        | _ -> Bs_ast_mapper.default_mapper.expr self e
      end *)
    | _ ->
      begin match
          Ext_list.exclude_with_val
            e.pexp_attributes 
            Ast_attributes.is_bs with
      | false, _ -> Bs_ast_mapper.default_mapper.expr self e
      | true, pexp_attributes ->
        {e with pexp_desc = Ast_util.uncurry_fn_apply loc self fn args ;
                pexp_attributes }
      end
  end
