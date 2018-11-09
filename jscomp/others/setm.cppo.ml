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

(** This module is {!Belt.MutableSet} specialized with key type to be a primitive type.
    It is more efficient in general, the  API is the same with {!Belt_MutableSet} except its key type is fixed,
    and identity is not needed(using the built-in one) 
*)

#ifdef TYPE_INT
module I = Belt_internalSetInt
module S = Belt_SortArrayInt
#elif defined TYPE_STRING
module I = Belt_internalSetString
module S = Belt_SortArrayString
#else
  [%error "unknown type"]
#endif  
module N = Belt_internalAVLset
module A = Belt_Array 

type value = I.value
(** The type of the set elements. *)
             

type t = {
  mutable data : I.t
} [@@bs.deriving abstract]
(** The type of sets. *)


let rec remove0 nt (x : value)= 
  let k = N.valueGet nt in 
  if x = k then 
    let l,r = N.(leftGet nt, rightGet nt) in       
    match N.(toOpt l, toOpt r) with 
    | None, _ -> r 
    | _, None -> l 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
  else 
    begin 
      if x < k then 
        match N.toOpt (N.leftGet nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (remove0 l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.rightGet nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (remove0 r x);
          N.return (N.balMutate nt)
    end

let remove d v = 
  let oldRoot = dataGet d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 -> 
  let newRoot = remove0 oldRoot2 v in 
  if newRoot != oldRoot then 
    dataSet d newRoot       

let rec removeMany0 t xs i len  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = remove0 t ele in 
    match N.toOpt u with 
    | None -> N.empty
    | Some t -> removeMany0 t xs (i+1) len
  else N.return t    


let removeMany  (d : t) xs =  
  let oldRoot = dataGet d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    dataSet d  (removeMany0 nt xs 0 len) 
    
let rec removeCheck0  nt (x : value) removed = 
  let k = N.valueGet nt in 
  if x = k then 
    let () = removed := true in  
    let l,r = N.(leftGet nt, rightGet nt) in       
    match N.(toOpt l, toOpt r) with 
    | None, _ -> r 
    | _ , None -> l 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
  else 
    begin 
      if x < k then 
        match N.toOpt (N.leftGet nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeCheck0  l x removed);
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.rightGet nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeCheck0  r x removed);
          N.return (N.balMutate nt)
    end



let removeCheck  (d :  t) v =  
  let oldRoot = dataGet d in 
  match N.toOpt oldRoot with 
  | None -> false 
  | Some oldRoot2 ->
    let removed = ref false in 
    let newRoot = removeCheck0  oldRoot2 v removed in 
    if newRoot != oldRoot then  
      dataSet d newRoot ;   
    !removed

    
let rec addCheck0  t (x : value) added  =   
  match N.toOpt t with 
  | None -> 
    added := true;
    N.singleton x 
  | Some nt -> 
    let k = N.valueGet nt in 
    if x = k then t 
    else
      let l, r = N.(leftGet nt, rightGet nt) in 
      (if x < k then                   
         let ll = addCheck0  l x added in
         N.leftSet nt ll
       else   
         N.rightSet nt (addCheck0 r x added );
      );
      N.return (N.balMutate nt)

let addCheck (m :  t) e = 
  let oldRoot = dataGet m in 
  let added = ref false in 
  let newRoot = addCheck0 oldRoot e added in 
  if newRoot != oldRoot then 
    dataSet m newRoot;
  !added        

let add d k = 
  let oldRoot = dataGet d in 
  let v = I.addMutate oldRoot k in 
  if v != oldRoot then 
    dataSet d v   

  
let addArrayMutate t  xs =       
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v := I.addMutate !v (A.getUnsafe xs i)
  done ;
  !v    

let mergeMany d arr = 
  dataSet d (addArrayMutate (dataGet d) arr)
    


let make  () = t ~data:N.empty

let isEmpty d = 
  N.isEmpty (dataGet d)

let minimum d = 
  N.minimum (dataGet d)

let minUndefined d =
  N.minUndefined (dataGet d)

let maximum d = N.maximum (dataGet d)

let maxUndefined d = N.maxUndefined (dataGet d)

let forEachU d f = N.forEachU (dataGet d) f     
let forEach d f = forEachU d (fun[@bs] a -> f a)
    
let reduceU d acc cb = N.reduceU (dataGet d) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b -> cb a b)    

let everyU d p = N.everyU (dataGet d) p
let every d p = everyU d (fun[@bs] a -> p a)    
let someU d p = N.someU (dataGet d) p   
let some d p = someU d (fun [@bs] a -> p a)
let size d = 
  N.size (dataGet d)
let toList d =
  N.toList (dataGet d)
let toArray d = 
  N.toArray (dataGet d)
  

let fromSortedArrayUnsafe xs =
  t ~data:(N.fromSortedArrayUnsafe xs)    

let checkInvariantInternal d = 
  N.checkInvariantInternal (dataGet d)



let fromArray xs = 
  t  ~data:(I.fromArray xs)

let cmp d0 d1 = 
  I.cmp (dataGet d0) (dataGet d1)
let eq d0 d1 = 
  I.eq (dataGet d0) (dataGet d1)
let get d x = 
  I.get (dataGet d) x
let getUndefined d x =
  I.getUndefined (dataGet d) x
let getExn d x =
  I.getExn (dataGet d) x 

let split d  key =  
  let arr = N.toArray (dataGet d) in 
  let i = S.binarySearch arr key   in   
  let len = A.length arr in 
  if i < 0 then 
    let next = - i -1 in 
    (t
      ~data:(N.fromSortedArrayAux arr 0 next)
    , 
    t
      ~data:(N.fromSortedArrayAux arr next (len - next))
    ), false
  else 
    (t
      ~data:(N.fromSortedArrayAux arr 0 i)
    ,
    t
      ~data:(N.fromSortedArrayAux arr (i+1) (len - i - 1))
      ), true   
  
let keepU d p = 
  t ~data:(N.keepCopyU (dataGet d) p )
let keep d p = keepU d (fun[@bs] a -> p a)
    
let partitionU d p = 
  let a , b = N.partitionCopyU (dataGet d) p in 
  t ~data:a, t ~data:b
let partition d p = partitionU d (fun[@bs] a -> p a)
    
let subset a b = I.subset  (dataGet a) (dataGet b)
let intersect dataa datab  = 
  let dataa, datab = dataGet dataa, dataGet datab in
    match N.toOpt dataa, N.toOpt datab with 
    | None, _ -> make ()
    | _, None -> make ()
    | Some dataa0, Some datab0 ->  
    let sizea, sizeb = 
        N.lengthNode dataa0, N.lengthNode datab0 in          
    let totalSize = sizea + sizeb in 
#ifdef BS_NATIVE
    let tmp = A.makeUninitializedUnsafe totalSize (dataa0 |. N.valueGet) in 
#else
    let tmp = A.makeUninitializedUnsafe totalSize in 
#endif
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    if ((A.getUnsafe tmp (sizea - 1) < 
        A.getUnsafe tmp sizea))
      || 
      (
      (A.getUnsafe tmp (totalSize - 1) <
      A.getUnsafe tmp 0)
      )
       then make ()
    else 
#ifdef BS_NATIVE
    let tmp2 = A.makeUninitializedUnsafe (Pervasives.min sizea sizeb) (dataa0 |. N.valueGet) in 
#else
    let tmp2 = A.makeUninitializedUnsafe (Pervasives.min sizea sizeb) in 
#endif
    let k = S.intersect tmp 0 sizea tmp sizea sizeb tmp2 0  in 
    t ~data:(N.fromSortedArrayAux tmp2 0 k)
  
let diff dataa datab : t = 
  let dataa, datab = dataGet dataa, dataGet datab in
  match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> make ()
  | _, None -> t ~data:(N.copy dataa)
  | Some dataa0, Some datab0 -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in  
    let totalSize = sizea + sizeb in 
#ifdef BS_NATIVE
    let tmp = A.makeUninitializedUnsafe totalSize (dataa0 |. N.valueGet) in 
#else
    let tmp = A.makeUninitializedUnsafe totalSize in 
#endif
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    if ( (A.getUnsafe tmp (sizea - 1)) < 
        (A.getUnsafe tmp sizea))
      ||       
      (A.getUnsafe tmp (totalSize - 1)
      < A.getUnsafe tmp 0) 
       then t ~data:(N.copy dataa) 
    else 
#ifdef BS_NATIVE
    let tmp2 = A.makeUninitializedUnsafe sizea (dataa0 |. N.valueGet) in 
#else
    let tmp2 = A.makeUninitializedUnsafe sizea in 
#endif
    let k = S.diff tmp 0 sizea tmp sizea sizeb tmp2 0  in 
    t ~data:(N.fromSortedArrayAux tmp2 0 k)

let union (dataa : t)  (datab : t) : t = 
  let dataa, datab = dataGet dataa, dataGet datab in
   match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> t ~data:(N.copy datab) 
  | _, None -> t ~data:(N.copy dataa) 
  | Some dataa0, Some datab0 
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in 
    let totalSize = sizea + sizeb in 
#ifdef BS_NATIVE
    let tmp = A.makeUninitializedUnsafe totalSize (dataa0 |. N.valueGet) in 
#else
    let tmp = A.makeUninitializedUnsafe totalSize in 
#endif
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp ;
    if 
      (A.getUnsafe tmp (sizea - 1) < 
      A.getUnsafe tmp sizea)  then 
      t  ~data:(N.fromSortedArrayAux tmp 0 totalSize) 
    else   
#ifdef BS_NATIVE
      let tmp2 = A.makeUninitializedUnsafe totalSize (dataa0 |. N.valueGet) in 
#else
      let tmp2 = A.makeUninitializedUnsafe totalSize in 
#endif
      let k = S.union tmp 0 sizea tmp sizea sizeb tmp2 0  in 
      t ~data:(N.fromSortedArrayAux tmp2 0 k) 
  
let has d x = I.has (dataGet d) x 

let copy d = t ~data:(N.copy (dataGet d)) 
