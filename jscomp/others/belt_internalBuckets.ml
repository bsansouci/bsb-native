(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(**  Adapted by Authors of BuckleScript 2017                           *)

(* For JS backends, we use [undefined] as default value, so that buckets
   could be allocated lazily
*)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
module C = Belt_internalBucketsType
(* TODO:
   the current implementation relies on the fact that bucket 
   empty value is [undefined] in both places,
   in theory, it can be different 

*)
type ('a,'b) bucket = {
  mutable key : 'a;
  mutable value : 'b;
  mutable next : ('a,'b) bucket C.opt
}  
and ('hash, 'eq, 'a, 'b) t = ('hash, 'eq, ('a,'b) bucket) C.container  
[@@bs.deriving abstract]

module A = Belt_Array


let rec copy ( x : _ t) : _ t= 
  C.container
    ~hash:(C.hashGet x)
    ~eq:(C.eqGet x)
    ~size:(C.sizeGet x)
    ~buckets:(copyBuckets (C.bucketsGet x))
and copyBuckets ( buckets : _ bucket C.opt array) =  
  let len = A.length buckets in 
#if BS_NATIVE then
  let newBuckets = if len > 0 then A.makeUninitializedUnsafe len (A.getUnsafe buckets 0) else [||] in 
#else
  let newBuckets = A.makeUninitializedUnsafe len in 
#end
  for i = 0 to len - 1 do 
    A.setUnsafe newBuckets i 
    (copyBucket (A.getUnsafe buckets i))
  done ;
  newBuckets
and copyBucket c =   
  match C.toOpt c with 
  | None -> c 
  | Some c -> 
    let head = (bucket ~key:(keyGet c) ~value:(valueGet c)
                  ~next:(C.emptyOpt)) in 
    copyAuxCont (nextGet c) head;
    C.return head
and copyAuxCont c prec =       
  match C.toOpt c with 
  | None -> ()
  | Some nc -> 
    let ncopy = 
        bucket ~key:(keyGet nc) ~value:(valueGet nc) ~next:C.emptyOpt in 
    nextSet prec (C.return ncopy) ;
    copyAuxCont (nextGet nc) ncopy




let rec bucketLength accu buckets = 
  match C.toOpt buckets with 
  | None -> accu
  | Some cell -> bucketLength (accu + 1) (nextGet cell)



let rec do_bucket_iter ~f buckets = 
  match C.toOpt buckets with 
  | None ->
    ()
  | Some cell ->
    f (keyGet cell)  (valueGet cell) [@bs]; do_bucket_iter ~f (nextGet cell)

let forEachU h f =
  let d = C.bucketsGet h in
  for i = 0 to A.length d - 1 do
    do_bucket_iter f (A.getUnsafe d i)
  done

let forEach h f = forEachU h (fun [@bs] a b -> f a b)
    
let rec do_bucket_fold ~f b accu =
  match C.toOpt b with
  | None ->
    accu
  | Some cell ->
    do_bucket_fold ~f (nextGet cell) (f accu (keyGet cell) (valueGet cell)  [@bs]) 

let reduceU  h init f =
  let d = C.bucketsGet h in
  let accu = ref init in
  for i = 0 to A.length d - 1 do
    accu := do_bucket_fold ~f (A.getUnsafe d i) !accu
  done;
  !accu

let reduce h init f = reduceU h init (fun [@bs] a b c -> f a b c)


let getMaxBucketLength h =
  A.reduceU (C.bucketsGet h) 0
    (fun[@bs] m b -> 
       let len = bucketLength 0 b in
       Pervasives.max m len)

let getBucketHistogram h =
  let mbl = getMaxBucketLength h in 
  let histo = A.makeByU (mbl + 1) (fun[@bs] _ -> 0) in
  A.forEachU (C.bucketsGet h)
    (fun[@bs] b ->
       let l = bucketLength 0 b in
       A.setUnsafe histo l (A.getUnsafe histo l + 1)
    );
  histo

let logStats h =
  let histogram = getBucketHistogram h in 
#if BS_NATIVE then
  Printf.printf 
    "{\n\tbindings: %d,\n\tbuckets: %d\n\thistogram: %s\n}" 
    (C.sizeGet h) 
    (A.length (C.bucketsGet h))
    (A.reduceU histogram "" (fun[@bs] acc x -> acc ^ (string_of_int x)))
#else
  Js.log [%obj{ bindings = C.sizeGet h;
                buckets = A.length (C.bucketsGet h);
                histogram}]
#end


(** iterate the Buckets, in place remove the elements *)                
let rec filterMapInplaceBucket f h i prec cell =
  let n = nextGet cell in
  begin match f (keyGet cell) (valueGet cell) [@bs] with
    | None ->
      C.sizeSet h (C.sizeGet h - 1); (* delete *)
      begin match C.toOpt n with 
        | Some nextCell -> 
          filterMapInplaceBucket f h i prec nextCell
        | None -> 
          match C.toOpt prec with 
          | None -> A.setUnsafe (C.bucketsGet h) i prec
          | Some cell -> nextSet cell n
      end
    | Some data -> (* replace *)
      let bucket = C.return cell in 
      begin match C.toOpt prec with
        | None -> A.setUnsafe (C.bucketsGet h) i  bucket 
        | Some c -> nextSet cell bucket
      end;
      valueSet cell data;
      match C.toOpt n with 
      | None -> nextSet cell n 
      | Some nextCell -> 
        filterMapInplaceBucket f h i bucket nextCell
  end

let keepMapInPlaceU h f =
  let h_buckets = C.bucketsGet h in
  for i = 0 to A.length h_buckets - 1 do
    let v = A.getUnsafe h_buckets i in 
    match C.toOpt v with 
    | None -> ()
    | Some v -> filterMapInplaceBucket f h i C.emptyOpt v
  done

let keepMapInPlace h f = keepMapInPlaceU h (fun [@bs] a b -> f a b)
    
let rec fillArray i arr cell =  
  A.setUnsafe arr i (keyGet cell, valueGet cell);
  match C.toOpt (nextGet cell) with 
  | None -> i + 1
  | Some v -> fillArray (i + 1) arr v 

#if BS_NATIVE then
let toArray h = 
  let d = C.bucketsGet h in 
  let current = ref 0 in 
  let arr = ref None in
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      let arr = begin match !arr with 
        | None -> 
          let a = A.makeUninitializedUnsafe (C.sizeGet h) (keyGet cell, valueGet cell) in
          arr := Some a;
          a
        | Some arr -> arr
      end in
      current := fillArray !current arr cell
  done;
  match !arr with 
  | None -> [||]
  | Some arr -> arr
#else
let toArray h = 
  let d = C.bucketsGet h in 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe (C.sizeGet h) in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current := fillArray !current arr cell
  done;
  arr 
#end

let rec fillArrayMap i arr cell f =  
  A.setUnsafe arr i (f cell [@bs]);
  match C.toOpt (nextGet cell) with 
  | None -> i + 1
  | Some v -> fillArrayMap (i + 1) arr v f

#if BS_NATIVE then
let linear h f = 
  let d = C.bucketsGet h in 
  let current = ref 0 in 
  let arr = ref None in
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      let arr = begin match !arr with 
        | None -> 
          let a = A.makeUninitializedUnsafe (C.sizeGet h) (f cell [@bs]) in
          arr := Some a;        
          a
        | Some arr -> arr 
      end in 
      current := fillArrayMap !current arr cell f
  done;
  match !arr with
  | None -> [||]
  | Some arr -> arr
#else
let linear h f = 
  let d = C.bucketsGet h in 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe (C.sizeGet h) in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current := fillArrayMap !current arr cell f
  done;
  arr 
#end

let keysToArray h = linear h (fun [@bs] x -> keyGet x)  
let valuesToArray h = linear h (fun [@bs] x -> valueGet x)  
let toArray h = linear h (fun [@bs]x -> keyGet x, valueGet x)



