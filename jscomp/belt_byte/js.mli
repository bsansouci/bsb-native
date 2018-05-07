type 'a null

type 'a undefined

val toOpt : 'a null -> 'a option

val fromOpt : 'a option -> 'a undefined

val undefined : 'a undefined

val empty : 'a null

val log : 'a -> unit

module Undefined : sig
  type 'a t = 'a undefined
  
  val return : 'a -> 'a t
  
  val empty : 'a t
  
  val toOpt : 'a t -> 'a option
end

module Exn : sig
  val raiseError : string -> 'a
end
