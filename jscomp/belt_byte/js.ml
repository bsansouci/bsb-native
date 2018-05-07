type 'a null = 'a option

type 'a undefined = 'a option

external toOpt : 'a null -> 'a option = "%identity"

external fromOpt : 'a option -> 'a undefined = "%identity"

let undefined = None

let empty = None

let log a = 
  let _ = Obj.magic a in 
  ()

module Undefined = struct
  type 'a t = 'a undefined
  external return : 'a -> 'a t = "%identity"
  let empty = None
  external toOpt : 'a t -> 'a option = "%identity"
end

module Exn = struct
  let raiseError _str = assert false
end
