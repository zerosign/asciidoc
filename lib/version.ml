type t = { major : int ; minor: int }

let v1_0 = { major = 1; minor = 0 }
let v1_1 = { major = 1; minor = 1 }

let compare x y =
  let c = compare x.major y.major in
  if c <> 0 then c else compare x.minor y.minor

let pp_hum fmt t =
  Format.fprintf fmt "asciidoc %d.%d" t.major t.minor
