open Angstrom

module Document = struct

  (*
    include::intro.adoc[]
   *)
  let includes = ()

end

module Header = struct
  (*
    = Writing posts
    :awestruct-layout: base
    :showtitle:
    :prev_section: defining-frontmatter
    :next_section: creating-pages
   *)
  let header = unit
end

module Paragraph = struct

  let some f =
    f >>| fun x -> Some x

  let newline = function
    | '\n' -> true
    | _ -> false

  let simple_paragraph =
    take_till newline

  let admonition_list = ["NOTE" ; "TIP" ; "IMPORTANT"; "WARNING"; "CAUTION"]

  (*
    TIP: You can add line numbers to source listings by
    adding the word `numbered` in the attribute list
    after the language name.
   *)
  let admonition =
    choice (List.map (fun ad -> string ad <* char ':') admonition_list)

  let parse =
    (option None (some admonition)) >>= fun ad -> simple_paragraph >>= fun p -> return (ad, p)

end


module Anchor = struct
  type t = Inline
         | Block
end

module Link = struct

  module Urn = struct
    type t = Xref
           | Link
  end

  type t = Link
         | Relative of Urn.t

  let between s c b =
    char s *> b <* char c

  let mark b = between '[' ']' b

  let urn = choice [string "link" ; string "xref"]

  let relative = urn

end

Text of Format.effect * string

module Format = struct

  module Style = struct
    type t = Class of string
           | Id of string
  end

  module List = struct
    type t = Unordered
           | Ordered
  end

  type t = Bold
         | Italic
         | Monospace
         | Style of Style.t
         | Supercript
         | Subscript
         | Heading of int
         | Quote
         | Literal
         | Code
         | List
         | Break
         | SmartQuote
         | Admonition
         | Sidebar
         | BlockTitle
         | Include
         | UriRef

  type effect = Format.t list

  let whitespace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false

  (* let chars =
   *   take_while1 (fun c -> !whitespace c) *)

  let around c b =
    let scan_until = peek_char >>= fun x
    char c *>
    char c *> b <* char c

  let between s c b =
    char s *> b <* char c

  let bold b = around '*' b

  let bold' = bold chars

  let italic b = around '_' b

  let italic' = italic chars

  let monospace b = around '`' b

  let monospace' = monospace b

  let mark b =
    between '[' ']' b

  let custom m b =
    let mark' = around '#' b >>| b in
    let extract = fun marked -> around '#' b in
    mark m >>| ( extract >>| mark' >>= (fun (style, text) -> (style, text)))

  let super b = around '^' b

  let sub b = around '~' b

end
