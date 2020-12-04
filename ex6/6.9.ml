(* 6.9 *)
type token = PCDATA of string | Open of string | Close of string;;

let token_list = [Open "a"; Open "b"; Close "b"; Open "c"; PCDATA "Hello"; Close "c"; Close "a"];;

type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

let rec xml_of_tokens tokens =
  match parse_sub_tokens "" tokens with
    ([], _) -> XLf None
    | (xml::empty, _) -> xml
  and
  parse_sub_tokens tag = function
    [] -> ([], [])
    | token :: rest -> 
      match token with
      PCDATA s ->
        let (xml_list, rest_tokens) = parse_sub_tokens tag rest in
        (XLf (Some s) :: xml_list, rest_tokens)
      | Open tag_open ->
        let (tag_open_xml_list, tag_open_rest_tokens) = parse_sub_tokens tag_open rest in
        let tag_open_sub_xml_list = if tag_open_xml_list = [] then [XLf None] else tag_open_xml_list in
        let (tag_xml_list, tag_rest_tokens) = parse_sub_tokens tag tag_open_rest_tokens in
        (XBr (tag_open, tag_open_sub_xml_list) :: tag_xml_list, tag_rest_tokens)
      | Close tag_close -> ([], rest)
      ;;

(* xml_of_tokens [];; *)
(* XLf None *)

  #trace parse_sub_tokens;;
xml_of_tokens token_list;;
  #untrace parse_sub_tokens;;
(* XBr ("a", [XBr ("b", [XLf None]); XBr ("c", [XLf (Some "Hello")])]) *)
