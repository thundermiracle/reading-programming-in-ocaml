(* Rose tree *)
type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

(* AddressBook XML *)
let address = XBr ("addressbook", [
  XBr ("person", [
    XBr ("name", [XLf (Some "Atsushi Igarashi")]);
    XBr ("tel", [XLf (Some "075-123-4567")]);
  ]);
  XBr ("person", [XLf None]);
  XBr ("person", [XLf None]);
]);;

let rec string_of_xml = function
  XLf None -> ""
  | XLf Some s -> s
  | XBr (tag, xmllist) -> "<" ^ tag ^ ">" ^ string_of_xmllist xmllist ^ "</" ^ tag ^ ">"
  and string_of_xmllist = function
    [] -> ""
    | singleXml::rest -> string_of_xml singleXml ^ string_of_xmllist rest;;

string_of_xml address;;

