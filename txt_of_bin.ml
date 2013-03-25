let fois_2 = function (* Multiplit par 2 un entier codé en binaire dans une liste *)
	| 0::a::b::c::d::e::f::g::[] -> [a;b;c;d;e;f;g;0]
	| _ -> failwith "fois_2 : Nombre sous un mauvais format";;
let ajouter_list l m = List.map2 (fun x y -> x+y) l m;;

let rec bin_of_int = function (* transforme un entier en une liste de binaire *)
	| 0 -> [0;0;0;0;0;0;0;0]
	| n -> ajouter_list (fois_2 (bin_of_int (n/2))) [0;0;0;0;0;0;0;n mod 2];;


let txt_of_bin cin cout =
	let fin = open_in_bin cin and fout = open_out cout in
	let rec print_list = function
		| [] -> ()
		| t::q -> output_string fout (string_of_int t); print_list q;
	in
	let rec aux a = print_list (bin_of_int (input_byte fin)); aux a; in
	try aux "" with End_of_file -> ();
;;


let cin = ref "" and cout = ref "";;
let description = "";;
let f1 s = cin := s;;
let f2 s = cout := s;;
let arguments = [("-i", Arg.String(f1),"Entrée");("-o",Arg.String(f2),"Sortie")];;
Arg.parse arguments (fun s -> ();) description;;

txt_of_bin (!cin) (!cout);;