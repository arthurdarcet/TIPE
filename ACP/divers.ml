											(* Fonctions diverse utilisées dans tous les scripts *)

open Unix;;
open Types;;

let ts_debut = ref (gettimeofday ());;						(* Référence car la valeur doit etre mise à jour dans interface.ml *)

let sous_windows () =										(* Indique si le programme est exécuté sous Windows *)
	try ignore (Unix.nice 1); false
	with invalid_arg -> true;;

let afficher_heure () =										(*  Affiche  le nombre de minutes et de secondes écoulées depuis le début *)
	let ts = gettimeofday () in
	let transf n = if n < 10 then "0"^(string_of_int n) else (string_of_int n)
	in
	let tps = gmtime (ts -. !ts_debut) in
	" "^(transf (tps.tm_min))^":"^(transf (tps.tm_sec))^" "
;;

let afficher s lvl =										(* Affiche le temps écoulé, et un message au niveau lvl : 2*lvl espaces sont placés avant le message *)
	let rec n_espaces = function
		| 0 -> ""
		| n -> " "^(n_espaces (n-1))
	in
	print_endline ((afficher_heure ())^(n_espaces (2*lvl))^s);;

let int_of_bool = function
	| true -> 1
	| false -> 0;;
let bool_of_int = function
	| 1 -> true
	| 0 -> false;;	
	
let int_of_bin l = 											(* Fonctions de transformation d'entier en liste de booléens représentant la décomposition de l'entier en base 2 *)
	let rec aux acc = function
		| [] -> acc
		| t::q -> aux (2*acc + int_of_bool t) q
	in aux 0 l
;;
let bin_of_int n =
	let rec aux acc = function
		| 0 -> acc
		| n -> aux ((n mod 2)::acc) (n/2)
	in 
	if n=0 then [false] else List.map bool_of_int (aux [] n)
;;

let norm l n elt = 											(* Ajoute elt au début de la liste l jusqu'à c e que la taille de l soit n *)
	let rec ajouter_elt l = function
		| 0 -> l
		| n -> ajouter_elt (elt::l) (n-1)
	in ajouter_elt l (n- (List.length l))
;;

let concat_début chaine carac =								(* Ajoute le char 'carac' au début ou à la fin de la string 'chaine' *)
	let ch2 = " "^chaine in
	ch2.[0] <- carac;
	ch2
;;
let concat_fin chaine carac = 
	let ch2 = chaine^" " in 
	ch2.[String.length chaine] <- carac;
	ch2
;;

let list_of_tbl table =										(* Transforme le tableau contenant les poids de chaque couleurs en une liste de lettre, enregistrement {couleur, poids} *)
	let liste = ref [] in
	for i=0 to 255 do 
	for j=0 to 255 do
		if table.(i).(j) <> 0 then liste := {couleur={x=i; y=j} ; poids=table.(i).(j)}::(!liste);
	done;done;
	!liste
;;

let rec tri_fusion l = 										(* Tri fusion en ordre décroissant *)
	let rec separe l l1 l2 = match l with 
		| [] -> (l1,l2)
		| t::q -> separe q l2 (t::l1)
	in
	let rec fusion  = function 
		|([],l2) -> l2
		|(l1,[]) -> l1
		|(a::b,c::d) -> if  a.poids > c.poids 
	  					then a::(fusion (b,c::d))
	  					else c::(fusion (a::b,d))
	in
	let rec tri = function
		| [] -> []
		| [a] -> [a]
		| l ->  let (l1,l2) = (separe l [] []) in 
				fusion ( (tri l1) ,(tri l2))
	in tri l
;;

let copie_queue queue =
	let q1 = Queue.create () and q2 = Queue.create () in
	let rec aux () =
		if Queue.is_empty queue then q1,q2
		else begin
			let e = Queue.pop queue in
			Queue.push e q1;
			Queue.push e q2;
			aux ();
		end
	in
	aux ()
;;

let rec add_list liste queue = match liste with
	| [] -> ()
	| t::q -> Queue.push t queue; add_list q queue;;

let queue_obtenir file n = 											(* Renvoi la liste des n premiers éléments de la queue file (moins si la queue n'est pas assez remplie *)
	let rec aux acc = function
		| 0 -> List.rev acc
		| n -> try aux ((Queue.pop file)::acc) (n-1) with Queue.Empty -> (List.rev acc)
	in aux [] n;;