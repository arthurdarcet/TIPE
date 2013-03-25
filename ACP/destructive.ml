											(* Sélection des couleurs *)

open Types;;
open Divers;;
open Huffman;;

let liste_des_prem_coul pds_max l =									(* Crée la liste des couleurs sélectionnées pour coder  l'image *)
	let rec aux l pds acc =											(* Il faut choisir les couleurs de poids les plus important, tel que le poids total ne dépasse pas pds_max *)
		if pds < pds_max then match l with 
			| []   -> acc
			| t::q -> aux q (pds + t.poids) (t::acc) 
		else acc
	in aux l 0 []
;;

let modifier_tab_inventaire table liste n =							(* Modifie le tableau inventaire pour supprimer les couleurs inutiles	*)
	let	l_chang = ref [] and min_selec = (List.hd liste).poids in
	let chercher_sous_couleurs lettre = let {couleur = {x=x; y=y;} ; poids = _} = lettre in
		for i = max 0 (x-n) to min 255 (x+n) do
		for j = max 0 (y-n) to min 255 (y+n) do					 	(* On cherche pour chaque couleur forte les couleurs proche (en norme infinie) qu'elle peut remplacer *)
			let en_cour = table.(i).(j) in
			if en_cour < min_selec && en_cour <> 0 then 
			begin
				table.(x).(y) <- en_cour + table.(x).(y);
				table.(i).(j) <- 0;
				let c1 = {x=i; y=j;} and c2 = {x=x; y=y} in
				l_chang := (c1,c2)::(!l_chang);						(* On ajoute le remplacement effectué à la liste des changements *)
			end;
		done;done;
	in List.iter chercher_sous_couleurs liste;
	!l_chang
;;

let rec modifier_tab_alphabet tab l = match l with					(* Modifie le tableau alphabet pour copier les codes des couleurs fortes *)
	| [] -> ()														(* dans les cases des couleurs qu'elles remplacent  *)
	| ({x=x1; y=y1},{x=x2; y=y2})::q -> tab.(x1).(y1) <- tab.(x2).(y2);
										modifier_tab_alphabet tab q
;;