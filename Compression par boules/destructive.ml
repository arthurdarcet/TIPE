												(* Sélection des couleurs *)

open Types;;
open Divers;;
open Huffman;;

let liste_des_prem_coul pds_max l =									(* Crée la liste des couleurs sélectionnées pour coder  l'image 	*)
	let rec aux l pds acc =											(* Il faut choisir les couleurs de poids les plus important, tel que le poids total ne dépasse pas pds_max *)
		if pds < pds_max then match l with
			| []   -> acc
			| t::q -> aux q (pds + t.poids) (t::acc) 
		else acc
	in aux l 0 []
;;

let modifier_tab_inventaire table liste n =							(* Modifie le tableau inventaire pour supprimer les couleurs inutiles *)
	let	l_chang = ref [] and min_selec = (List.hd liste).poids in
	let chercher_sous_couleurs {couleur = {rouge=r; vert=v; bleu=b} ; poids = _} =
		for i = max 0 (r-n) to min 255 (r+n) do
		for j = max 0 (v-n) to min 255 (v+n) do					 	(* On cherche pour chaque couleur forte les couleurs proche (en norme infinie) qu'elle peut remplacer *)
		for k = max 0 (b-n) to min 255 (b+n) do
			let en_cour = table.(i).(j).(k) in
			if en_cour < min_selec && en_cour <> 0 then 
			begin
				table.(r).(v).(b) <- en_cour + table.(r).(v).(b);
				table.(i).(j).(k) <- 0;
				let c1 = {rouge=i; vert=j; bleu=k} and c2 = {rouge=r; vert=v; bleu=b} in
				l_chang := (c1,c2) :: (!l_chang);					(* On ajoute le remplacement effectué à la liste des changements *)
			end;
		done;done;done;
	in List.iter chercher_sous_couleurs liste;
	!l_chang
;;

let rec modifier_tab_alphabet tab l = match l with				(* Modifie le tableau alphabet pour copier les codes des couleurs fortes dans les couleurs qu'elles remplacent *)
	| [] -> ()
	| ({rouge=r1;vert=v1;bleu=b1},{rouge=r2;vert=v2;bleu=b2})::q -> tab.(r1).(v1).(b1) <- tab.(r2).(v2).(b2);
																    modifier_tab_alphabet tab q
;;