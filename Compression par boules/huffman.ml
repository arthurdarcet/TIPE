											(* Algorithme d'Huffman *)

open Divers;;
open Types;;

let rec foret_of_list = function									(* Transforme une liste d'élément en une liste d'arbre de sommet ces éléments *)
	| [] -> []
	| lettre::q -> Noeud(lettre,Vide,Vide)::(foret_of_list q);;

let insere n l = 													(* Insere un élément à sa place dans une liste triée *)
	let rec aux l acc = match l,n  with
		| [],noeud								 -> List.rev (noeud::acc)
		| Noeud(a,g1,d1)::queue, Noeud(x,_,_)-> if x.poids < a.poids 
													then (List.rev acc)@(n::l)
													else aux queue (Noeud(a,g1,d1)::acc)
	in aux l [];;

let relier n m =												(* n et m sont deux arbres non vides, que l'on place sous une racine commune, *)
	let (Noeud(a,_,_),Noeud(b,_,_)) = n,m in					(* de poids la somme des poids des racines de n et m *)
	if a.poids < b.poids										(*  La racine de poids le plus faible est placé à gauche *)
		then Noeud({couleur=c_vide; poids=a.poids + b.poids;}, n, m)
		else Noeud({couleur=c_vide; poids=a.poids + b.poids;}, m, n);;

let rec huffman = function										(* Recoit une liste d'arbre, et les relie de manière a créer l'arbre d'Huffman *)
	| [a] -> a
	| a::b::reste -> huffman (insere (relier a b) reste);;

let tbl_of_arbre arbre =										(* Crée un tableau de codage : transcription indexées par les coordonnées des couleurs (rouge).(vert).(bleu) *)
	let table = Array.make_matrix 256 256 (Array.make 256 []) in
	for i=0 to 255 do for j=0 to 255 do
			table.(i).(j) <- Array.make 256 [];					(* Initialisation du tableau à trois dimensions *)
	done; done;
	let rec aux code_en_cours_inv arbre = match arbre with
		| Noeud({couleur={rouge=r; vert=v; bleu=b}; poids=_},Vide,Vide) -> table.(r).(v).(b) <- List.rev code_en_cours_inv
		| Noeud(e,g,d)-> 
			aux (false::code_en_cours_inv) g;
			aux (true::code_en_cours_inv) d;
	in aux [] arbre;
	table;;