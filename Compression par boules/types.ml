														(* Types *)


type couleur  = { rouge:int; vert:int; bleu:int };;			(* les entiers composant une couleur prennent des valeurs entre 0 et 255  ou la valeur  -1 pour une couleur vide *)


type image	  = {	lignes : int; 							(* Type codant une image. chaque pixel de l'image est représenté par une couleur	*)
				    colonnes : int;
					pixels : couleur Queue.t;				(* La suite des pixels est représenté par une queue (First in, first out) *)
				};;
type lettre   = { 	couleur : couleur;						(* Couleurs pondérées pour l'algorithme d'Huffman *)
					poids : int;
				};;
type 'a arbre = | Vide										(* Arbre pour Huffman *)
				| Noeud  of 'a * 'a arbre * 'a arbre ;;


let c_vide = {rouge=(-1); vert=(-1); bleu=(-1)};;