									(* Déclaration des types *)


type couleur  = {rouge:int;	vert:int; bleu:int}				(* les entiers composant une couleur prennent des valeurs entre 0 et 255  ou la valeur  -1 pour une couleur vide *)
type couleur_f = {rouge_f:float;  vert_f:float; bleu_f:float};;
type couleur_p = {x:int; y: int} ;;
type couleur_p_f = {x_f:float ; y_f: float} ;;

type image	  = {	lignes : int; 							(* Type codant une image. chaque pixel de l'image est représenté par une couleur	*)
				    colonnes : int;
					pixels : couleur Queue.t;				(* La suite des pixels est représenté par une queue (First in, first out) *)
				};;
type lettre   = { 	couleur : couleur_p;					(* Couleurs pondérées pour l'algorithme d'Huffman *)
					poids : int;
				};;
type 'a arbre = | Vide										(* Arbre pour Huffman *)
				| Noeud  of 'a * 'a arbre * 'a arbre ;;
				
type v_p = {lambda :float ; vecteur : couleur_f};;

type case_acp_f = {v1_f:couleur_f; v2_f:couleur_f; pix_case_f: couleur_p_f Queue.t } ;;
type case_acp = {v1:couleur; v2:couleur; pix_case: couleur_p Queue.t } ;;
type img_acp = {lig_acp:int; col_acp:int; case:int; vecteurs:(couleur*couleur) Queue.t; pix_acp: couleur_p Queue.t};;

let c_vide = {rouge=(-1); vert=(-1); bleu=(-1)};;
let p_vide = {x=(-1); y=(-1)};;
let null = {rouge_f = 0.; vert_f =0. ;  bleu_f = 0. };;
let r = {rouge_f = 1.; vert_f =0. ;  bleu_f = 0. };;
let v = {rouge_f = 0.; vert_f =1. ;  bleu_f = 0. };;
let b = {rouge_f = 0.; vert_f =0. ;  bleu_f = 1. };;
