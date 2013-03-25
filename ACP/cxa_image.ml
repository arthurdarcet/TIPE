										(* Fonction de conversion entre fichier CXA et 'image' *)

	
open Divers;;
open Types;;
open Huffman;;
open Destructive;;
open Acp;;
open Operations_de_base;;

(* Image vers CXA *)

let découpe img taille = 
	let hauteur = img.lignes and largeur = img.colonnes in
	let table = Array.make_matrix (hauteur/taille) (largeur/taille) (Queue.create()) in

	for i=0 to (hauteur/taille)-1 do for j=0 to (largeur/taille)-1 do
		table.(i).(j) <- Queue.create ();
	done; done;
					(* En numérotant les pixels a partir de 0, le nième pix est aux coord   n/largeur , (n mod largeur)de l'image *)
	let classer_pixel n_pixel pix = match  n_pixel/(taille*largeur) , (n_pixel mod largeur)/taille with
		| n,p when n = hauteur/taille && p = largeur/taille		-> Queue.push pix (table.(n-1).(p-1))
		| n,p when n = hauteur/taille 							-> Queue.push pix (table.(n-1).(p))
		| n,p when p = largeur/taille 							-> Queue.push pix (table.(n).(p-1))
		| n,p 													-> Queue.push pix (table.(n).(p))
	in
	let rec ranger pixel_en_cours = match pixel_en_cours with
		| n when n >= (hauteur*largeur) -> () 
		| _ -> classer_pixel pixel_en_cours (Queue.pop img.pixels);
				ranger (pixel_en_cours + 1);
	in
	ranger 0;
	table
;;

let regroupe table_acp taille col =
	let queue_vect = Queue.create() in
	let largeur = (Array.length table_acp.(0)) and hauteur = (Array.length table_acp) in
	for i=0 to hauteur-1 do
	for j=0 to largeur-1 do 
		Queue.push ((table_acp.(i).(j).v1),(table_acp.(i).(j).v2)) queue_vect;
	done; done;
	let queue_img = Queue.create() in
	let rec récupérer n_pixel = 
		( match  n_pixel/(taille*col) , (n_pixel mod col)/taille with
			| n,p when n = hauteur && p = largeur	-> Queue.push (Queue.pop (table_acp.(n-1).(p-1).pix_case)) queue_img;
			| n,p when n = hauteur 					-> Queue.push (Queue.pop (table_acp.(n-1).(p).pix_case)) queue_img; 
			| n,p when p = largeur					-> Queue.push (Queue.pop (table_acp.(n).(p-1).pix_case)) queue_img;
			| n,p 									-> Queue.push (Queue.pop (table_acp.(n).(p).pix_case)) queue_img; );
		récupérer (n_pixel+1);		
	in
	try récupérer 0; with Queue.Empty -> queue_vect , queue_img
;;

let coder_cases table =
	let h = Array.length table and l = Array.length table.(0) and pourc = ref 0. in
	let table_acp = Array.make_matrix h l {v1=c_vide; v2=c_vide; pix_case=Queue.create ()} in
	for i=0 to h-1 do
	for j=0 to l-1 do
		let p,case = acp table.(i).(j) in
		pourc := !pourc +. p/.(float_of_int (h*l));
		table_acp.(i).(j) <- case_acp_of_case_acp_f case;
	done; done;
	!pourc,table_acp
;;

let img_acp_of_image img taille =
	afficher "Decoupe de l'image ..." 2;
	let cases = découpe img taille in
	afficher "Projection des cases ..." 2;
	let pourc,table_acp = coder_cases cases in
	afficher ("Pourcentage d'information perdue : "^(string_of_int (int_of_float (100.*.(1. -. pourc))))^" %") 2;
	afficher "Synthese des projections ..." 2;
	let vect,pix = regroupe table_acp taille img.colonnes in
	{lig_acp=img.lignes; col_acp=img.colonnes; case=taille; vecteurs=vect; pix_acp=pix}
;;

let inventaire file_pixels =												(* Renvoi un tableau à trois dimensions contenant le nombre d'occurence dans l'image *)
	let table = Array.make_matrix 256 256 0 in								(* de la couleur {rouge=i; vert=j; bleu=k} à la case table.(i).(j).(k) *)
	let rec remplit_tab () =
		let {x=i; y=j} = Queue.pop file_pixels in
		table.(i).(j) <- table.(i).(j) + 1; 
		remplit_tab ();
	in
	(try remplit_tab (); with Queue.Empty -> (););
	table
;;
let rec plus_longue_transcription inventaire codage = 						(* Renvoi la longueur maximale des transcriptions qui seront utilisées pour coder l'image *)
	let rec aux max_en_cour = function
		| [] -> max_en_cour
		| {couleur={x=i; y=j}; poids=_}::q -> aux (max max_en_cour (List.length (codage.(i).(j)))) q
	in aux 0 inventaire
;;

let rec alphabet_binaire tbl_codage max list_invent img_bin = match list_invent with (* Ajoute l'alphabet encodé à la queue en construction img_bin *)
	| [] -> ()																		(* Pour chaque élément de la liste des couleurs, on ajoute son code, *)
	| {couleur = {x=i; y=j}; poids=_}::q -> 										(*  et la couleur suivant les conventions CXA *)
		let f n = norm (bin_of_int n) 8 false in
		add_list ((norm (true::(tbl_codage.(i).(j))) (max+1) false)@(f i)@(f j)) img_bin;
		alphabet_binaire tbl_codage max q img_bin;
;;

let vecteurs_binaire vect img_bin =
	let coder_vect (v1,v2) =
		if v2 = {rouge=0; vert=0; bleu=0} then
			false::((norm (bin_of_int v1.rouge) 15 false)@(norm (bin_of_int v1.vert) 15 false))
		else
			let v1_bin = if v1.rouge = 0 
							then false::(norm (bin_of_int v1.vert) 14 false)
							else true::(norm (bin_of_int v1.rouge) 14 false)
			and v2_bin = if v2.bleu = 0 
							then false::(norm (bin_of_int v2.vert) 14 false)
							else true::(norm (bin_of_int v2.bleu) 14 false)
			in true::(v1_bin@v2_bin)
	in let rec traiter_vect () =
		add_list (coder_vect (Queue.pop vect)) img_bin;
		traiter_vect ()
	in
	try traiter_vect () with Queue.Empty -> ()
;;

let image_binaire img table img_bin =											(* Ajoute l'image encodé à la queue en construction img_bin *)
	let rec aux () =
		let {x=i; y=j} = Queue.pop img in
		add_list (table.(i).(j)) img_bin;
		aux ()
	in
	try aux () with Queue.Empty -> ()
;;


let coder_image img taux_compress distance_entre_coul taille_case0 = 			(* Renvoi la liste de booléens correspondant au fichier encodé *)
	afficher "Projection par l'ACP" 1;
	let taille_case = min (min img.lignes img.colonnes) taille_case0 in
	let img_acp = img_acp_of_image img taille_case in
	afficher "Copie de la queue" 1;
	let pix1,pix2 = copie_queue img_acp.pix_acp in								(* Il faut une queue pour creer l'alphabet, puis une queue pour coder l'image *)
	afficher "Inventaire de l'image ..." 1;										(* Elles sont vidées pendant l'opération *)
	let tbl_invent = inventaire pix1 in 
	afficher "Passage de l'inventaire en liste ..." 1;
	let list_invent = tri_fusion (list_of_tbl tbl_invent) in
	afficher "Selection des couleurs ..." 1;
	let list_selec = liste_des_prem_coul (int_of_float (taux_compress*.(float_of_int (img.lignes*img.colonnes)))) list_invent in
	afficher "Modification de l'inventaire ..." 1;	
	let list_changements = modifier_tab_inventaire tbl_invent list_selec distance_entre_coul in
	afficher "Passage de l'inventaire modifié en liste ..." 1;
	let list_invent_modif = List.rev (tri_fusion (list_of_tbl tbl_invent)) in
	afficher ("Nombre de couleurs : "^(string_of_int (List.length list_invent))) 2;
	afficher ("Nombre de couleurs selectionnees : "^(string_of_int (List.length list_selec))) 2;
	afficher ("Nombre de couleurs encodees : "^(string_of_int (List.length list_invent_modif))) 2;
	afficher "Transformation en foret ..." 1;
	let foret = foret_of_list list_invent_modif in
	afficher "Application de l'algorithme d'Huffman ..." 1;
	let arbre_huffman = huffman foret in
	afficher "Transformation en tableau ..." 1;
	let tbl_codage = tbl_of_arbre arbre_huffman in
	afficher "Ajout des couleurs oubliees ..." 1;
	modifier_tab_alphabet tbl_codage list_changements;
	afficher "Encodage des informations de l'image ..." 1;
	let max = plus_longue_transcription list_invent_modif tbl_codage in 	
	let lig = norm (bin_of_int img.lignes) 12 false
	and col = norm (bin_of_int img.colonnes) 12 false
	and max_binaire = norm (bin_of_int max) 8 false
	and case = norm (bin_of_int img_acp.case) 12 false
	and taux_bin = norm (bin_of_int (int_of_float (1000.*.taux_compress))) 10 false
	and dist_bin = norm (bin_of_int distance_entre_coul) 8 false
	and img_bin = Queue.create () in
	add_list (col@lig@max_binaire@case@taux_bin@dist_bin) img_bin;
	afficher "Encodage de l'alphabet ..." 1;
	alphabet_binaire tbl_codage max list_invent_modif img_bin;
	let taille1 = Queue.length img_bin in
	let separateur = norm [] (max+1) false in
	add_list separateur img_bin;
	afficher "Encodage des bases de projections ..." 1;
	vecteurs_binaire img_acp.vecteurs img_bin;
	let taille2 = Queue.length img_bin in
	afficher "Encodage de l'image ..." 1;
	image_binaire pix2 tbl_codage img_bin;
	let taille3 = Queue.length img_bin in
	afficher ("Taille de l'alphabet : "^(string_of_int (taille1/8))^" o") 2;
	afficher ("Taille des bases     : "^(string_of_int ((taille2-taille1)/8))^" o") 2;
	afficher ("Taille de l'image    : "^(string_of_int ((taille3-taille2)/(1024*8)))^" ko") 2;
	img_bin
;;

let cxa_of_bin queue cout = 															(* Ecrit dans le fichier situé au chemin cout la liste de booléens liste	*)
	let fich = open_out_bin cout in
	let rec aux () =
		let l1 = queue_obtenir queue 8 in
		if l1 = [] then ()
		else begin
			let l2 = List.rev (norm (List.rev l1) 8 false) in							(* On ajoute des zéros à la fin de l1 pour gérer la fin du fichier *)
			output_byte fich (int_of_bin l2); 											(* car on est obligé d'écrire octet par octet : 8 bits par 8 bits *)
			aux ();
		end
	in
	aux ();
	flush fich;
	close_out fich;
;;

(* CXA vers image *)
let norm8 l = norm l 8 false;;

let rec arbre_assigner arbre coord elt = match arbre,coord with		(* Fonction de créations des arbres, les coordonnées sont des listes de booléens, false pour gauche, true pour droite *)
	| Vide, [] 					-> Noeud(elt,Vide,Vide)
	| Vide, true::q 			-> Noeud(p_vide,Vide,(arbre_assigner Vide q elt))
	| Vide, false::q 			-> Noeud(p_vide,(arbre_assigner Vide q elt),Vide)
	| Noeud(a,g,d), true::q 	-> Noeud(a,g,(arbre_assigner d q elt))
	| Noeud(a,g,d), false::q 	-> Noeud(a,(arbre_assigner g q elt),d)
;;

let bin_queue_of_cxa fichier =						(* Lit bit par bit le fichier encodé, et rend queue de booléens correspondant au fichier *)
	let file = Queue.create () in					(* La fonction input_byte renvoi l'entier stocké (sous la forme de sa décomposition binaire) dans les 8 prochains bits du fichier *)
	let ajouter_int n =								(* bin_of_int transcrit de nouveau cet entier en sa décomposition binaire *)
		List.iter (fun x -> Queue.push x file) (norm8 (bin_of_int n)) in
	let rec acquisition () =						(* ajouter_int récupère les bits correspondant à l'entier n, puis ajoute ces bits dans la queue *)
		ajouter_int	(input_byte fichier);
		acquisition ();
	in
	try acquisition ()
	with End_of_file -> file;
;;

let décode_dim bin_queue =							(* Retire les informations sur les dimensions de l'image de la file bin_queue, et les renvois *)
	let h = queue_obtenir bin_queue 12 and l = queue_obtenir bin_queue 12 in
	(int_of_bin h, int_of_bin l)
;;

let décode_alphabet bin max =						(* Décode l'alpahbet de la queue bin *)
	let rec ne_contient_que_des_zeros = function
		| false::q -> ne_contient_que_des_zeros q
		| [] -> true
		| _ -> false
	in let rec retirer0 = function					(* Les transcriptions contiennent des zéros, puis un un, puis la transcription en elle même. Cette fonction retire les 0, et le 1 *)
		| false::q -> retirer0 q
		| true::q -> q
	in
	let prochain_code () =							(* Lit le code suivant, puis la couleur	*)
		let code = queue_obtenir bin (max+1) in		(* Si la transcription ne contient que des 0, c'est la fin de l'alphabet, on rend p_vide comme couleur	*)
		if ne_contient_que_des_zeros code then code,p_vide else
		let i = queue_obtenir bin 8
		and j = queue_obtenir bin 8 in
		code, {x=(int_of_bin i); y=(int_of_bin j)}
	in
	let rec construire_arbre arbre = 				(* Tant que prochain_code () ne rend pas p_vide, on construit l'arbre de décodage *)
		let code,coul = prochain_code () in
		if coul = p_vide then arbre
		else construire_arbre (arbre_assigner arbre (retirer0 code) coul);
	in
	construire_arbre Vide
;;

let décode_vecteurs bin nb_bases =
	let queue = Queue.create () in
	let rec prochaine_base () =
		if not (Queue.pop bin) then (* Un seul vecteur à décodé *)
			let rouge = int_of_bin (queue_obtenir bin 15) and vert = int_of_bin (queue_obtenir bin 15) in
			let r_f = (float_of_int rouge)  /. 10_000. and v_f = (float_of_int vert)  /. 10_000. in
			let b_f = sqrt (3. -. r_f*.r_f -. v_f*.v_f) in
			Queue.push ({rouge_f=r_f; vert_f=v_f; bleu_f= b_f},null) queue;
		else (* deux vecteurs *)
			let v1 = 
				if Queue.pop bin then (* La coord verte est nulle *)
					let rouge = int_of_bin (queue_obtenir bin 14) in
					let r_f = (float_of_int rouge)  /. 10_000. in
					let b_f = sqrt (2. -. r_f*.r_f) in
					{rouge_f = r_f; vert_f = 0.; bleu_f = b_f}
				else (* Le rouge est nul *)
					let vert = int_of_bin (queue_obtenir bin 14) in
					let v_f = (float_of_int vert)  /. 10_000. in
					let b_f = sqrt (2. -. v_f*.v_f) in
					{rouge_f = 0.; vert_f = v_f; bleu_f = b_f}
			and v2 =
				if Queue.pop bin then (* La coord verte est nulle *)
					let bleu = int_of_bin (queue_obtenir bin 14) in
					let b_f = (float_of_int bleu)  /. 10_000. in
					let r_f = sqrt (2. -. b_f*.b_f) in
					{rouge_f = r_f; vert_f = 0.; bleu_f = b_f}
				else (* Le bleu est nul *)
					let vert = int_of_bin (queue_obtenir bin 14) in
					let v_f = (float_of_int vert)  /. 10_000. in
					let r_f = sqrt (2. -. v_f*.v_f) in
					{rouge_f = r_f; vert_f = v_f; bleu_f = 0.}
			in Queue.push (v1,v2) queue;
	in
	for i=1 to nb_bases do prochaine_base () done;
	queue
;;

let tbl_vect_of_queue queue lig col case =
	let table = Array.make_matrix (lig/case) (col/case) (null,null) in
	for i=0 to lig/case -1 do
	for j=0 to col/case -1 do
		table.(i).(j) <- Queue.pop queue
	done; done;
	table
;;

let décode_image codage pixels nb_pixels =					(* Décode l'image de la queue bin *)
	let rec décode_pix sous_arbre = match sous_arbre with	(* décode_pix décode le pixels en cours *)
		| Noeud(coul,Vide,Vide) -> coul
		| Noeud(_,g,d) 			-> if Queue.pop pixels
										then décode_pix d
										else décode_pix g
	in let pixels_projetés = Queue.create () in
	for i=1 to nb_pixels do	Queue.push (décode_pix codage) pixels_projetés; done;
	pixels_projetés
;;

let déprojette_pixels pixels vecteurs lig col case =
	let min_coul_255 {rouge=r;  vert=v; bleu=b} = {rouge=(min 255 r); vert=(min 255 v); bleu=(min 255 b)} in
	let vect_du_pixel n_pixel = match n_pixel/(case*col) , (n_pixel mod col)/case with
		| n,p when n = lig/case && p = col/case		-> vecteurs.(n-1).(p-1)
		| n,p when n = lig/case 					-> vecteurs.(n-1).(p)
		| n,p when p = col/case 					-> vecteurs.(n).(p-1)
		| n,p 										-> vecteurs.(n).(p)
	in
	let pix_décodés = Queue.create () in
	let rec traite_pix n =
		let {x=i; y=j} = Queue.pop pixels
		and v1,v2 = vect_du_pixel n in
		Queue.push (min_coul_255 (couleur_of_couleur_f ((v1%(float_of_int i)) ++ (v2%(float_of_int j))) 1.)) pix_décodés;
		traite_pix (n+1);
	in
	try traite_pix 0; with Queue.Empty -> pix_décodés
;;

let decoupe = découpe;;
let decode_dim = décode_dim;;
let decode_image = décode_image;;
let decode_vecteurs =  décode_vecteurs;;
let decode_alphabet = décode_alphabet;;
let deprojette_pixels = déprojette_pixels;;

let image_of_cxa cin =														(* Lit le fichier encodé, renvoi l' 'image' correspondante *)
	let f = open_in_bin cin in
	afficher "Lecture du fichier ..." 1;
	let bin = bin_queue_of_cxa f in											(* On récupère d'un coup toute la queue de booléens correspondant au fichier *)
	close_in f;
	let (col,lignes) = décode_dim bin in									(* 24 premiers bits codent les dimensions *)
	let max = int_of_bin (queue_obtenir bin 8) in							(* 8 suivants codent la longueur maximale des transcriptions *)
	let case = int_of_bin (queue_obtenir bin 12) in							(* 12 suivants codent le coté des cases utilisées pour coder l'image *)
	let taux = (float_of_int (int_of_bin (queue_obtenir bin 10)))/.10. in (* 10 suivants codent le pourcentage de couleurs principales utilisé *)
	let dist = int_of_bin (queue_obtenir bin 8) in							(* 8 suivants codent la distance maximale entre couleur suprimmée et couleur principale *)
	if case = 0 then failwith "Image codée sans ACP, non prise en charge par ce script";
	afficher ("Taille des cases : "^(string_of_int case)) 2;
	afficher ("Pourcentage de couleurs principales 	: "^(string_of_float taux)^"%") 2;
	afficher ("Distance maximale entre les couleurs : "^(string_of_int dist)) 2;
	afficher "Decodage de l'alphabet ..." 1;
	let codage = décode_alphabet bin max in
	afficher "Decodage des vecteurs ..." 1;
	let nb_bases = (col/case)*(lignes/case) in
	let vecteurs = décode_vecteurs bin nb_bases in
	afficher "Creation du tableau des vecteurs ..." 1;
	let tbl_vect = tbl_vect_of_queue vecteurs lignes col case in
	afficher "Decodage de l'image ..." 1;
	let pixels_projetés = décode_image codage bin (col*lignes) in
	let pixels_décodés = déprojette_pixels pixels_projetés tbl_vect lignes col case in
	{lignes=lignes; colonnes=col; pixels=pixels_décodés}
;;