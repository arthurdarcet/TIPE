											(* Fonction de conversion entre fichier CXA et 'image' *)
	
open Divers;;
open Types;;
open Huffman;;
open Destructive;;


(* Image vers CXA *)
let rec plus_longue_transcription inventaire codage = match inventaire with	(* Renvoi la longueur maximale des transcriptions qui seront utilisées pour coder l'image *)
	| [] -> 0
	| {couleur={rouge=r; vert=v; bleu=b}; poids=_}::q -> max (List.length (codage.(r).(v).(b))) (plus_longue_transcription q codage)
;;

let inventaire file_pixels =													(* Renvoi un tableau à trois dimensions contenant le nombre d'occurence dans l'image *)
	let table = Array.make_matrix 256 256 (Array.make 256 0) in					(* de la couleur {rouge=i; vert=j; bleu=k} à la case table.(i).(j).(k) *)
	for i=0 to 255 do for j=0 to 255 do
			table.(i).(j) <- Array.make 256 0;									(* Initialisation du tableau *)
	done; done;
	let rec remplit_tab () =
		let {rouge=r;vert=v;bleu=b} = Queue.pop file_pixels in 
		table.(r).(v).(b) <- table.(r).(v).(b) + 1; 
		remplit_tab ();
	in
	(try remplit_tab (); with Queue.Empty -> (););
	table
;;

let rec alphabet_binaire tbl_codage max list_invent img_bin = match list_invent with (* Ajoute l'alphabet encodé à la queue en construction img_bin *)
	| [] -> ()																		(* Pour chaque élément de la liste des couleurs, on ajoute son code, *)
	| {couleur = {rouge=r; vert=v; bleu=b}; poids=_}::q -> 							(*  et la couleur suivant les conventions CXA *)
		let f n = norm (bin_of_int n) 8 false in
		add_list ((norm (true::(tbl_codage.(r).(v).(b))) (max+1) false)@(f r)@(f v)@(f b)) img_bin;
		alphabet_binaire tbl_codage max q img_bin;
;;


let image_binaire img table img_bin =											(* Ajoute l'image encodé à la queue en construction img_bin *)
	let rec aux () =
		let {rouge=r; vert=v; bleu=b} = Queue.pop img in
		add_list (table.(r).(v).(b)) img_bin;
		aux ()
	in
	try aux () with
		| Queue.Empty -> ()
;;

let coder_image img taux_compress distance_entre_coul = 						(* Renvoi la liste de booléens correspondant au fichier encodé	*)
	afficher "Copie de la queue ..." 1;
	let pix1,pix2 = copie_queue img.pixels in									(* Il faut une queue pour creer l'alphabet, puis une queue pour coder l'image *)
	afficher "Inventaire de l'image ..." 1;										(* Elles sont vidées pendant l'opération *)
	let tbl_invent = inventaire pix1 in 
	afficher "Passage de l'inventaire en liste ..." 1;
	let list_invent = tri_fusion (list_of_tbl tbl_invent) in
	afficher "Selection des couleurs ..." 1;
	let list_selec = liste_des_prem_coul (int_of_float (taux_compress*.(float_of_int (img.lignes*img.colonnes)))) list_invent in
	afficher "Modification de l'inventaire ..." 1;
	let list_changements = modifier_tab_inventaire tbl_invent list_selec distance_entre_coul in
	afficher "Passage de l'inventaire en liste ..." 1;
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
	afficher "Encodage des informations de l'image ..." 1;
	let max = plus_longue_transcription list_invent_modif tbl_codage in 	
	let lig = norm (bin_of_int img.lignes) 12 false
	and col = norm (bin_of_int img.colonnes) 12 false
	and max_binaire = norm (bin_of_int max) 8 false
	and case = norm [] 12 false													(* Case est nul : l'image n'est pas codée avec de l'acp *)
	and taux_bin = norm (bin_of_int (int_of_float (1000.*.taux_compress))) 10 false
	and dist_bin = norm (bin_of_int distance_entre_coul) 8 false
	and img_bin = Queue.create () in
	add_list (col@lig@max_binaire@case@taux_bin@dist_bin) img_bin;
	afficher "Encodage de l'alphabet ..." 1;
	alphabet_binaire tbl_codage max list_invent_modif img_bin;
	let taille1 = Queue.length img_bin in
	let separateur = norm [] (max+1) false in
	add_list separateur img_bin;
	afficher "Ajout des couleurs oubliees ..." 1;
	modifier_tab_alphabet tbl_codage list_changements;
	afficher "Encodage de l'image ..." 1;
	image_binaire pix2 tbl_codage img_bin;
	let taille2 = Queue.length img_bin in
	afficher ("Taille de l'alphabet : "^(string_of_int (taille1/8))^" o") 2;
	afficher ("Taille de l'image    : "^(string_of_int ((taille2 - taille1)/(1024*8)))^" ko") 2;
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

let rec arbre_assigner arbre coord elt = match arbre,coord with	
	| Vide, [] 					-> Noeud(elt,Vide,Vide) (* Fonction de créations des arbres, les coordonnées sont des listes de booléens, false pour gauche, true pour droite *)
	| Vide, true::q 			-> Noeud(c_vide,Vide,(arbre_assigner Vide q elt))
	| Vide, false::q 			-> Noeud(c_vide,(arbre_assigner Vide q elt),Vide)
	| Noeud(a,g,d), true::q 	-> Noeud(a,g,(arbre_assigner d q elt))
	| Noeud(a,g,d), false::q 	-> Noeud(a,(arbre_assigner g q elt),d)
;;

let bin_queue_of_cxa fichier =						(* Lit bit par bit le fichier encodé, et rend queue de booléens correspondant au fichier *)
	let file = Queue.create () in					(* La fonction input_byte renvoi l'entier stocké (sous la forme de sa décomposition binaire) dans les 8 prochains bits du fichier *)
	let ajouter_int n =								(* bin_of_int transcrit de nouveau cet entier en sa décomposition binaire, on impose sa longueur à 8, pour ne pas perdre de 0 (false)*)
		List.iter (fun x -> Queue.push x file) (norm8 (bin_of_int n)) in
	let rec acquisition () =						(* ajouter_int récupère les bits correspondant à l'entier n, puis ajoute ces bits dans la queue *)
		ajouter_int	(input_byte fichier);
		acquisition ();
	in
	try acquisition ()
	with End_of_file -> file;
;;

let décode_dim bin_queue =							(* Retire les informations sur les dimensions de l'image de la file bin_queue, et les renvois *)
	let h = queue_obtenir bin_queue 12
	and l = queue_obtenir bin_queue 12 in
	(int_of_bin h, int_of_bin l)
;;

let décode_alphabet bin max =						(* Décode l'alpahbet de la file bin *)
	let rec ne_contient_que_des_zeros = function
		| false::q -> ne_contient_que_des_zeros q
		| [] -> true
		| _ -> false
	in let rec retirer0 = function					(* Les transcriptions contiennent des zéros, puis un un, puis la transcription en elle même. Cette fonction retire les 0, et le 1 *)
		| false::q -> retirer0 q
		| true::q -> q
	in
	let nb_coul = ref 0 in
	let prochain_code () =							(* Lit le code suivant, puis la couleur	*)
		let code = queue_obtenir bin (max+1) in		(* Si la transcription ne contient que des 0, c'est la fin de l'alphabet, on rend c_vide (noir, défini dans types.ml) comme couleur *)
		if ne_contient_que_des_zeros code then code,c_vide else
		let r = queue_obtenir bin 8
		and v = queue_obtenir bin 8
		and b = queue_obtenir bin 8 in
		code, {rouge=(int_of_bin r); vert=(int_of_bin v); bleu=(int_of_bin b)}
	in
	let rec construire_arbre arbre = 				(* Tant que prochain_code () ne rend pas c_vide,  on augmente le nombre de couleurs présentes, et on construit l'arbre de décodage *)
		let code,coul = prochain_code () in
		if coul = c_vide then arbre,!nb_coul 
		else 
			begin
				nb_coul := (!nb_coul)+1;
				construire_arbre (arbre_assigner arbre (retirer0 code) coul);
			end;
	in
	construire_arbre Vide
;;

let décode_image codage pixels nb_pixels =			(* Décode l'image de la file bin *)
	let rec décode_pix sous_arbre = match sous_arbre with
		| Noeud(coul,Vide,Vide) -> coul				(* décode_pix décode le pixels en cours *)
		| Noeud(_,g,d) ->
			if Queue.pop pixels
				then décode_pix d
				else décode_pix g
	in
	let pixels_décodées = Queue.create () in
	for i=1 to nb_pixels do	Queue.push (décode_pix codage) pixels_décodées; done;
	pixels_décodées
;;

let image_of_cxa cin =								(* Lit le fichier encodé, renvoi l' 'image' correspondante *)
	let f = open_in_bin cin in
	afficher "Lecture du fichier ..." 1;
	let bin = bin_queue_of_cxa f in					(* On récupère d'un coup toute la queue de booléens correspondant au fichier *)
	close_in f;
	let (col,lignes) = décode_dim bin in			(* 24 premiers bits codent les dimensions *)
	let max = int_of_bin (queue_obtenir bin 8) in	(* 8 suivants codent la longueur maximale des transcriptions *)
	let case = int_of_bin (queue_obtenir bin 12) in	(* 12 suivants codent le coté des cases utilisées pour coder l'image *)
	let taux = (float_of_int (int_of_bin (queue_obtenir bin 10)))/.1000. in
	if case <> 0 then failwith "Cette image CXA est encodée avec l'ACP, ce script ne la prend pas en charge";
	let dist = int_of_bin (queue_obtenir bin 8) in
	afficher ("Pourcentage de couleurs principales 	: "^(string_of_float taux)^"%") 2;
	afficher ("Distance maximale entre les couleurs : "^(string_of_int dist)) 2;
	afficher "Decodage de l'alphabet ..." 1;
	let codage,nb_coul = décode_alphabet bin max in
	afficher ("Nombre de couleurs : "^(string_of_int nb_coul)) 1;
	afficher "Decodage de l'image ..." 1;
	let pixels_décodés = décode_image codage bin (col*lignes) in
	{ lignes=lignes; colonnes=col; pixels=pixels_décodés}
;;