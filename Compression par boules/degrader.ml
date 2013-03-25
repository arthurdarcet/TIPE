open Ppm_image;;
open Cxa_image;;
open Divers;;
open Types;;

let découpe img =
	let table = Array.make_matrix 10 10 (Queue.create ()) in
	for i=0 to 9 do for j=0 to 9 do
		table.(i).(j) <- Queue.create ();
	done; done;
	let classer_pix nb =
		let lig = nb/1920 and col = nb mod 1920 in
		let lig_tab = lig/120 and col_tab = col/192 in
		Queue.push (Queue.pop img.pixels) (table.(lig_tab).(col_tab));
	in
	for i=0 to (1920*1200) -1 do classer_pix i done;
	table
;;

let regroupe table =
	let trouver_pix nb =
		let lig = nb/1920 and col = nb mod 1920 in
		let lig_tab = lig/120 and col_tab = col/192 in
		Queue.pop (table.(lig_tab).(col_tab).pixels);
	in
	let queue = Queue.create () in
	for i=0 to (1920*1200)-1 do
		Queue.push (trouver_pix i) queue
	done;
	queue
;;

let traiter img (taux,dist) =
	afficher "Encodage de l'image ..." 1;
	let bin = coder_image img taux dist in				(* Encode l''image' et rend la liste de booléens à inscire dans le fichier de sortie *)
	afficher "Ecriture du fichier de sortie ..." 1;
	cxa_of_bin bin "c:/tmp.cxa";												(* Ecrit la liste de booléens dans le fichier cout *)
	afficher "Décodage du cxa ..." 1;
	image_of_cxa "c:/tmp.cxa";
;;

let exec tabl_img compression =
	let décomp = Array.make_matrix 10 10 {pixels=Queue.create (); lignes=0; colonnes=0} in
	for i=0 to 9 do
	for j=0 to 9 do
		afficher ("Case : "^(string_of_int (10*i + j +1))^"/100, taux : "
					^(string_of_float (fst compression.(i).(j)))
					^", dist : "^(string_of_int (snd compression.(i).(j)))) 12;
		décomp.(i).(j) <- traiter ({pixels=tabl_img.(i).(j); lignes=120; colonnes=192}) (compression.(i).(j));
	done; done;
	décomp
;;

let compil () =
	afficher "Ouverture de l'image" 0;
	let img = image_of_ppm "c:/img.ppm" in
	afficher "Découpage de l'image" 0;
	let tabl_img = découpe img in
	let compression = Array.make_matrix 10 10 (1.,0) in
	for i=0 to 9 do for j=0 to 9 do
		let taux = 1. -. ((float_of_int i)/.10.) and dist = j*10 in
		compression.(i).(j) <- taux,dist;
	done; done;
	afficher "Compression des cases" 0;
	let décomp = exec tabl_img compression in
	afficher "Regroupement des cases ..." 0;
	let img_finale = { pixels=regroupe décomp; lignes=1200; colonnes=1920} in
	ppm_of_image img_finale "c:/boule.ppm";
;;

compil ();;