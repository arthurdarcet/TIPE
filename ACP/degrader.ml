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

let traiter img (taux,dist,case) =
	afficher "Encodage de l'image ..." 1;
	let bin = coder_image img taux dist case in				(* Encode l''image' et rend la liste de booléens à inscire dans le fichier de sortie *)
	afficher "Ecriture du fichier de sortie ..." 1;
	cxa_of_bin bin "c:/tmp.cxa";												(* Ecrit la liste de booléens dans le fichier cout *)
	afficher "Décodage du cxa ..." 1;
	image_of_cxa "c:/tmp.cxa";
;;

let exec tabl_img compression =
	let décomp = Array.make_matrix 10 10 {pixels=Queue.create (); lignes=0; colonnes=0} in
	for i=0 to 9 do
	for j=0 to 9 do
		let a,b,c = compression.(i).(j) in
		afficher ("Case : "^(string_of_int (10*i + j +1))^"/100, taux : "
					^(string_of_float a)
					^", dist : "^(string_of_int b)
					^", cases : "^(string_of_int c)) 12;
		décomp.(i).(j) <- traiter ({pixels=tabl_img.(i).(j); lignes=120; colonnes=192}) (compression.(i).(j));
	done; done;
	décomp
;;

let creer_comp () =
	let boule = [| (0.,); (0.1,); (0.2,); (0.3,); (0.4,); (0.5,); (0.6,); (0.7,); (0.8,); (0.9,); |] in
	let compression = Array.make_matrix 10 10 (1.,0,0) in (* taux,distance,cases *)
	for i=0 to 9 do for j=0 to 9 do
		let taux = (float_of_int (i+1)) /. 10. and dist = j*10 in
		compression.(i).(j) <- (boule.(i)),(12*(j+1));
	done; done;
	compression
;;

let compil () =
	afficher "Ouverture de l'image" 0;
	let img = image_of_ppm "c:/img.ppm" in
	afficher "Découpage de l'image" 0;
	let tabl_img = découpe img 192 120 in
	let compression = creer_comp () in
	afficher "Compression des cases" 0;
	let décomp = exec tabl_img compression in
	afficher "Regroupement des cases ..." 0;
	let img_finale = { pixels=regroupe décomp; lignes=1200; colonnes=1920} in
	ppm_of_image img_finale "c:/acp.ppm";
;;

compil ();;