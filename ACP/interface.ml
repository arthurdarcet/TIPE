													(* Interface *)
							(* Seules les deux premières fonctions ont un intérêt (le reste est de la manipulation de fenêtre) *)

let ch_defaut = "../../Images/";;

open Labltk;;
open Tk;;

open Divers;;
open Ppm_image;;
open Cxa_image;;

Gc.set { (Gc.get()) with Gc.stack_limit = 100000000 };;

let decompression cin cout =									(*  cin et cout sont des string représentant le chemin d'accès au fichier en entrée (cin) et en sortie (cout) *)
	Divers.ts_debut := Unix.gettimeofday ();
	print_newline ();
	afficher "Conversion en image ..." 0;
	let img = image_of_cxa cin in								(* Lit le cxa à cin et le transforme en 'image' *)
	afficher "Ecriture du fichier ppm ..." 0;
	ppm_of_image img cout;										(* Ecrit l'image dans un ppm à cout *)
	print_endline "Fini !";
	print_endline ("Temps d'exécution :"^(afficher_heure ()));
	print_newline (); print_newline ();	print_newline ();
;;
let compression cin cout taux_compress dist_coul taille_case =	(*  cin et cout sont des string représentant le chemin d'accès au fichier en entrée (cin) et en sortie (cout) *)
	Divers.ts_debut := Unix.gettimeofday ();
	print_endline (" Distance maximale entre les couleurs : "^(string_of_int dist_coul));
	print_endline (" Poids des couleurs principales : "^(string_of_float (taux_compress*.100.))^" %");
	print_endline (" Taille des cases : "^(string_of_int (taille_case))^" pixels");
	print_newline ();
	afficher "Conversion en image ..." 0;
	let img = image_of_ppm cin in								(* Lit le ppm à cin, et le transforme en 'image' *)
	afficher "Encodage de l'image ..." 0;						(* Encode l''image' et rend la liste de booléens à inscire dans le fichier de sortie	*)
	let bin = coder_image img taux_compress dist_coul taille_case in
	afficher "Ecriture du fichier de sortie ..."0;
	cxa_of_bin bin cout;										(* Ecrit la liste de booléens dans le fichier cout *)
	print_endline "Fini !";
	print_endline ("Temps d'exécution :"^(afficher_heure ()));
	print_newline (); print_newline ();	print_newline ();
;;


let compute ch_in ch_out taux_entry dist_entry case_entry() =
	let txt_in = Entry.get ch_in and txt_out = Entry.get ch_out 
	and taux = Entry.get taux_entry and dist = Entry.get dist_entry 
	and taille_case = Entry.get case_entry in
	match String.sub txt_in (String.length txt_in - 3) 3 with
		| "cxa" -> decompression txt_in txt_out;
		| "ppm" -> compression txt_in txt_out ((float_of_string taux)/.100.) (int_of_string dist) (int_of_string taille_case);
		| _		-> failwith "Format d'image inconnu"
	
;;

let recopier ch_in ch_out () =
	let s = Entry.get ch_in in
	let t = String.sub s 0 (String.length s - 4) in
	let s2 = t^".out.ppm" and s1 = t^".cxa" in
	let txt = Textvariable.create () in
	match String.sub s (String.length s - 3) 3 with
		| "cxa" -> Textvariable.set txt s2; Entry.configure ch_out ~textvariable:txt
		| "ppm" -> Textvariable.set txt s1; Entry.configure ch_out ~textvariable:txt
		| _		-> Textvariable.set txt s;  Entry.configure ch_out ~textvariable:txt
;;
let info ch_in () =
	let f = open_in_bin (Entry.get ch_in) in
	afficher "Lecture du fichier ..." 1;
	let bin = bin_queue_of_cxa f in						(* On récupère d'un coup toute la queue de booléens correspondant au fichier *)
	let (col,lignes) = décode_dim bin in				(* 24 premiers bits codent les dimensions *)
	let max = int_of_bin (queue_obtenir bin 8) in		(* 8 suivants codent la longueur maximale des transcriptions *)
	let case = int_of_bin (queue_obtenir bin 12) in		(* 12 suivants codent le coté des cases utilisées pour coder l'image *)
	let taux = (float_of_int (int_of_bin (queue_obtenir bin 10)))/.1000. in
	let dist = int_of_bin (queue_obtenir bin 8) in
	afficher ("Hauteur : "^(string_of_int lignes)^" pixels, largeur :  "^(string_of_int col)^" pixels") 2;
	afficher ("Taille de la plus grande transcription : "^(string_of_int max)^" bits") 2;
	afficher (if case = 0 then "Image codée sans ACP" else "Taille des cases : "^(string_of_int case)^" pixels") 2;
	afficher ("Pourcentage de couleurs principales 	: "^(string_of_float (taux*.100.))^"%") 2;
	afficher ("Distance maximale entre les couleurs : "^(string_of_int dist)) 2;
	afficher "Fini !" 1;
	print_newline (); print_newline ();
	close_in f;
;;

let init () =
	let root = Tk.openTk () in
	let frame_in = Frame.create root in
	let frame_out = Frame.create root in
	let frame_blanc = Frame.create root in
	let frame_param = Frame.create root in
	let frame_param2 = Frame.create root in

	let txtvar = Textvariable.create () and t2 = Textvariable.create () and t3 = Textvariable.create () and t4 = Textvariable.create () in
	Textvariable.set txtvar ch_defaut;
	Textvariable.set t2 "100";
	Textvariable.set t3 "0";
	Textvariable.set t4 "100";

	let txt_in = Label.create frame_in ~text:"Fichier entrant :	" in
	let ch_in = Entry.create frame_in ~width:50 ~textvariable:txtvar in

	
	let txt_out = Label.create frame_out ~text:"Fichier sortant :	" in
	let ch_out = Entry.create frame_out ~width:50 in
	let bout_out1 = Button.create frame_out ~text:"Recopier" ~command: (recopier ch_in ch_out) in

	let txt_blanc = Label.create frame_blanc in
	
	let txt_taux = Label.create frame_param ~text:"Poids des couleurs principales : " in
	let taux = Entry.create frame_param ~width:5 ~textvariable:t2 in
	let txt_pourc = Label.create frame_param ~text:" %" in
	let txt_dist = Label.create frame_param ~text:"Distance maximale entre les couleurs : " in
	let dist = Entry.create frame_param ~width:5 ~textvariable:t3 in

	let txt_case = Label.create frame_param2 ~text:"Taille des cases : " in
	let case = Entry.create frame_param2 ~width:5 ~textvariable:t4 in


	let boutton = Button.create root ~text:"Compresser / Décompresser" ~command: (compute ch_in ch_out taux dist case) in
	let info = Button.create root ~text:"Information sur l'image" ~command: (info ch_in) in

	pack [frame_in] ~side:`Top ~expand:true ~fill:`Both ;
	pack [txt_in] ~side:`Left;
	pack [ch_in] ~side:`Left;
	pack [frame_out] ~side:`Top ~expand:true ~fill:`Both ;
	pack [txt_out] ~side:`Left;
	pack [ch_out] ~side:`Left;
	pack [bout_out1] ~side:`Left;
	pack [frame_blanc] ~side:`Top ~expand:true ~fill:`Both ;
	pack [txt_blanc] ~side:`Left;
	pack [frame_param] ~side:`Top ~expand:true ~fill:`Both ;
	pack [txt_taux] ~side:`Left;
	pack [taux] ~side:`Left;
	pack [txt_pourc] ~side:`Left;
	pack [dist] ~side:`Right;
	pack [txt_dist] ~side:`Right;
	pack [frame_param2] ~side:`Top ~expand:true ~fill:`Both ;
	pack [txt_case] ~side:`Left;
	pack [case] ~side:`Left;
	pack [frame_blanc] ~side:`Top ~expand:true ~fill:`Both ;
	pack [info;boutton] ~side:`Bottom ~expand:true ~fill:`X ;
	mainLoop ()
;;

init ();;