												(* Fonction de conversion entre fichier PPM et 'image' *)


open Divers;;
open Types;;

let rec lire_nouvelle_ligne f =							(* Lit la prochaine ligne du in_channel qui n'est pas un commentaire (commence par  #)  *)
	let l = input_line f in
	if l.[0] = '#' then lire_nouvelle_ligne f
	else l
;;

let rec lire_nouvelle_ligne_bin s�p f =					(* Lit la prochaine ligne du in_channel qui est n'est pas un commentaire,  *)
	let c =  input_char f in							(* les diff�rentes lignes sont s�par�es par le caract�re s�p *)
	if c = '#' then										(* Si la ligne en cours commence par un #, c'est un commentaire : on la saute, *)
		begin 											(* on lit caract�re par caract�re jusqu'au prochain s�parateur,*)
			while input_char f <> s�p do () done;		(*  puis on relance la fonction, pour g�rer plusiseurs comentaires de suite  *)
			lire_nouvelle_ligne_bin s�p f;
		end
	else
		begin
			let rec lire () =
				let carac = input_char f in
				if carac <> s�p then
					concat_d�but (lire ()) carac
				else ""
			in
			concat_d�but (lire ()) c
		end
;;


let lire_dim s =  										(* S�pare une chaine en deux, � l'espace *)
	let l = List.map int_of_string (Str.split (Str.regexp "[ \t]+") s) in (List.hd l,List.hd (List.tl l))
;;


let formatter_ppm cin cout=							(* Formatte le ppm en un format plus lisible : supression des commentaires et des sauts de lignes dans la description de l'image *)
	let fin = open_in cin and fout = open_out cout in
	output_string fout ((lire_nouvelle_ligne fin)^"\n");
	output_string fout ((lire_nouvelle_ligne fin)^"\n");
	output_string fout ((lire_nouvelle_ligne fin)^"\n");
	let rec aux fin fout = output_string fout (lire_nouvelle_ligne fin); flush fout; aux fin fout; in
	try aux fin fout with End_of_file -> (close_in fin; close_out fout)
;;


let sp�cial_input_char f = 								(* Lit le caract�re courant du fichier, et renvoi le caract�re espace si input_char raise End_of_file *)
	let c = ref ' ' in
	try c := input_char f; !c; with End_of_file -> !c
;;

let lire_image f col lignes =							(* Lit le in_channel 'f' et met les couleurs rencontr�es dans les la queue 'file' *)
	let file = Queue.create () in						(* Struture du ppm : (taux de rouge) espace (taux de vert) espace (taux de bleu) espace, pour chaque pixel	*)
	for j=1 to lignes do
		let c = ref ' ' and rouge = ref "" and vert = ref "" and bleu = ref "" in
		for i=1 to col do
			rouge := "";
			vert := "";
			bleu := "";

			c := input_char f;

			while !c <> ' ' do rouge := concat_fin !rouge !c; c := input_char f; done;

			c := input_char f;
			while !c <> ' ' do vert := concat_fin !vert !c; c := input_char f; done;

			c := input_char f;
			while !c <> ' ' do bleu := concat_fin !bleu !c; c := sp�cial_input_char f; done;

			Queue.push {rouge = (int_of_string !rouge); vert = (int_of_string !vert); bleu = (int_of_string !bleu)} file;
		done;
	done;
	file
;;

let lire_image_bin f =									(* Equivalent de lire_image pour une image binaire: pour chaque pixel, trois octets codent le taux de rouge, de vert, de bleu *)
	let file = Queue.create () in
	let rec lire_coul () =
		let r = input_byte f in
		let v = input_byte f in
		let b = input_byte f in
		Queue.push {rouge=r; vert=v; bleu=b} file;
		lire_coul ();
	in
	try lire_coul () with End_of_file -> file
;;


let image_of_P3 s =										(* Lit l'image ppm de type P3 (codage ASCII) et renvoi l' "image" correspondante *)
	afficher "Formatage du ppm ..." 1;
	let ch_tmp = (if sous_windows () then "c:/tmp.ppm" else "/tmp/tmp.ppm") in
	formatter_ppm s ch_tmp;								(* Formate l'image ppm, et la stocke dans un fichier temporaire *)
	let fichier = open_in ch_tmp in
	afficher "Lecture des entetes ..." 1;
	ignore (lire_nouvelle_ligne fichier);				(*  La premi�re ligne est P3, d�j� lu, on l'ignore *)
	let dim = lire_nouvelle_ligne fichier in			(* La deuxi�me ligne est de la forme (nombre de pixels en largeur) espace (nombre de pixels en hauteur) *)
	let (col,lign) = lire_dim dim in
	ignore (lire_nouvelle_ligne fichier);				(* Vaut toujours 255 dans les images trait�es, c'est la valeur maximale atteint par les taux de chaques couleurs *)
	afficher "Lecture de l'image ..." 1;
	let file = lire_image fichier col lign in			(* Lit l'image en elle m�me, et renvoi la queue correspondante	*)
	close_in fichier;
	{lignes=lign; colonnes=col; pixels=file}
;;

let image_of_P6 ch =									(* Lit l'image ppm de type P6 (codage binaire) et renvoi l' "image" correspondante *)
	let f = open_in_bin ch in
	afficher "Lecture des entetes ..." 1;
	ignore (input_char f); 								(* La premi�re ligne est toujours P6, on l'ignore caract�re par caract�re *)
	ignore (input_char f);
	let s�p = input_char f in							(* Le caract�re suivant est le s�parateur, qui d�limite chaque ligne dans le reste du fichier *)
	let dim = lire_nouvelle_ligne_bin s�p f in			(* Suivent les informations sur la taille de l'image *)
	let (col,lign) = lire_dim dim in
	ignore (lire_nouvelle_ligne_bin s�p f);				(* On ignore le 255 *)
	afficher "Lecture de l'image ..." 1;
	let file = lire_image_bin f in						(* Lecture de l'image en elle m�me *)
	close_in f;
	{lignes=lign; colonnes=col; pixels=file}
;;

let image_of_ppm ch = 									(* Lit le ppm situ� � l'emplacement ch, renvoi l' 'image' correspondante. *)
	let f = open_in ch in
	ignore (input_char f);								(* Le premier caract�re est un P *)
	let c = input_char f in								(* Le deuxi�me donne le format de l'image : 3 ppm cod� ASCII, 6 ppm cod� binaire *)
	close_in f;
	if c = '3' then image_of_P3 ch
	else if c = '6' then image_of_P6 ch
	else failwith "Format d'image inconnu"
;;

let �crire_image fich queue =							(* Ecrit dans le out_channel fich la queue de couleur queue *)
	let rec aux () =
		let {rouge=r; vert=v; bleu=b} = Queue.pop queue in
		output_byte fich r;
		output_byte fich v;
		output_byte fich b;
		flush fich;
		aux ();
	in
	try aux ()
	with Queue.Empty -> ()
;;

let ppm_of_image img chemin =							(* Fonction d'�criture d'une 'image' dans un fichier ppm (qui sera du type P6 (binaire))	*)
	afficher "Ouverture du ppm ..." 1;
	let fichier = open_out chemin in
	afficher "Ecriture des entetes ..." 1;
	output_string fichier ("P6\n"^(string_of_int img.colonnes)^" "^(string_of_int img.lignes)^"\n255\n");
	afficher "Ecriture de l'image ..." 1;
	set_binary_mode_out fichier true;					(* Le fichier �crit n'est pas un fichier texte, ceci �vite les conversions � l'�criture *)
	�crire_image fichier img.pixels;
	flush fichier;
	close_out fichier;
	print_newline ();
;;