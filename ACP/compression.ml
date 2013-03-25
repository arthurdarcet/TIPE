											(* Compression en ligne de commande : fonction finale et gestion des arguments *)



open Unix;;
let t_debut = gettimeofday ();;											(* Initialisation du compteur pour afficher le temps écoulé  *)
open Divers;;
open Ppm_image;;
open Cxa_image;;

Gc.set { (Gc.get()) with Gc.stack_limit = 100_000_000 };;				(* Evite les stack_overflow *)

let compil cin cout taux_compress dist_coul taille_case =				(*  cin et cout sont des string représentant le chemin d'accès au fichier en entrée (cin) et en sortie (cout)	*)
	print_newline ();
	afficher "Conversion en image ..." 0;
	let img = image_of_ppm cin in										(* Lit le ppm à cin, et le transforme en 'image' *)
	afficher "Encodage de l'image ..." 0;
	let bin = coder_image img taux_compress dist_coul taille_case in	(* Encode l''image' et rend la liste de booléens à inscire dans le fichier de sortie	*)
	afficher "Ecriture du fichier de sortie ..."0;
	cxa_of_bin bin cout;												(* Ecrit la liste de booléens dans le fichier cout *)
;;

let cin = ref "" and cout = ref "" and taux_compress = ref 1. and dist_coul = ref 1 and taille = ref 100;;
																		(* taux_compress et dist_coul par défaut à 1 se qui équivaut à aucune destruction de l'image *)

let arguments = Arg.align [("-i", Arg.Set_string(cin),"Chemin du fichier à compresser");
							("-o",Arg.Set_string(cout),"Chemin du fichier de sortie");
							("-c",Arg.Set_float(taux_compress),"Pourcentage de couleurs à conserver");
							("-d",Arg.Set_int(dist_coul),"Distance maximale entre les couleurs remplacées");
							("-t",Arg.Set_float(taux_compress),"Taille des cases");];;
							
Arg.parse arguments (fun s -> ();) "Compression";;

print_endline (" Distance maximale entre les couleurs : "^(string_of_int !dist_coul));;
print_endline (" Taux de compression : "^(string_of_float !taux_compress));;
print_endline (" Taille des cases : "^(string_of_int !taille));;

compil (!cin) (!cout) (!taux_compress) (!dist_coul) (!taille);;

let t_fin = gettimeofday ();;


print_newline ();
print_string " Temps d'execution : ";;									(* Affichage du temps d'exécution *)
let tps = gmtime (t_fin -. t_debut);;
if tps.tm_mday <> 1 then (print_int (tps.tm_mday -1); print_string " jours, ");;
if tps.tm_hour <> 0 then (print_int tps.tm_hour; print_string " heures, ");;
if tps.tm_min <> 0 then (print_int tps.tm_min; print_string " minutes, ");;
print_int tps.tm_sec;;
print_string " secondes.";;
print_newline ();;