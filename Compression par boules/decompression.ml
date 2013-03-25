											(* Décompression en ligne de commande : fonction finale et gestion des arguments *)


open Unix;;
let t_debut = gettimeofday ();;
open Divers;;
open Ppm_image;;
open Cxa_image;;

Gc.set { (Gc.get()) with Gc.stack_limit = 100_000_000 };;

let compil cin cout =
	print_newline ();
	afficher "Conversion en image ..." 0;
	let img = image_of_cxa cin in
	afficher "Ecriture du fichier ppm ..." 0;
	ppm_of_image img cout;
;;

let cin = ref "" and cout = ref "";;
let arguments = Arg.align [("-i", Arg.Set_string(cin),"Chemin du fichier à compresser");
							("-o",Arg.Set_string(cout),"Chemin du fichier de sortie")];;
Arg.parse arguments (fun s -> ();) "Décompression";;

compil (!cin) (!cout);;
let t_fin = gettimeofday ();;

(* Affichage du temps d'exécution *)
print_newline ();
print_string " Temps d'execution : ";;
let tps = gmtime (t_fin -. t_debut);;
if tps.tm_mday <> 1 then (print_int (tps.tm_mday-1); print_string " jours, ");;
if tps.tm_hour <> 0 then (print_int tps.tm_hour; print_string " heures, ");;
if tps.tm_min <> 0 then (print_int tps.tm_min; print_string " minutes, ");;
print_int tps.tm_sec;;
print_string " secondes.";;
print_newline ();;