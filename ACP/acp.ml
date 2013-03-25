													(*  Analyse en composantes principales *)


open Operations_de_base;;
open Types;;
open Divers;;

let moyenne queue  = 										(* Calcule la moyenne de la queue queue *)
	let rec l = float_of_int(Queue.length queue) and acc = ref null in
	let rec aux () =
		acc := (!acc ++ ((couleur_f_of_couleur (Queue.pop queue))%(1./.l)));
		aux ();
	in
	try aux () with Queue.Empty -> !acc
;;

let ecart_t queue moy = 									(* Calcule l'écart type de la queue queue de moyenne moy *)
	let l = Queue.length queue and acc = ref null in 
	let rec aux () = 
		acc := !acc ++ (( (couleur_f_of_couleur   (Queue.pop queue) -- moy))^^2.)%(1./.(float_of_int l));
		aux ()
	in
	try aux () with Queue.Empty -> racine (!acc)
;;
	
let centrer queue = 										(* Passe la queue en variable adimenssionée *)
	let q,q1 = copie_queue queue in
	let q2,q3 = copie_queue q in
	let m = moyenne q1 in
	let s = ecart_t q2 m in
	let proj = Queue.create () in
	let rec aux () =
		Queue.push (((couleur_f_of_couleur (Queue.pop q3))--m)//s) proj;
		aux ();
	in 
	try aux () with Queue.Empty -> proj
;;

let correlation queue2 = 									(* Calcule la matrice des corrélations de la queue queue2 *)
	let l = float_of_int (Queue.length queue2) and queue = centrer queue2 in
	let corr = [| [|1.;0.;0.|]; [|0.;1.;0.|]; [|0.;0.;1.|] |] in
	let rec aux () =
		let c = Queue.pop queue	in
		corr.(1).(0) <- corr.(1).(0) +. c.rouge_f*.c.vert_f/.l;
		corr.(2).(0) <- corr.(2).(0) +. c.rouge_f*.c.bleu_f/.l;
		corr.(2).(1) <- corr.(2).(1) +. c.bleu_f*.c.vert_f/.l;
		aux ();
	in
	try aux () with Queue.Empty -> ();
	corr.(0).(1) <- corr.(1).(0);
	corr.(0).(2) <- corr.(2).(0);
	corr.(1).(2) <- corr.(2).(1);
	corr
;;


let pourcentage m = 										(* Calcule le pourcentage d'information conservée par l'ACP *)
	if (det m) = 0. then 1.
	else let [{lambda =lambda1; vecteur = u1};
			  {lambda =lambda2; vecteur = u2};
			  {lambda =lambda3; vecteur = u3}] = valeur_propre m in 
	(lambda1+.lambda2)/.(lambda1+.lambda2+.lambda3)
;;

let intersect {rouge_f = a; vert_f = b; bleu_f = c} s = 	(* Intersecte le plan normal à a,b,c avec le plan rouge, bleu ou vert pur *)
	match s with
	|"rouge" -> if b*.c < 0.
					then {rouge_f = 0.; vert_f = 1.; bleu_f = -. b/.c}
					else null
	|"vert" -> if a*.c < 0.
					then {rouge_f = 1.; vert_f = 0.; bleu_f = -. a/.c}
					else null
	|"bleu" -> if a*.b < 0.    
					then {rouge_f = 1.; vert_f = -. a/.b; bleu_f = 0.}
					else null
;;

let selectionne u v w = 									(* Sélectionne parmi les vecteurs u,v et w un base *)
	if u = null
		then  v,w
	else if v = null
		then u,w
		else u,v
;;

let bonne_base u1 u2 = 										(* Transforme la base u1,u2 en une base encodable dans le fichier de sortie *)
	let arrondir {rouge_f=r; vert_f=v; bleu_f=b}=			(* ie  correctement normée et avec des coordonnées nulles			 *)
		{rouge_f = (floor (r*.100_000.))/.100_000.; vert_f =(floor (v*.100_000.))/.100_000.;bleu_f = (floor (b*.100_000.))/.100_000.}
	in
	let normer2 (u,v) a = (arrondir (normer u a ), arrondir (normer v a)) in
	let w = prod_vectoriel (arrondir u1) (arrondir u2) in
	if w = null then normer2 (u1,null) (sqrt 3.)
	else
		let v1 = intersect w "rouge"
		and v2 = intersect w "vert"
		and v3 = intersect w "bleu"	in 
		normer2 (selectionne v1 v2 v3) (sqrt 2.)
;;

let array_of_coul_f {rouge_f=r;  vert_f=v; bleu_f=b;} = [|r; v; b |];;

let projeter u v queue =									(* Projette chaque couleur de la queue sur la base u,v (qui n'est pas orthogonale) avec les relations Maple *)
	let proj coul =
		if v = null then {x_f= (coul||u)/.3.; y_f = 0. }
		else
			let w =  prod_vectoriel u v in (* d = determinant de la transposée de la matrice de passage de la base canonique dans v1,v2,w (ie de la matrice de passage) *)
			let d = det [| (array_of_coul_f u); (array_of_coul_f v); (array_of_coul_f w) |] in
			let {rouge_f=c1; vert_f=c2; bleu_f=c3} = coul in
			let {rouge_f=u1; vert_f=u2; bleu_f=u3} = u and {rouge_f=v1; vert_f=v2; bleu_f=v3} = v in
			let x = max 0. (c1*.u1*.v2*.v2/.d -. c1*.v2*.u2*.v1/.d -. c1*.v3*.u3*.v1/.d +. c1*.u1*.v3*.v3/.d -.
					c2*.v1*.u1*.v2/.d +. c2*.u2*.v1*.v1/.d +. c2*.u2*.v3*.v3/.d -. c2*.v3*.u3*.v2/.d +.
					c3*.u3*.v1*.v1/.d -. c3*.v1*.u1*.v3/.d -. c3*.v2*.u2*.v3/.d +. c3*.u3*.v2*.v2/.d) in (* Formule de Maple *)
			let y = max 0. (c1*.u2*.u2*.v1/.d -. c1*.u2*.u1*.v2/.d +. c1*.u3*.u3*.v1/.d -. c1*.u3*.u1*.v3/.d +.
					c2*.u1*.u1*.v2/.d -. c2*.u1*.u2*.v1/.d -. c2*.u3*.u2*.v3/.d +. c2*.u3*.u3*.v2/.d -.
					c3*.u1*.u3*.v1/.d +. c3*.u1*.u1*.v3/.d +. c3*.u2*.u2*.v3/.d -. c3*.u2*.u3*.v2/.d) in
			{y_f = y ; x_f = x }
	in
	let queue_projeté =  Queue.create() in
	let rec vider () =
		Queue.push (proj (couleur_f_of_couleur (Queue.pop queue))) queue_projeté;
		vider ();
	in
	try vider () with Queue.Empty -> {v1_f=u; v2_f=v; pix_case_f = queue_projeté}
;; 

let acp queue = 											(* Applique l'acp à la queue *)
	let q1,q2 = copie_queue queue in
	let mat = correlation q1 in
	let {lambda=_; vecteur=u}::{lambda=vp; vecteur=v}::_ = valeur_propre mat in
	let (v1,v2) = bonne_base u (if vp < 0.00001 then null else v) in
	(pourcentage mat,projeter v1 v2 q2)
;;

let case_acp_of_case_acp_f img = 							(* Transforme une case de float en entiers *)
	let q = Queue.create() in
	let rec aux () = 
		let c = Queue.pop img.pix_case_f in
		Queue.push (couleur_p_of_couleur_p_f c 1.) q;
		aux ();
	in 
	try aux ()
	with Queue.Empty -> {v1= couleur_of_couleur_f (img.v1_f)  10_000.; v2= couleur_of_couleur_f (img.v2_f ) 10_000. ; pix_case = q}
;;