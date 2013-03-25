											(* Opérations de base *)

open Types;;

(* Opérations sur les couleurs : *)

let couleur_f_of_couleur {rouge = r; vert = v; bleu = b} =
	{rouge_f = float_of_int r; vert_f = float_of_int v;  bleu_f = float_of_int b }
;;

let couleur_of_couleur_f {rouge_f = r; vert_f = v; bleu_f = b} k=
	{rouge = int_of_float (r*.k); vert = int_of_float (v*.k);  bleu = int_of_float (b*.k) }
;;
let couleur_p_of_couleur_p_f {x_f = a ; y_f = b} k = {x= int_of_float (a*.k) ; y= int_of_float (b*.k)};;

let couleur_f_queue_of_couleur_queue queue = 
	let queue_f = Queue.create () in
	let rec aux () =
		Queue.push (couleur_f_of_couleur (Queue.pop queue)) queue_f;
		aux ();
	in
	try aux () with Queue.Empty -> queue_f
;;

let (++) {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	{rouge_f = r+.r2; vert_f = v+.v2; bleu_f = b+.b2}
;;
let (--) {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	{rouge_f = r-.r2; vert_f = v-.v2; bleu_f = b-.b2}
;;
let (^^) {rouge_f = r; vert_f = v; bleu_f = b} k =
	{rouge_f = r**k; vert_f = v**k; bleu_f = b**k}
;;
let racine {rouge_f = r; vert_f = v; bleu_f = b} = 
	{rouge_f = sqrt r; vert_f = sqrt v; bleu_f = sqrt b}
;;
let (||) {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	r*.r2+.v*.v2+.b*.b2
;;
let (%) {rouge_f = r; vert_f = v; bleu_f = b} k =
	{rouge_f = r*.k; vert_f = v*.k; bleu_f = b*.k}
;;
let (%%) {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	{rouge_f = r*.r2; vert_f = v*.v2; bleu_f = b*.b2} 
;;
let (//) {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	{rouge_f = r/.r2; vert_f = v/.v2; bleu_f = b/.b2} 
;;

let prod_vectoriel {rouge_f = r; vert_f = v; bleu_f = b} {rouge_f = r2; vert_f = v2; bleu_f = b2} =
	{rouge_f = v*.b2-.v2*.b ; vert_f = r2*.b-.r*.b2 ; bleu_f =r*.v2-.r2*.v  } 
;;

let normer u k = 
	let a = sqrt(u||u) in 
	if a <>  0. 
		then 	u % (k/.a)
		else u
;;

(* Opérations sur les matrices : *)
let ($) n t = 
	let acc = ref 0. in
	for i = 1 to n do acc := !acc +. t.(i-1).(0)**2. done ;
	sqrt(!acc)
;;



let (&*) a b = 									(* Produit matriciel *)
	let n = Array.length a.(0) and m = Array.length a and p = Array.length b.(0) in
	if (Array.length b) <> n then failwith "mauvaises dimensions"
	else let matrice = Array.make_matrix m p 0. in
	for i = 0 to (m-1) do 
	for j = 0 to (p-1) do 
		for k = 0 to (n-1) do
			matrice.(i).(j) <- matrice.(i).(j) +. a.(i).(k) *. b.(k).(j);
		done;
	done;done;
	matrice
;;

let gram_schmidt m = 
	let a = sqrt(m.(0).(0)**2.+. m.(1).(0)**2.+. m.(2).(0)**2.) in 
	m.(0).(0)<-m.(0).(0)/.a;
	m.(1).(0)<-m.(1).(0)/.a;
	m.(2).(0)<-m.(2).(0)/.a;
	let b =  m.(0).(0)*.m.(0).(1) +. m.(1).(0)*.m.(1).(1) +.m.(2).(0)*.m.(2).(1) in 
	m.(0).(1)<-m.(0).(1)-.m.(0).(0)*.b;
	m.(1).(1)<-m.(1).(1)-.m.(1).(0)*.b;
	m.(2).(1)<-m.(2).(1)-.m.(2).(0)*.b;
	let c = sqrt(m.(0).(1)**2.+. m.(1).(1)**2.+. m.(2).(1)**2.) in 
	m.(0).(1)<-m.(0).(1)/.c;
	m.(1).(1)<-m.(1).(1)/.c;
	m.(2).(1)<-m.(2).(1)/.c;
	let d = m.(0).(0)*.m.(0).(2) +. m.(1).(0)*.m.(1).(2) +.m.(2).(0)*.m.(2).(2) 
	and e = m.(0).(1)*.m.(0).(2) +. m.(1).(1)*.m.(1).(2) +.m.(2).(1)*.m.(2).(2) in
	m.(0).(2)<-m.(0).(2)-.m.(0).(0)*.d-.m.(0).(1)*.e;
	m.(1).(2)<-m.(1).(2)-.m.(1).(0)*.d-.m.(1).(1)*.e;
	m.(2).(2)<-m.(2).(2)-.m.(2).(0)*.d-.m.(2).(1)*.e;
	let f = sqrt(m.(0).(2)**2.+. m.(1).(2)**2.+. m.(2).(2)**2.) in
	m.(0).(2)<-m.(0).(2)/.f;
	m.(1).(2)<-m.(1).(2)/.f;
	m.(2).(2)<-m.(2).(2)/.f;
	m;;

 
let vect_p m n =					(* Calcule les vecteurs propres de la matrice m *)
	let o = ref [| [|1.;0.;0.|]; [|0.;1.;0.|]; [|0.;0.;1.|] |] in 
	for i = 1 to n do o:=gram_schmidt(m &* !o); done;
	!o
;;

let vect m i =[|[|m.(0).(i-1)|]; [|m.(1).(i-1)|]; [|m.(2).(i-1)|]|];;
  
let couleur_f_of_mat_col v = {rouge_f = v.(0).(0); vert_f = v.(1).(0); bleu_f =v.(2).(0)} ;; 
  
let valeur_propre matrice =
	let vp = vect_p matrice 10_000 in 
	let u1= vect vp 1
	and u2= vect vp 2
	and u3= vect vp 3 in 
	let lambda1 = (3$(matrice &* u1))/.(3$u1)
	and lambda2 = (3$(matrice &* u2))/.(3$u2)
	and lambda3 = (3$(matrice &* u3))/.(3$u3) in
	{lambda =lambda1; vecteur = couleur_f_of_mat_col u1}::{lambda =lambda2; vecteur = couleur_f_of_mat_col u2}::{lambda =lambda3; vecteur =  couleur_f_of_mat_col u3}::[]
;;

let det [|[|a;b;c|];
		  [|d;e;f|];
		  [|g;h;i|]|] =  a*.e*.i +. b*.f*.g +. d*.c*.h -. c*.e*.g -. d*.b*.i -. h*.f*.a;; 