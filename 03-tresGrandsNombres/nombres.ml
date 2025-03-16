(* Adapté de TP algo ENS 2017 *)


(***************************)
(* Le type et les exemples *)
(***************************)


(** Le type proposé par l'énoncé.
    Notez qu'un noeud a trois enfants et aucune étiquettes. *)
type inttree =
  | Zero
  | Un
  | Node of inttree * inttree * inttree (* g, p, d *)


(* L'exemple de l'énoncé et ses sous-arbres.
   Très utile pour tester !
*)
let      deux = Node (Zero, Zero, Un)
let     trois = Node (Un, Zero, Un)
let cinquante = Node (deux, deux, trois)
let    ex_818 = Node (cinquante, trois, trois)



(***************************)
(* Fonctions de conversion *)
(***************************)

(** [eval tree] est l'entier encodé par [tree] *)
let rec eval tree =
  match tree with
  | Zero -> 0
  | Un -> 1
  | Node(g, p, d) -> eval g +  Suites.x (eval p) * eval d


(** [trouve_p n] vaut la valeur de p dans le triplet (g, p, d)
    correspondant à [n] *)
let rec interm p n =
  if p>5 then 5 else if Suites.x p > n then
  p - 1
  else interm (p+1) n

let trouve_p n =
  if n < 2 then failwith "pas de p associe" else interm 0 n


(** [of_int n] est l'arbre ternaire associé à [n] *)
let rec of_int n =
  if n = 0 then Zero else
    if n = 1 then Un else
      let x_p = Suites.x (trouve_p n) in
        Node(of_int (n mod x_p), of_int (trouve_p n), of_int (n / x_p))

(***************************)
(* Signature et générateur *)
(***************************)

(** [signature tree] est la signature de tree *)
let rec signature tree =
  match tree with
  | Zero -> 0
  | Un -> (Suites.u 10) mod 97
  | Node(g,p,d) ->
    let sig_g = signature g in
    let sig_d = signature d in
    if (eval p) mod 2 = 1 then
    ((sig_g) + (Suites.u 20) * sig_d) mod 97
  else ((sig_g) + (Suites.u 30) * sig_d) mod 97

let rec puissance n k=
  if k = 0 then 1
  else n * puissance n (k-1)

 let sig_v k n =
   signature (of_int(Suites.v k n))


let rec sig_h_interm k =
   if k < 2 then signature (of_int (Suites.h k)) else
   let resultat = sig_h_interm (k-1) in
   (resultat + (Suites.u 20)*resultat) mod 97
 let sig_h k =
   sig_h_interm (Suites.v k (7*k))


let rec sig_gen_interm k n =
  if k=0 then
    if Suites.u n mod 7 = 0 then signature Zero else signature Un
  else
    let k' = max 0 (k - 1 -((Suites.u n)mod 2)) in
    let g = sig_gen_interm k' ((n+1)mod 997) in
    let p = (Suites.v k' n) in
    let d = sig_gen_interm k' ((n+2)mod 997) in
    if d = 0
      then g
    else
      let sig_p = Suites.u (if p mod 2 = 0 then 30 else 20) in
      (g + sig_p * d) mod 97




let sig_gen k n =
  sig_gen_interm k n


(*******************)
(** Décrémentation *)
(*******************)

let sig_dec_gen k =
  failwith "TODO"


(*******************)
(** Incrémentation *)
(*******************)

let sig_inc_gen k =
  failwith "TODO"
