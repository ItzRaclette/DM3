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
  | Node(g, p, d) -> eval g * eval p + eval d


(* Test de eval sur les exemples de l'énoncé *)
let () =
  let output = List.map eval [deux; trois; cinquante; ex_818] in
  assert (output = [2; 3; 50; 818])


(** [trouve_p n] vaut la valeur de p dans le triplet (g, p, d)
    correspondant à [n] *)
let rec interm p n =
   if Suites.x p > n then p - 1 else interm (p+1) n

let trouve_p n =
  if n < 2 then failwith "pas de p associe" else interm 0 n


(** [of_int n] est l'arbre ternaire associé à [n] *)
let rec of_int n =
  if n = 0 then Zero else
    if n = 1 then Un else
      let x_p = Suites.x p in
        Node(of_int (n mod x_p), of_int (trouve_p n), of_int (n / x_p))


(* Test de of_int sur les exemples de l'énoncé *)
let () =
  let output = List.map of_int [2; 3; 50; 818] in
  assert (output = [deux; trois; cinquante; ex_818])



(***************************)
(* Signature et générateur *)
(***************************)

(** [signature tree] est la signature de tree *)
let rec signature tree =
  failwith "TODO"





























  

let () =
  print_endline "All tests passed !"