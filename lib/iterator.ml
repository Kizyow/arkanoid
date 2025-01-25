(* interface des itérateurs (implémentée par les flux) *)
module type Intf = sig
  type 'a t

  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val uncons : 'a t -> ('a * 'a t) option
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val unless : 'a t-> ('a -> bool) -> ('a -> 'a t) -> 'a t
  val unless_modif : 'a t -> ('a -> bool) -> ('a -> ('a*bool)) -> ('a -> bool -> 'a t) -> 'a t
end

type 'a flux = Tick of ('a * 'a flux) option Lazy.t

module Flux : Intf with type 'a t = 'a flux = struct
  type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t

  let vide = Tick (lazy None)
  let cons t q = Tick (lazy (Some (t, q)))
  let uncons (Tick (lazy flux)) = flux

  let rec apply f x =
    Tick
      (lazy
        (match uncons f, uncons x with
         | None, _ -> None
         | _, None -> None
         | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)))

  let rec unfold f e =
    Tick
      (lazy
        (match f e with
         | None -> None
         | Some (t, e') -> Some (t, unfold f e')))

  let rec filter p flux =
    Tick
      (lazy
        (match uncons flux with
         | None -> None
         | Some (t, q) -> if p t then Some (t, filter p q) else uncons (filter p q)))

  let rec append flux1 flux2 =
    Tick
      (lazy
        (match uncons flux1 with
         | None -> uncons flux2
         | Some (t1, q1) -> Some (t1, append q1 flux2)))

  let constant c = unfold (fun () -> Some (c, ())) ()

  (* implantation rapide mais inefficace de map *)
  let map f i = apply (constant f) i
  let map2 f i1 i2 = apply (apply (constant f) i1) i2
  let map3 f i1 i2 i3 = apply (map2 f i1 i2) i3

(* Fonction qui filtre et transforme les éléments d'un flux en fonction d'une condition *)
(* Si un élément satisfait la condition, il est transformé par une fonction donnée et   *)
(*    il est transmis à un nouveau générateur de flux.                                  *)
(* Sinon, il est laissé inchangé.                                                       *)
(* paramètres:                                                                          *)
(* - flux : 'a flux : Le flux de départ.                                                *)
(* - cond : 'a -> bool : La conditon à vérifier sur les éléments du flux                *)
(* - f_cond : 'a -> 'a flux : Le générateur de flux à partir de l'élément où cond est vérifié *)
let rec unless flux cond f_cond =
    Tick 
      (lazy
        (match (uncons flux) with
        |None -> None
        |Some (t,q) ->
            if (cond t) then
              uncons (f_cond t)
            else
              Some(t,unless q cond f_cond))
      )

(* Fonction qui filtre et transforme les éléments d'un flux en fonction d'une condition *)
(* Si un élément satisfait la condition, il est d'abord modifié par une fonction donnée, *)
(* puis il est transmis à un nouveau générateur de flux en fonction de la valeur booléenne retournée par la modification. *)
(* Sinon, il est laissé inchangé. *)
(* paramètres: *)
(* - flux : 'a flux : Le flux de départ. *)
(* - cond : 'a -> bool : La condition à vérifier sur les éléments du flux. *)
(* - elt_modif : 'a -> ('a * bool) : La fonction qui modifie l'élément et retourne une paire (élément modifié, booléen). *)
(* - f_cond : 'a -> bool -> 'a flux : Le générateur de flux à partir de l'élément modifié et de la valeur booléenne. *)
let rec unless_modif flux cond elt_modif f_cond =
    Tick 
      (lazy
        (match (uncons flux) with
        |None -> None
        |Some (t,q) ->
            if (cond t) then
              let newElt, boolVal = (elt_modif t) in
              uncons (f_cond newElt boolVal)
            else
              Some(t,unless_modif q cond elt_modif f_cond))
      )

end
