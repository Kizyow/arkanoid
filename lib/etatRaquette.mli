(* Définition des types pour les composantes de l'état de la raquette *)

type position = float
type vitesse = float
type t
val initialiser : position -> vitesse -> t
val position : t -> position
val vitesse : t -> vitesse