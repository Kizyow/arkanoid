(* Définition des types pour les composantes de l'état de la raquette *)

type t
val initialiser : float -> bool -> t
val position : t -> float
val clique : t -> bool