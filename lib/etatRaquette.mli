(* Définition des types pour les composantes de l'état de la raquette *)

type t
val initialiser : float -> float -> t
val position : t -> float
val vitesse : t -> float