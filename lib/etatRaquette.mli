open Iterator

(* Définition des types pour les composantes de l'état de la raquette *)

type t
val initialiser : float -> bool -> t
val position : t -> float
val clique : t -> bool
val flux_etat_raquette : float -> float -> float -> t flux