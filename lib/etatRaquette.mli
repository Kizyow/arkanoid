open Iterator

(* Définition des types pour les composantes de l'état de la raquette *)

type etatRaquette
val initialiser : float -> bool -> etatRaquette
val position : etatRaquette -> float
val clique : etatRaquette -> bool
val flux_etat_raquette : float -> float -> float -> etatRaquette flux