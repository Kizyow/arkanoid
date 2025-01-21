open EtatBalle
open EtatEspaceBrique

(* Définition des types pour les composantes de l'état du jeu *)


type t
val initialiser : EtatBalle.t -> EtatEspaceBrique.t -> 'a -> 'b -> t
val balle : t -> EtatBalle.t
val briques : t -> EtatEspaceBrique.t
val score : t -> 'a
val vies : t -> 'b
val position_balle : t -> position
val vitesse_balle : t -> vitesse
val acceleration_balle : t -> acceleration