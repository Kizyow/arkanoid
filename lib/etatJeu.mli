
open EtatBalle
open EtatEspaceBrique
open EtatRaquette
(* Définition des types pour les composantes de l'état du jeu *)

type score = int
and vies = int

type t
val initialiser : EtatBalle.t -> EtatEspaceBrique.t -> EtatRaquette.t -> score -> vies -> t
val balle : t -> EtatBalle.t
val briques : t -> EtatEspaceBrique.t
val raquette : t -> EtatRaquette.t
val score : t -> score
val vies : t -> vies
val position_balle : t -> position
val vitesse_balle : t -> vitesse
val acceleration_balle : t -> acceleration