(* Définition des types pour les composantes de l'état de la balle *)

type position = float * float
type vitesse = float * float
type acceleration = float * float
type t
val initialiser : position -> vitesse -> acceleration -> t
val position : t -> position
val vitesse : t -> vitesse
val acceleration : t -> acceleration