(* Définition des types pour les composantes de l'état de la balle *)

type position = float * float
type vitesse = float * float
type acceleration = float * float
type etatBalle
val initialiser : position -> vitesse -> acceleration -> etatBalle
val position : etatBalle -> position
val vitesse : etatBalle -> vitesse
val acceleration : etatBalle -> acceleration