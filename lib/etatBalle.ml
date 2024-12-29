(* Définition des types pour les composantes de l'état de la balle *)

type position = float * float
type vitesse = float * float
type acceleration = float * float
type t = position * vitesse * acceleration
let initialiser pos vit acc = (pos, vit, acc)
let position ((pos, _, _) : t) = pos
let vitesse ((_, vit, _) : t) = vit
let acceleration ((_, _, acc) : t) = acc

