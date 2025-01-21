(* Définition des types pour les composantes de l'état de la raquette *)

type position = float
type vitesse = float
type t = position * vitesse
let initialiser pos vit = (pos, vit)
let position ((pos, _) : t) = pos
let vitesse ((_, vit) : t) = vit

