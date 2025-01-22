(* Définition des types pour les composantes de l'état de la raquette *)

type t = float * float
let initialiser pos vit = (pos, vit)
let position ((pos, _) : t) = pos
let vitesse ((_, vit) : t) = vit

