(* Définition des types pour les composantes de l'état de la raquette *)

type t = float * bool
let initialiser pos clique = (pos, clique)
let position ((pos, _) : t) = pos
let clique ((_, cliq) : t) = cliq

