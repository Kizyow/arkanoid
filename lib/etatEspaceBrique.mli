type point = float * float
type rect = float * float * float * float
type brique = point * float * float * string

type t

val initialiser : t

val ajouter_brique : t -> brique -> rect -> t
val retirer_brique : t -> brique -> t
val query: t -> rect -> brique list
val collision_avec_brique: t -> (float * float * float) -> bool