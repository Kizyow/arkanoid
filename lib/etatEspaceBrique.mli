type point = float * float
type rect = float * float * float * float

type t

val initialiser : rect -> t

val ajouter_briques : t -> point list -> t
val retirer_brique : t -> point -> t
val est_dans_brique: t -> point -> bool

val liste_briques : t -> rect list
val rectangle : t -> rect