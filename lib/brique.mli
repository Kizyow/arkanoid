type point = float * float
type rect = float * float * float * float
type brique = point * float * float * string

val creer_briques :
  rect -> int -> int -> bool -> ((float * float) * float * float * string) list
