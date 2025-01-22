(* Types de données *)
type point = float * float
type aabb = float * float * float * float

(* Type du quadtree *)
type t

(* Type des quadrants *)
type quadrant =
  | NO
  | NE
  | SO
  | SE

val initialiser : aabb -> t


(* Fonctions utilitaires *)
val contains : aabb -> point -> bool
val intersects : t -> aabb -> bool

(* Fonctions pour le quadtree *)
val quadtree_vide : aabb -> t
val insert : t -> point -> t
val query : t -> aabb -> point list -> point list
val remove : t -> point -> t