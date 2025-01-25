
open Brique

type etatEspaceBrique

val initialiser : etatEspaceBrique

val ajouter_brique : etatEspaceBrique -> brique -> rect -> etatEspaceBrique
val retirer_brique : etatEspaceBrique -> brique -> etatEspaceBrique
val query: etatEspaceBrique -> rect -> brique list
val afficher_quadtree: etatEspaceBrique -> rect -> rect list
val collision_avec_brique: etatEspaceBrique -> (float * float * float) -> bool