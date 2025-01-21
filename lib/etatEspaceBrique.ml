
type point = float * float
type aabb = float * float * float * float

type t =
  | Vide of aabb 
  | Feuille of aabb * point
  | Noeud of aabb * t * t * t * t

(* Positions relatives possibles des quadrants.*)
type quadrant =
  | NO
  | NE
  | SO
  | SE

(* Fonction permettant d'ajouter un point dans le quadrant. *)
let rec insert ((x, y):point) = function
  (* Cas du quadrant vide : on remplace tout simplement par une feuille contenant le point.*)
  | Vide(aabb) -> Feuille (aabb,(x, y))
  | Feuille ((aMin,aMax,bMin,bMax),(px,py)) ->
    (* On cherche les coordonnées du centre du quadrant. *)
    let mid_x = (aMin +. aMax) /. 2.0
    and mid_y = (bMin +. bMax) /. 2.0 in
    (* On construit les 4 sous-quadrants. *)
    let q_no = Vide (aMin, mid_x, bMin, mid_y)
    and q_ne = Vide (mid_x, aMax, bMin, mid_y)
    and q_so = Vide (aMin, mid_x, mid_y, bMax)
    and q_se = Vide (mid_x, aMax, mid_y, bMax) in
    (* On réinsère le point existant ainsi que notre nouveau point dans le nouveau noeud. *)
    insert (x,y) (insert (px,py) (Noeud ((aMin, aMax, bMin, bMax), q_no, q_ne, q_so, q_se)))
| Noeud ((aMin, aMax, bMin, bMax), nw, ne, sw, se) ->
  (* Faire coùmmmentaire...*)
    let mid_x = (aMin +. aMax) /. 2.0
    and mid_y = (bMin +. bMax) /. 2.0 in
    if x < mid_x then
      if y < mid_y then
        Noeud ((aMin, aMax, bMin, bMax), insert (x, y) nw, ne, sw, se)
      else
        Noeud ((aMin, aMax, bMin, bMax), nw, ne, insert (x, y) sw, se)
    else
      if y < mid_y then
        Noeud ((aMin, aMax, bMin, bMax), nw, insert (x, y) ne, sw, se)
      else
        Noeud ((aMin, aMax, bMin, bMax), nw, ne, sw, insert (x, y) se)

  (* REFLECHIR :
  il faut réussir à représenter les briques pas carré dans le quadtree qui est carré *)