type point = float * float
type aabb = float * float * float * float

type t =
  | Vide of aabb
  | Feuille of aabb * point
  | Noeud of aabb * t * t * t * t

type quadrant =
  | NO
  | NE
  | SO
  | SE

let creer_briques start_x start_y distance_x distance_y num_cols num_rows =
  let rec aux current_x current_y row col acc =
    if row = num_rows then acc
    else if col = num_cols then
      aux start_x (current_y +. distance_y) (row + 1) 0 acc
    else
      let new_x = current_x +. distance_x in
      aux new_x current_y row (col + 1) ((current_x, current_y) :: acc)
  in
  List.rev (aux start_x start_y 0 0 [])

let start_x = 24.38
let start_y = 489.38
let distance_x = 48.75
let distance_y = 36.25
let num_cols = 16
let num_rows = 4

let briques = generate_bricks start_x start_y distance_x distance_y num_cols num_rows

let contains (x, y, w, h) (px, py) =
  x <= px && px < x +. w &&
  y <= py && py < y +. h

let intersects (x1, y1, w1, h1) (x2, y2, w2, h2) =
  not (x1 > x2 +. w2 ||
       x1 +. w1 < x2 ||
       y1 > y2 +. h2 ||
       y1 +. h1 < y2)

let quadtree_vide w h = Vide (0., 0., w, h)

let divide_quadtree (Vide (x, y, w, h)) =
  let half_w = w /. 2.0 in
  let half_h = h /. 2.0 in
  Noeud ((x, y, w, h),
         Vide (x, y, half_w, half_h),       (* NO *)
         Vide (x +. half_w, y, half_w, half_h), (* NE *)
         Vide (x, y +. half_h, half_w, half_h), (* SO *)
         Vide (x +. half_w, y +. half_h, half_w, half_h)) (* SE *)

let rec insert quadtree point =
  match quadtree with
  | Vide (x, y, w, h) -> Feuille ((x, y, w, h), point)
  | Feuille (aabb, _) ->
    let node = divide_quadtree (Vide aabb) in
    insert node point
  | Noeud (aabb, no, ne, so, se) ->
    let insert_in_quadrant quadrant qt =
      match quadrant with
      | NO -> insert qt point
      | NE -> insert qt point
      | SO -> insert qt point
      | SE -> insert qt point
    in
    let no = if contains no.aabb point then insert_in_quadrant NO no else no in
    let ne = if contains ne.aabb point then insert_in_quadrant NE ne else ne in
    let so = if contains so.aabb point then insert_in_quadrant SO so else so in
    let se = if contains se.aabb point then insert_in_quadrant SE se else se in
    Noeud (aabb, no, ne, so, se)

let rec query quadtree range found =
  match quadtree with
  | Vide _ -> found
  | Feuille (_, point) -> if contains range point then point :: found else found
  | Noeud (_, no, ne, so, se) ->
    let found = if intersects no.aabb range then query no range found else found in
    let found = if intersects ne.aabb range then query ne range found else found in
    let found = if intersects so.aabb range then query so range found else found in
    let found = if intersects se.aabb range then query se range found else found in
    found

let rec remove quadtree point =
  match quadtree with
  | Vide _ -> quadtree
  | Feuille (aabb, p) when p = point -> Vide aabb
  | Feuille _ -> quadtree
  | Noeud (aabb, no, ne, so, se) ->
    let no = remove no point in
    let ne = remove ne point in
    let so = remove so point in
    let se = remove se point in
    Noeud (aabb, no, ne, so, se)