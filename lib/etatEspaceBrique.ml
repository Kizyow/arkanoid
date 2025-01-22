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

let briques = creer_briques start_x start_y distance_x distance_y num_cols num_rows

let contains (x, y, w, h) (px, py) =
  x <= px && px < x +. w &&
  y <= py && py < y +. h

let intersects quadtree (x2, y2, w2, h2) =
  match quadtree with
  | Vide(_) -> false
  | Feuille((x1, y1, w1, h1), _) -> not (x1 > x2 +. w2 || x1 +. w1 < x2 || y1 > y2 +. h2 || y1 +. h1 < y2)
  | Noeud((x1, y1, w1, h1), _, _, _, _) -> not (x1 > x2 +. w2 || x1 +. w1 < x2 || y1 > y2 +. h2 || y1 +. h1 < y2)


let quadtree_vide (x, y, w, h) = Vide (x, y, w, h)

let divide_aabb (x, y, w, h) =
  let half_w = w /. 2.0 in
  let half_h = h /. 2.0 in
  [ (x, y, half_w, half_h);             (* NO *)
    (x +. half_w, y, half_w, half_h);   (* NE *)
    (x, y +. half_h, half_w, half_h);   (* SO *)
    (x +. half_w, y +. half_h, half_w, half_h)] (* SE *)

let rec insert quadtree point =
  match quadtree with
  | Vide (x, y, w, h) -> 
      Feuille ((x, y, w, h), point)  (* Si la case est vide, créer une feuille avec ce point *)
  
  | Feuille (aabb, p) -> 
      (* Si la feuille contient déjà un point, diviser l'aabb en 4 et réinsérer les points *)
      let [no_aabb; ne_aabb; so_aabb; se_aabb] = divide_aabb aabb in
      let (x, y, w, h) = aabb in
      (* Réinsérer le point existant et le nouveau point dans leurs sous-quadrants respectifs *)
      let no = if contains no_aabb point then Feuille (no_aabb, point) else Vide no_aabb in
      let ne = if contains ne_aabb point then Feuille (ne_aabb, point) else Vide ne_aabb in
      let so = if contains so_aabb point then Feuille (so_aabb, point) else Vide so_aabb in
      let se = if contains se_aabb point then Feuille (se_aabb, point) else Vide se_aabb in
      Noeud (aabb, no, ne, so, se)
  
  | Noeud (aabb, no, ne, so, se) -> 
      (* Si c'est déjà un noeud, déterminer dans quel quadrant insérer le point *)
      let [no_aabb; ne_aabb; so_aabb; se_aabb] = divide_aabb aabb in
      let insert_in_quadrant qt quadrant_aabb =
        if contains quadrant_aabb point then insert qt point else qt
      in
      let no = insert_in_quadrant no no_aabb in
      let ne = insert_in_quadrant ne ne_aabb in
      let so = insert_in_quadrant so so_aabb in
      let se = insert_in_quadrant se se_aabb in
      Noeud (aabb, no, ne, so, se)

let rec query quadtree range found =
  match quadtree with
  | Vide _ -> found
  | Feuille (_, point) -> if contains range point then point :: found else found
  | Noeud (_, no, ne, so, se) ->
    let found = if intersects no range then query no range found else found in
    let found = if intersects ne range then query ne range found else found in
    let found = if intersects so range then query so range found else found in
    let found = if intersects se range then query se range found else found in
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

let initialiser aabb =
  List.fold_left insert (quadtree_vide aabb) briques