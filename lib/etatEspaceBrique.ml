type point = float * float
type rect = float * float * float * float

type t =
  | Vide of rect
  | Brique of rect * point
  | Noeud of rect * t * t * t * t

let initialiser rect = (Vide rect)

let contains (x, y, w, h) (px, py) =
  x <= px && px < x +. w &&
  y <= py && py < y +. h

let intersects quadtree (x2, y2, w2, h2) =
  match quadtree with
  | Vide(_) -> false
  | Brique((x1, y1, w1, h1), _) -> not (x1 > x2 +. w2 || x1 +. w1 < x2 || y1 > y2 +. h2 || y1 +. h1 < y2)
  | Noeud((x1, y1, w1, h1), _, _, _, _) -> not (x1 > x2 +. w2 || x1 +. w1 < x2 || y1 > y2 +. h2 || y1 +. h1 < y2)


let diviser_rect (x, y, w, h) =
  let half_w = w /. 2.0 in
  let half_h = h /. 2.0 in
  ((x, y, half_w, half_h),             (* NO *)
    (x +. half_w, y, half_w, half_h),   (* NE *)
    (x, y +. half_h, half_w, half_h),   (* SO *)
    (x +. half_w, y +. half_h, half_w, half_h)) (* SE *)

let rectangle quadtree =
  match quadtree with
  | Vide(rect) -> rect
  | Brique(rect, _) -> rect
  | Noeud(rect, _, _, _, _) -> rect

let rec ajouter_brique quadtree point =
  match quadtree with
  | Vide (x, y, w, h) -> Brique ((x, y, w, h), point)
  
  | Brique (rect, p) -> 
      (* Si la Brique contient déjà un point, diviser l'rect en 4 et réinsérer les points *)
      let (no_rect, ne_rect, so_rect, se_rect) = diviser_rect rect in
      let (x, y, w, h) = rect in
      (* Réinsérer le déjà point existant dans le nouveau sous-quadrants approprié *)
      let no = if contains no_rect p then Brique (no_rect, point) else Vide no_rect in
      let ne = if contains ne_rect p then Brique (ne_rect, point) else Vide ne_rect in
      let so = if contains so_rect p then Brique (so_rect, point) else Vide so_rect in
      let se = if contains se_rect p then Brique (se_rect, point) else Vide se_rect in
      (* Insérer le nouveau point dans le nouveau quadrant *)
      ajouter_brique (Noeud(rect, no, ne, so, se)) point
  
  | Noeud (rect, no, ne, so, se) -> 
      (* Si c'est déjà un noeud, déterminer dans quel quadrant insérer le point *)
      let (no_rect, ne_rect, so_rect, se_rect) = (rectangle no, rectangle ne, rectangle so, rectangle se) in
      let inserer_point_si_correct qt quadrant_rect =
        if contains quadrant_rect point then ajouter_brique qt point else qt
      in
      let no = inserer_point_si_correct no no_rect in
      let ne = inserer_point_si_correct ne ne_rect in
      let so = inserer_point_si_correct so so_rect in
      let se = inserer_point_si_correct se se_rect in
      Noeud (rect, no, ne, so, se)

let rec ajouter_briques quadtree points =
  List.fold_left ajouter_brique quadtree points

let rec retirer_brique quadtree point =
  match quadtree with
  | Vide _ -> quadtree
  | Brique (rect, p) when p = point -> Vide rect
  | Brique _ -> quadtree
  | Noeud (rect, no, ne, so, se) ->
    let no = retirer_brique no point in
    let ne = retirer_brique ne point in
    let so = retirer_brique so point in
    let se = retirer_brique se point in
    Noeud (rect, no, ne, so, se)

let rec est_dans_brique quadtree range found =
  match quadtree with
  | Vide _ -> found
  | Brique (_, point) -> if contains range point then point :: found else found
  | Noeud (_, no, ne, so, se) ->
    let found = if intersects no range then est_dans_brique no range found else found in
    let found = if intersects ne range then est_dans_brique ne range found else found in
    let found = if intersects so range then est_dans_brique so range found else found in
    let found = if intersects se range then est_dans_brique se range found else found in
    found


let liste_briques (quadtree : t) : rect list =
  let rec aux (quadtree:t) (acc:rect list) =
  match quadtree with 
  | Vide _-> []
  | Brique(rect, _) -> [rect]
  | Noeud(_, no, ne, so, se) -> (aux no acc) @ (aux ne acc) @ (aux so acc) @ (aux se acc) 
  in aux quadtree []