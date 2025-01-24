type point = float * float
type rect = float * float * float * float
type brique = point * float * float * string

type t =
  | Vide
  | Brique of brique
  | Noeud of rect * t * t * t * t

let initialiser = Vide

(* Vérifier que le point est dans le rectangle *)
let contains ((x, y, w, h) : rect) ((px, py) : point) =
  px >= x && px <= x +. w &&  py >= y && py <= y +. h

(* Vérifier que le point est dans le rectangle *)
let contains_qt ((x, y, w, h) : rect) ((px, py) : point) =
  px >= x && px < x +. w &&  py >= y && py < y +. h

(* Vérifier que deux rectangles sont en collision *)
let intersects ((x1, y1, w1, h1) : rect) ((x2, y2, w2, h2) : rect) =
  not (x1 +. w1 < x2 || x2 +. w2 < x1 || y1 +. h1 < y2 || y2 +. h2 < y1)

(* Diviser un rectangle en 4 sous rectangle *)
let diviser_rect ((x, y, w, h) : rect) : (rect * rect * rect * rect) =
  let half_w = w /. 2.0 in
  let half_h = h /. 2.0 in
  ( (x, y +. half_h, half_w, half_h), (* NO *)
    (x +. half_w, y +. half_h, half_w, half_h), (* NE *)
    (x, y, half_w, half_h), (* SO *)
    (x +. half_w, y, half_w, half_h) (* SE *)
  )

(* Ajouter une nouvelle brique dans le quadtree *)
let rec ajouter_brique (quadtree : t) (brique : brique) (taille : rect) : t =
  match quadtree with
  | Vide -> Brique brique (* si le quadtree est vide, on crée une brique *)
  | Brique brique_actuelle -> (* si le quadtree est une brique, on la divise en 4 et on ajoute la nouvelle brique et on réajoute la brique existante *)
      let noeud = Noeud (taille, Vide, Vide, Vide, Vide) in
      let noeud = ajouter_brique noeud brique_actuelle taille in
      ajouter_brique noeud brique taille
  | Noeud (rect, no, ne, so, se) -> 
      (* si le quadtree est un noeud, on récupère les 4 sous-quadtree et on vérifie si les coords de la brique *)
      (* peuvent appartenir à ce sous-quadtree, et si oui, on l'ajoute sinon on garde le sous-quadtree existant *)
      let (q1, q2, q3, q4) = diviser_rect rect in
      let (x, y), _, _, _ = brique in (* coordonées de la brique *)
      let no' = if contains_qt q1 (x, y) then ajouter_brique no brique q1 else no in
      let ne' = if contains_qt q2 (x, y) then ajouter_brique ne brique q2 else ne in
      let so' = if contains_qt q3 (x, y) then ajouter_brique so brique q3 else so in
      let se' = if contains_qt q4 (x, y) then ajouter_brique se brique q4 else se in
      Noeud (rect, no', ne', so', se')

(* Retirer une brique du quadtree *)
let rec retirer_brique (quadtree : t) (brique : brique) : t =
  match quadtree with
  | Vide -> Vide (* si quadtree vide, alors y'a rien à supprimer *)
  | Brique brique_actuelle -> (* si le quadtree est une brique et qui est égale à notre brique, alors on le met vide *)
      if brique_actuelle = brique then Vide
      else quadtree
  | Noeud (rect, no, ne, so, se) -> (* si quadtree est un noeud, alors on cherche dans quel sous-quadtree peut contenir la brique *)
      let (q1, q2, q3, q4) = diviser_rect rect in
      let (x, y), _, _, _ = brique in (* coordonées de la brique *)
      let no' = if contains q1 (x, y) then retirer_brique no brique else no in
      let ne' = if contains q2 (x, y) then retirer_brique ne brique else ne in
      let so' = if contains q3 (x, y) then retirer_brique so brique else so in
      let se' = if contains q4 (x, y) then retirer_brique se brique else se in

      match (no', ne', so', se') with
      | (Vide, Vide, Vide, Vide) -> Vide (* Replace the node with Vide if all subnodes are empty *)
      | _ -> Noeud (rect, no', ne', so', se') (* Otherwise, keep the node with updated subnodes *)

let rec query (quadtree : t) (taille : rect) : brique list =
  match quadtree with
  | Vide -> [] (* quadtree vide, donc liste vide *)
  | Brique brique ->
      let (x, y), w, h, _ = brique in (* coordonnées, longueur et hauteur de la brique *)
      if intersects taille (x, y, w, h) then [brique] (* si taille est en collision avec la brique, alors on retourne la brique *)
      else []
  | Noeud (rect, no, ne, so, se) -> (* on cherche la collision si elle existe dans chaque sous-quadtree et on append *)
      if intersects rect taille then query no taille @ query ne taille @ query so taille @ query se taille
      else []

let rec afficher_quadtree (quadtree : t) (taille : rect) : rect list =
  match quadtree with
  | Vide -> [] (* quadtree vide, donc liste vide *)
  | Brique _ -> []
  | Noeud (rect, no, ne, so, se) -> (* on cherche la collision si elle existe dans chaque sous-quadtree et on append *)
      rect::(afficher_quadtree no taille @ afficher_quadtree ne taille @ afficher_quadtree so taille @ afficher_quadtree se taille)

let rec collision_avec_brique (quadtree : t) (xb, yb, rayon) : bool =
  match quadtree with
  | Vide -> false (* quadtree vide, donc pas de collision *)
  | Brique brique ->
      let (x, y), w, h, _ = brique in (* coordonnées, longueur et hauteur de la brique *)
      contains (x -. rayon, y -. rayon, w +. rayon *. 2.0, h +. rayon *. 2.0) (xb, yb)  (* si taille est en collision avec la brique, alors y'a collision *)
  | Noeud (rect, no, ne, so, se) -> (* on cherche la collision si elle existe dans chaque sous-quadtree et on append *)
      if contains rect (xb, yb) then 
        collision_avec_brique no (xb, yb, rayon) ||
        collision_avec_brique ne (xb, yb, rayon) ||
        collision_avec_brique so (xb, yb, rayon) ||
        collision_avec_brique se (xb, yb, rayon)
      else false

let rec print_quadtree (quadtree : t) (nb : int) : string =
  match quadtree with
  | Vide -> "Vide\n"
  | Brique ((x, y), _, _, _) -> "Brick ("^(string_of_float x)^";"^(string_of_float y)^")\n"
  | Noeud (rect, no, ne, so, se) ->
    "Iteration "^(string_of_int nb)^" = NO: "^(print_quadtree no (nb+1))^"\nNE: "^(print_quadtree ne (nb+1))^"\nSO: "^(print_quadtree so (nb+1))^"\nSE: "^(print_quadtree se (nb+1))


(* On ajoute une brique dans un quadtree vide *)
let%test "Ajout brique dans un QT vide" =
  let quadtree = initialiser in
  let brique1 = ((100.0, 500.0), 50.0, 20.0, "red") in
  let bounds = (0.0, 0.0, 800.0, 600.0) in
  let quadtree = ajouter_brique quadtree brique1 bounds in
  match quadtree with
  | Brique b -> b = brique1
  | _ -> false

(* On ajoute 4 briques à chaque sous-quadtree du quadtree principal *)
let%test "Ajout de 4 briques dans le quadtree aux 4 coins" =
  let quadtree = initialiser in
  let brique1 = ((100.0, 500.0), 50.0, 20.0, "red") in
  let brique2 = ((500.0, 500.0), 50.0, 20.0, "red") in
  let brique3 = ((175.0, 175.0), 50.0, 20.0, "red") in
  let brique4 = ((560.0, 175.0), 50.0, 20.0, "red") in
  let bounds = (0.0, 0.0, 800.0, 600.0) in
  let quadtree = ajouter_brique quadtree brique1 bounds in
  let quadtree = ajouter_brique quadtree brique2 bounds in
  let quadtree = ajouter_brique quadtree brique3 bounds in
  let quadtree = ajouter_brique quadtree brique4 bounds in
  match quadtree with
  | Noeud (_, no, ne, so, se) ->
      (match no, ne, so, se with
       |  Brique b1, Brique b2, Brique b3, Brique b4 -> b1 = brique1 && b2 = brique2 && b3 = brique3 && b4 = brique4 
       | _ -> false)
  | _ -> false

(* On retire la brique nord-est du quadtree *)
let%test "On retire la brique nord-ouest du quadtree" =
  let quadtree = initialiser in
  let brique1 = ((100.0, 500.0), 50.0, 20.0, "red") in
  let brique2 = ((500.0, 500.0), 50.0, 20.0, "red") in
  let brique3 = ((175.0, 175.0), 50.0, 20.0, "red") in
  let brique4 = ((560.0, 175.0), 50.0, 20.0, "red") in
  let bounds = (0.0, 0.0, 800.0, 600.0) in
  let quadtree = ajouter_brique quadtree brique1 bounds in
  let quadtree = ajouter_brique quadtree brique2 bounds in
  let quadtree = ajouter_brique quadtree brique3 bounds in
  let quadtree = ajouter_brique quadtree brique4 bounds in
  let quadtree = retirer_brique quadtree brique2 in
  match quadtree with
  | Noeud (_, no, ne, so, se) ->
      (match no, ne, so, se with
      |  Brique b1, Vide, Brique b3, Brique b4 -> b1 = brique1 && b3 = brique3 && b4 = brique4 
      | _ -> false)
  | _ -> false

(* On cherche la brique nord-ouest à partir des coords d'une balle de rayon 5 *)
let%test "Chercher la brique nord-ouest" =
  let quadtree = initialiser in
  let brique1 = ((100.0, 500.0), 50.0, 20.0, "red") in
  let brique2 = ((500.0, 500.0), 50.0, 20.0, "red") in
  let brique3 = ((175.0, 175.0), 50.0, 20.0, "red") in
  let brique4 = ((560.0, 175.0), 50.0, 20.0, "red") in
  let bounds = (0.0, 0.0, 800.0, 600.0) in
  let quadtree = ajouter_brique quadtree brique1 bounds in
  let quadtree = ajouter_brique quadtree brique2 bounds in
  let quadtree = ajouter_brique quadtree brique3 bounds in
  let quadtree = ajouter_brique quadtree brique4 bounds in
  let result = query quadtree (500.0, 490.0, 10.0, 10.0) in
  List.length result = 1 && List.hd result = brique2