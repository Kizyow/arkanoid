
open Brique

(* Module représentant l'état d'un espace de briques dans le cadre d'un jeu. *)

type etatEspaceBrique =
  | Vide
  | Brique of brique
  | Noeud of rect * etatEspaceBrique * etatEspaceBrique * etatEspaceBrique * etatEspaceBrique

(* Fonction pour initialiser l'état de l'espace des briques *)
let initialiser = Vide

(* Vérifie si un point est à l'intérieur d'un rectangle, y compris sur les bords
  Paramètres :
  - rect : (x, y, w, h) : Le rectangle avec ses coordonnées (x, y) et ses dimensions (w, h).
  - point : (px, py) : Le point avec ses coordonnées (px, py).
  Valeur de retour :
  - bool : true si le point est à l'intérieur du rectangle (y compris sur les bords), false sinon. *)
let contains_avec_bord ((x, y, w, h) : rect) ((px, py) : point) =
px >= x && px <= x +. w && py >= y && py <= y +. h

(* Vérifie si un point est à l'intérieur d'un rectangle, sans inclure les bords
  Paramètres :
  - rect : (x, y, w, h) : Le rectangle avec ses coordonnées (x, y) et ses dimensions (w, h).
  - point : (px, py) : Le point avec ses coordonnées (px, py).
  Valeur de retour :
  - bool : true si le point est à l'intérieur du rectangle (sans inclure les bords), false sinon. *)
let contains_sans_bord ((x, y, w, h) : rect) ((px, py) : point) =
px >= x && px < x +. w && py >= y && py < y +. h

(* Vérifie si deux rectangles se chevauchent/sont en collision
  Paramètres :
  - rect1 : (x1, y1, w1, h1) : Le premier rectangle avec ses coordonnées (x1, y1) et ses dimensions (w1, h1).
  - rect2 : (x2, y2, w2, h2) : Le deuxième rectangle avec ses coordonnées (x2, y2) et ses dimensions (w2, h2).
  Valeur de retour :
  - bool : true si les deux rectangles se chevauchent, false sinon. *)
let intersects ((x1, y1, w1, h1) : rect) ((x2, y2, w2, h2) : rect) =
not (x1 +. w1 < x2 || x2 +. w2 < x1 || y1 +. h1 < y2 || y2 +. h2 < y1)

(* Divise un rectangle en quatre sous-rectangles égaux
  Paramètres :
  - rect : (x, y, w, h) : Le rectangle avec ses coordonnées (x, y) et ses dimensions (w, h).
  Valeur de retour :
  - (rect * rect * rect * rect) : Un tuple contenant les quatre sous-rectangles (Nord-Ouest, Nord-Est, Sud-Ouest, Sud-Est). *)
let diviser_rect ((x, y, w, h) : rect) : (rect * rect * rect * rect) =
  let half_w = w /. 2.0 in
  let half_h = h /. 2.0 in
  ( (x, y +. half_h, half_w, half_h), (* Nord-Ouest *)
    (x +. half_w, y +. half_h, half_w, half_h), (* Nord-Est *)
    (x, y, half_w, half_h), (* Sud-Ouest *)
    (x +. half_w, y, half_w, half_h) (* Sud-Est *)
  )
(* Ajoute une nouvelle brique dans le quadtree
   Paramètres :
   - quadtree : etatEspaceBrique : Le quadtree actuel.
   - brique : brique : La brique à ajouter.
   - taille : rect : Les dimensions du rectangle représentant le quadtree.
   Valeur de retour :
   - etatEspaceBrique : Le quadtree mis à jour avec la nouvelle brique ajoutée. *)
let rec ajouter_brique (quadtree : etatEspaceBrique) (brique : brique) (taille : rect) : etatEspaceBrique =
  match quadtree with
  | Vide -> Brique brique (* Si le quadtree est vide, on crée une brique *)
  | Brique brique_actuelle ->
      (* Si le quadtree est une brique, on la divise en 4 et 
          on ajoute la brique existante, puis la nouvelle *)
      let noeud = Noeud (taille, Vide, Vide, Vide, Vide) in
      let noeud = ajouter_brique noeud brique_actuelle taille in
      ajouter_brique noeud brique taille
  | Noeud (rect, no, ne, so, se) ->
      (* Si le quadtree est un noeud, on récupère les 4 sous-quadtree et on ajoute la brique
          dans le sous-quadtree auquelle elle peut appartenir. *)
      let (q1, q2, q3, q4) = diviser_rect rect in
      let (x, y), _, _, _ = brique in (* Coordonnées de la brique *)
      let no' = if contains_sans_bord q1 (x, y) then ajouter_brique no brique q1 else no in
      let ne' = if contains_sans_bord q2 (x, y) then ajouter_brique ne brique q2 else ne in
      let so' = if contains_sans_bord q3 (x, y) then ajouter_brique so brique q3 else so in
      let se' = if contains_sans_bord q4 (x, y) then ajouter_brique se brique q4 else se in
      Noeud (rect, no', ne', so', se')

(* Retire une brique du quadtree.
    Paramètres :
    - quadtree : etatEspaceBrique : Le quadtree actuel.
    - brique : brique : La brique à retirer.
    Valeur de retour :
    - etatEspaceBrique : Le quadtree mis à jour avec la brique retirée (ou inchangé si la brique n'existe pas) . *)
let rec retirer_brique (quadtree : etatEspaceBrique) (brique : brique) : etatEspaceBrique =
  match quadtree with
  | Vide -> Vide (* Si le quadtree est vide, il n'y a rien à supprimer *)
  | Brique brique_actuelle ->
      (* Si le quadtree est une brique et qu'elle est la brique à retirer, on met le quadtree à vide *)
      if brique_actuelle = brique then Vide
      else quadtree
  | Noeud (rect, no, ne, so, se) ->
      (* Si le quadtree est un noeud, on cherche dans quel sous-quadtree la brique peut se trouver *)
      let (q1, q2, q3, q4) = diviser_rect rect in
      let (x, y), _, _, _ = brique in (* Coordonnées de la brique *)
      let no' = if contains_avec_bord q1 (x, y) then retirer_brique no brique else no in
      let ne' = if contains_avec_bord q2 (x, y) then retirer_brique ne brique else ne in
      let so' = if contains_avec_bord q3 (x, y) then retirer_brique so brique else so in
      let se' = if contains_avec_bord q4 (x, y) then retirer_brique se brique else se in

      match (no', ne', so', se') with
      | (Vide, Vide, Vide, Vide) -> Vide (* Remplace le noeud par Vide si tous les sous-noeuds sont vides *)
      | _ -> Noeud (rect, no', ne', so', se') (* Sinon, conserve le noeud avec les sous-noeuds mis à jour *)

(* Recherche toutes les briques dans le quadtree qui intersectent avec un rectangle donné
Paramètres :
- quadtree : etatEspaceBrique : Le quadtree actuel.
- rectangle : rect : Le rectangle avec lequel on veut vérifier les intersections.
Valeur de retour :
- brique list : La liste des briques qui intersectent avec le rectangle donné. *)
let rec query (quadtree : etatEspaceBrique) (rectangle : rect) : brique list =
  match quadtree with
  | Vide -> [] (* Si le quadtree est vide, retourne une liste vide *)
  | Brique brique ->
      let (x, y), w, h, _ = brique in (* Coordonnées, largeur et hauteur de la brique *)
      if intersects rectangle (x, y, w, h) then [brique] (* Si le rectangle intersecte avec la brique, retourne la brique *)
      else []
  | Noeud (rect, no, ne, so, se) ->
      (* Si le quadtree est un noeud, vérifie les intersections dans chaque sous-quadtree et concatène les résultats *)
      if intersects rect rectangle then 
        query no rectangle @ query ne rectangle @ query so rectangle @ query se rectangle
      else []

(* Affiche tous les rectangles du quadtree
  Paramètres :
  - quadtree : etatEspaceBrique : Le quadtree actuel.
  - taille : rect : Le rectangle représentant le quadtree.
  Valeur de retour :
  - rect list : La liste des rectangles du quadtree. *)
let rec afficher_quadtree (quadtree : etatEspaceBrique) (taille : rect) : rect list =
  match quadtree with
  | Vide -> [] (* Si le quadtree est vide, retourne une liste vide *)
  | Brique _ -> [taille] (* Si le quadtree est une brique, retourne une liste vide *)
  | Noeud (rect, no, ne, so, se) ->
      (* Si le quadtree est un noeud, ajoute le rectangle du noeud et concatène les résultats des sous-quadtrees *)
      rect :: (afficher_quadtree no taille @ afficher_quadtree ne taille @ afficher_quadtree so taille @ afficher_quadtree se taille)

(* Vérifie si une brique dans le quadtree est en collision avec un cercle donné
  Paramètres :
  - quadtree : etatEspaceBrique : Le quadtree actuel.
  - xb, yb : float : Les coordonnées du centre du cercle.
  - rayon : float : Le rayon du cercle.
  Valeur de retour :
  - bool : true si une brique est en collision avec le cercle, false sinon. *)
let rec collision_avec_brique (quadtree : etatEspaceBrique) (xb, yb, rayon) : bool =
  match quadtree with
  | Vide -> false (* Si le quadtree est vide, retourne false *)
  | Brique brique ->
      let (x, y), w, h, _ = brique in (* Coordonnées, largeur et hauteur de la brique *)
      contains_avec_bord (x -. rayon, y -. rayon, w +. rayon *. 2.0, h +. rayon *. 2.0) (xb, yb)
      (* Vérifie si le cercle est en collision avec la brique *)
  | Noeud (rect, no, ne, so, se) ->
      (* Si le quadtree est un noeud, vérifie les collisions dans chaque sous-quadtree *)
      if contains_avec_bord rect (xb, yb) then
        collision_avec_brique no (xb, yb, rayon) ||
        collision_avec_brique ne (xb, yb, rayon) ||
        collision_avec_brique so (xb, yb, rayon) ||
        collision_avec_brique se (xb, yb, rayon)
      else false

(* Affiche la structure du quadtree sous forme de chaîne de caractères (pour debug)
  Paramètres :
  - quadtree : etatEspaceBrique : Le quadtree actuel.
  - nb : int : Le numéro de l'itération pour l'affichage.
  Valeur de retour :
  - string : La représentation sous forme de chaîne de caractères du quadtree. *)
let rec print_quadtree (quadtree : etatEspaceBrique) (nb : int) : string =
  match quadtree with
  | Vide -> "Vide\n"
  | Brique ((x, y), _, _, _) -> "Brick (" ^ (string_of_float x) ^ ";" ^ (string_of_float y) ^ ")\n"
  | Noeud (_, no, ne, so, se) ->
      "Iteration " ^ (string_of_int nb) ^ " = NO: " ^ (print_quadtree no (nb + 1)) ^
      "\nNE: " ^ (print_quadtree ne (nb + 1)) ^
      "\nSO: " ^ (print_quadtree so (nb + 1)) ^
      "\nSE: " ^ (print_quadtree se (nb + 1))


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