(* Module proposant des fonctions utilitaires pour gérer l'état d'une brique. *)

(* Définition du type point représentant un point à partir de ses coordonnées (x, y) *)
type point = float * float

(* Définition du type rect représentant un rectangle à partir de ses coordonnées (x, y) et ses dimensions (w, h) *)
type rect = float * float * float * float

(* Définition du type brique représentant une brique à partir de ses coordonnées/point (x, y), ses dimensions (w, h) et sa couleur *)
type brique = point * float * float * string

(* Fonction pour créer des briques séparées par un espace
   Paramètres :
   - limites : rect : Les limites du rectangle dans lequel les briques seront créées.
   - nbColonnes : int : Le nombre de colonnes de briques
   - nbLignes : int : Le nombre de lignes de briques
   Valeur de retour :
   - list brique : Une liste de briques créées avec un espace de deux unités entre elles. *)
let brique_separes_par_deux (limites : rect) (nbColonnes:int) (nbLignes:int) =
  let (x, y, w, h) = limites in
  let largeur_brique = w /. 16.0 in
  let hauteur_brique = h /. 16.0 in

  (* Fonction pour créer une rangée de briques *)
  let creer_ligne ligne =
    List.init nbColonnes (fun col ->
      let brick_x = x +. (float_of_int col) *. largeur_brique *. 2.0 in
      let brick_y = y +. h -. (float_of_int (ligne + 1)) *. hauteur_brique *. 2.0 in
      let color = match ligne with
        | 0 -> "red"
        | 1 -> "green"
        | 2 -> "blue"
        | _ -> "gray"
      in
      ((brick_x, brick_y), largeur_brique, hauteur_brique, color)
    )
  in
  List.concat (List.init nbLignes creer_ligne)

(* Fonction pour créer des briques complètes sans espace entre elles
   Paramètres :
   - limites : rect : Les limites du rectangle dans lequel les briques seront créées.
   - nbColonnes : int : Le nombre de colonnes de briques
   - nbLignes : int : Le nombre de lignes de briques
   Valeur de retour :
   - list brique : Une liste de briques créées sans espace entre elles. *)
let briques_completes (limites : rect) (nbColonnes:int) (nbLignes:int) =
  let (x, y, w, h) = limites in
  let largeur_brique = w /. 16.0 in
  let hauteur_brique = h /. 16.0 in

  (* Fonction pour créer une rangée de briques *)
  let creer_ligne ligne =
    List.init nbColonnes (fun col ->
      let brick_x = x +. (float_of_int col) *. largeur_brique in
      let brick_y = y +. h -. (float_of_int (ligne + 1)) *. hauteur_brique in
      let color = match ligne with
        | 0 -> "red"
        | 1 -> "green"
        | 2 -> "blue"
        | _ -> "gray"
      in
      ((brick_x, brick_y), largeur_brique, hauteur_brique, color)
    )
  in
  List.concat (List.init nbLignes creer_ligne)

(* Fonction pour créer des briques avec ou sans espace entre elles
   Paramètres :
   - limites : rect : Les limites du rectangle dans lequel les briques seront créées.
   - nbColonnes : int : Le nombre de colonnes de briques.
   - nbLignes : int : Le nombre de lignes de briques.
   - avec_espace : bool : Indique si les briques doivent être séparées par un espace.
   Valeur de retour :
   - list brique : Une liste de briques créées avec ou sans espace entre elles. *)
let creer_briques (limites : rect) (nbColonnes : int) (nbLignes : int) (avec_espace : bool) =
  if avec_espace then
    brique_separes_par_deux limites nbColonnes nbLignes
  else
    briques_completes limites nbColonnes nbLignes
  