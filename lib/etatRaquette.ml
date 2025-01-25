open Iterator

(* Définition des types pour les composantes de l'état de la raquette *)
(* Définition du type t représentant l'état de la raquette *)
type t = float * bool

(* Fonction pour initialiser l'état de la raquette 
   Paramètres :
   - pos : float : La position en X de la raquette.
   - clique : bool : L'état du clic de la souris (true si un bouton est enfoncé, false sinon).
   Valeur de retour :
   - t : L'état de la raquette créé. *)
let initialiser pos clique : t = (pos, clique)

(* Fonction pour obtenir la position de la raquette à partir de son état *)
let position ((pos, _) : t) = pos

(* Fonction pour obtenir l'état du clic de la raquette à partir de son état *)
let clique ((_, cliq) : t) = cliq

(* Fonction qui génère un flux d'état de raquette. Une fois exécutée, elle récupère le flux d'état 
  de la souris (à l'aide de la bibliothèque Graphics) et le transforme en flux d'état de raquette.
  Paramètres :
   - borneInfX : float : La limite gauche à laquelle la raquette peut se déplacer.
   - borneSupX : float : La limite droite à laquelle la raquette peut se déplacer.
   - longueurRaquette : float : La longueur de la raquette.
  Valeur de retour :
  - t flux : Un flux d'états de la raquette.*)
let flux_etat_raquette borneInfX borneSupX longueurRaquette : t flux =
  (* Flux de la position de la souris *)
  let flux_souris = Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()
  in
  (* Calcul de la demi-longueur de la raquette *)
  let demi_raquette = (longueurRaquette /. 2.) in
  (* Fonction pour transformer l'état de la souris en état de la raquette *)
  let f = fun (x_souris, b_clique) ->
    if x_souris -. demi_raquette < borneInfX then
      initialiser (borneInfX +. demi_raquette) b_clique
    else if x_souris +. demi_raquette > borneSupX then
      initialiser (borneSupX -. demi_raquette) b_clique
    else
      initialiser x_souris b_clique
  in
  (* Transformation du flux de la souris en flux de raquette *)
  Flux.map f flux_souris


