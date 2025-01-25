(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique
open Brique

(* ouvre la bibliotheque de modules définis dans bin/ *)
open ParametresJeu
open FreeFall
open GestionBalle
open Jeu

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let dessiner_brique (((x, y), w, h, color) : brique) =
  let x_int = int_of_float x in
  let y_int = int_of_float y in
  let w_int = int_of_float w in
  let h_int = int_of_float h in

  (match color with
   | "red" -> Graphics.set_color Graphics.red
   | "green" -> Graphics.set_color Graphics.green
   | "blue" -> Graphics.set_color Graphics.blue
   | _ -> Graphics.set_color (Graphics.rgb 128 128 128)); 

  (* la brique *)
  Graphics.fill_rect x_int y_int w_int h_int;

  (* bordure de la brique *)
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x_int y_int w_int h_int

let dessiner_espace_brique (quadtree : etatEspaceBrique) =
  let taille_jeu = (Box.infx, Box.infy, Box.supx, Box.supy) in
  let briques = query quadtree taille_jeu in
  List.iter dessiner_brique briques

let dessiner_quadtree ((x, y, w, h) : rect) =
  let x_int = int_of_float x in
  let y_int = int_of_float y in
  let w_int = int_of_float w in
  let h_int = int_of_float h in

  Graphics.set_color Graphics.red;
  Graphics.draw_rect x_int y_int w_int h_int

let dessiner_espace_quadtree (quadtree : etatEspaceBrique) =
  let taille_jeu = (Box.infx, Box.infy, Box.supx, Box.supy) in
  let qt = afficher_quadtree quadtree taille_jeu in
  List.iter dessiner_quadtree qt

let dessiner_raquette etatRaquette =
  let x_centre = EtatRaquette.position etatRaquette in
   let x, y, w, h = (x_centre -. (FormeRaquette.longeur)/.2.), 0., FormeRaquette.longeur, FormeRaquette.hauteur in
  Graphics.draw_rect (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h) 

let draw_state (etat: etatJeu) =
  let (x,y) = EtatJeu.position_balle etat in
  Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float ParametresBalle.rayon);
  dessiner_espace_brique (EtatJeu.briques etat);
  if ParametreBrique.voir_bordures_quadtree then dessiner_espace_quadtree (EtatJeu.briques etat) ; 
  Graphics.set_color Graphics.black ;
  dessiner_raquette (EtatJeu.raquette etat);
  Graphics.moveto 10 10 ;
  Graphics.draw_string ("Score : "^(string_of_int (EtatJeu.score etat))) ;
  Graphics.moveto 10 20 ;
  Graphics.draw_string ("Vies : "^(string_of_int (EtatJeu.vies etat)))

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (last_score + EtatJeu.score etat)
  in

  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let () = 
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let x,_ = (Graphics.mouse_pos ()) in
  let x_souris = float_of_int x in
  let etatRaquette = EtatRaquette.initialiser x_souris false in
  let positionBalle0 = (EtatRaquette.position etatRaquette,FormeRaquette.hauteur +. ParametresBalle.rayon) in
  let etatBalle = EtatBalle.initialiser positionBalle0 ParametresBalle.vitesse_initiale ParametresBalle.acceleration_initiale in
  let taille_jeu = (Box.infx, Box.infy, Box.supx, Box.supy) in
  let etatBrique = EtatEspaceBrique.initialiser in 
  let etatBrique = (List.fold_left (fun acc b -> ajouter_brique acc b taille_jeu) etatBrique (Brique.creer_briques taille_jeu ParametreBrique.nbColonnes ParametreBrique.nbLignes ParametreBrique.espace_entre_briques)) in
  let etatJeu = (EtatJeu.initialiser etatBalle etatBrique etatRaquette 0 5) in
  draw (Jeu.run etatJeu true)