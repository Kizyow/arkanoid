(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique
open Brique

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Jeu
open Input
open ParametresJeu

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
  dessiner_espace_quadtree (EtatJeu.briques etat) ;
  Graphics.set_color Graphics.black ;
  dessiner_raquette (EtatJeu.raquette etat);
  Graphics.moveto 10 10 ;
  Graphics.draw_string ("Score : "^(string_of_int (EtatJeu.score etat))) ;
  Graphics.moveto 10 20 ;
  Graphics.draw_string ("Vies : "^(string_of_int (EtatJeu.vies etat)))


(* extrait le score courant d'un etat : *)
let score etat : int = EtatJeu.score etat

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
      loop flux_etat' (last_score + score etat)
  in

  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

(*let () = game_hello ()*)

(* Fonction qui intègre/somme les valeurs successives du flux *)
(* avec un pas de temps dt et une valeur initiale nulle, i.e. *)
(* acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}             *)
(* paramètres:                                                *)
(* dt : float                                                 *)
(* flux : (float * float) flux                              *)
let integre dt flux =
(* valeur initiale de l'intégrateur                         *)
let init = ( 0., 0.) in
(* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
let iter (acc1, acc2) (flux1, flux2) =
  (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
(* définition récursive du flux acc                         *)
let rec acc =
  Tick (lazy (Some (init, Flux.map2 iter acc flux)))
in acc;;

(* Module du modèle dynamique d'une balle en 2D.              *)
(* A partir d'un état initial, run produit le flux des états  *)
(* successifs de la balle, qui pourra être affiché            *)
module FreeFall =
struct
  let (|+|) (x1,y1) (x2,y2) = x1+.x2,y1+.y2
  let run : etatBalle -> etatBalle flux = 
    fun etat0 ->
    let secousseFlux= Flux.constant (0.,0.) in
    let accFlux= Flux.map ((|+|) (EtatBalle.acceleration etat0)) (integre Init.dt secousseFlux) in
    let vitesseFlux = Flux.map ((|+|) (EtatBalle.vitesse etat0)) (integre Init.dt accFlux) in
    let positionFlux = Flux.(map ((|+|) (EtatBalle.position etat0))) (integre Init.dt vitesseFlux) in
    Flux.map3 (fun pos vit acc -> EtatBalle.initialiser pos vit acc) positionFlux vitesseFlux accFlux
end

(* Module proposant des fonctions utilitaire pour gérer l'état d'une balle dans un cadre de jeu. *)
module GestionBalle =
struct

  (* Fonction qui détecte un contact en une dimension. *)
  (* Si la balle touche une des bornes, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                                                                          *)
  (* (inf_x, sup_x) : float * float - l'intervalle du cadre                               *)
  (* x : float - la position de la balle                                                  *)
  (* dx : float - la vitesse de la balle                                                  *)
  let contact_1d inf_x sup_x x dx = (x < inf_x && dx < 0.) || (x > sup_x && dx > 0.)

  (* Fonction qui ............................................................*)
  (* paramètres:                                                                          *)
  (* (inf_x, sup_x) : float * float - l'intervalle du cadre                               *)
  (* x : float - la position de la balle                                                  *)
  (* dx : float - la vitesse de la balle                                                  *)
  let getVitesseRebondBrique (((xbr, ybr), w, h, _):brique) ((xb,yb):position) ((dx, dy):vitesse) =
    if (xbr <= xb && xb <= xbr+.w && ((ybr-.20. <= yb && yb <= ybr+.10.) || (ybr +. h -. 20. <= yb && yb <= ybr+.h+.30.)))
      then (dx,-.dy)
  else
    (-.dx,dy)

  (* Fonction qui vérifie si la balle touche la raquette. *)
  (* Si la balle touche la raquette, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float) - Etat de la balle *)
  (* ((x, y), (dx, dy) : (float * float) * (float * float) - Etat de la raquette *)
  let contact_raquette etatBalle etatRaquette = 
    let (xb,yb) = EtatBalle.position etatBalle in
    let xr = EtatRaquette.position etatRaquette in
    let (_,dy) = EtatBalle.vitesse etatBalle in
    let demi_raquette = (FormeRaquette.longeur/. 2.) in
    (xb<=(xr +. demi_raquette)) && (xb>=(xr -. demi_raquette))
      && (yb <= FormeRaquette.hauteur +. ParametresBalle.rayon +. 2.) && (dy<0.)

  (* Fonction qui détecte un contact en deux dimensions. *)
  (* Si la balle touche une des bornes du cadre de jeu, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float)  - Etat de la balle *)
  let contact (etatJeu:etatJeu) = 
    let etatBalle = EtatJeu.balle etatJeu in
    let etatRaquette = EtatJeu.raquette etatJeu in
    let espaceBrique = EtatJeu.briques etatJeu in
    let (x,y) = EtatBalle.position etatBalle  in
    let (dx,dy) = EtatBalle.vitesse etatBalle  in
    let collisionBrique = collision_avec_brique espaceBrique (x, y, ParametresBalle.rayon) in
    collisionBrique || (contact_1d Box.infx Box.supx x dx) || (contact_1d Box.infy Box.supy y dy) || (contact_raquette etatBalle etatRaquette)

  (* Fonction qui vérifie si la balle touche le sol (bas du cadre). *)
  (* Si la balle touche le bas du cadre, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float) - Etat de la balle *)
  let contact_sol etatBalle = 
    let (_,y) = EtatBalle.position etatBalle in
    let (_,dy) = EtatBalle.vitesse etatBalle in
    y < (Box.infy) && dy < 0.

  (* Fonction qui fait rebondir la balle sur les bords du cadre de jeu. *)
  (* Si la balle touche une des bornes, sa vitesse est inversée, on inverse sa trajectoire. *)
  (* Sinon, sa vitesse reste inchangée. *)
  (* paramètres:                                                                                     *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float) - Etat de la balle     *)
  (* (addAccX, addAccYgetVitesseRebondBrique) : (float * float) - Quantité d'ajout à l'accélération     *)
  let rebond etatBalle (addAccX, addAccY): etatBalle = 
    let (x,y) = EtatBalle.position etatBalle in
    let (dx,dy) = EtatBalle.vitesse etatBalle in
    let (ddx,ddy) = EtatBalle.acceleration etatBalle in
    EtatBalle.initialiser 
      (x, y)
      ((if contact_1d Box.infx Box.supx x dx then -.dx else dx),
      (if contact_1d Box.infy Box.supy y dy then -.dy else dy))
      (ddx+.addAccX, ddy+.addAccY)

  let rebond_brique etatBalle brique (addAccX, addAccY) : etatBalle = 
    let (ddx,ddy) = EtatBalle.acceleration etatBalle in
    EtatBalle.initialiser 
    (EtatBalle.position etatBalle)
      (getVitesseRebondBrique brique (EtatBalle.position etatBalle) (EtatBalle.vitesse etatBalle))
      (ddx+.addAccX, ddy+.addAccY) 

  (*Signature TODO*)
  let rebond_raquette etatBalle etatRaquette (addAccX, addAccY): etatBalle = 
      let (xb,yb) = EtatBalle.position etatBalle in
      let (dxb,dyb) = EtatBalle.vitesse etatBalle in
      let (ddxb,ddyb) = EtatBalle.acceleration etatBalle in
      let xr = EtatRaquette.position etatRaquette in
      
      (* La vitesse horizontale de la balle est modifiée selon la zone qu'elle touche  de la raquette : si c'est au centre, vitesse horizontale vaut 0
      si c'est à gauche, la vitesse horizontale est négative, si c'est à droite, la vitesse horizontale est positive*)

      let coeff_direction = (abs_float (xb -. xr))/.(FormeRaquette.longeur/.2.) in
      let somme_dxb_dyb = abs_float dxb +. abs_float dyb in
      let new_dxb = 
        if (xb -. xr) < 0. then
            -.(somme_dxb_dyb*.coeff_direction)
        else somme_dxb_dyb*.coeff_direction
      in
      EtatBalle.initialiser (xb, yb) (new_dxb, somme_dxb_dyb*.(1.-.coeff_direction)) (ddxb+.addAccX, ddyb+.addAccY)
end

module Jeu = struct
  let rec run (etatJeu: etatJeu) (balleCollee:bool) : etatJeu flux =
    let nbVies = EtatJeu.vies etatJeu in
    if nbVies < 1 then Flux.vide
    else
      let score = EtatJeu.score etatJeu in
      let nbVies = EtatJeu.vies etatJeu in
      let etatBalle0 = EtatJeu.balle etatJeu in
      let etatBrique = EtatJeu.briques etatJeu in
      let fluxBalle = (FreeFall.run etatBalle0) in
      let fluxEtatRaquetteBalleCollee = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
      let fluxJeuBalleCollee = if balleCollee then
        Flux.unless (Flux.map
      (fun raquette ->
          EtatJeu.initialiser (EtatBalle.initialiser ((EtatRaquette.position raquette),FormeRaquette.hauteur) ParametresBalle.vitesse_initiale ParametresBalle.acceleration_initiale) etatBrique raquette score nbVies
      ) fluxEtatRaquetteBalleCollee) (fun jeu -> (EtatRaquette.clique (EtatJeu.raquette jeu))) (fun _ -> Flux.vide) 
      else
        Flux.vide
    in
      let etatRaquetteFlux = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
      let fluxJeu = Flux.map2 
      (fun balle raquette ->
          EtatJeu.initialiser balle etatBrique raquette score nbVies
      )
      fluxBalle etatRaquetteFlux in
      Flux.append  fluxJeuBalleCollee 
      (Flux.unless_modif fluxJeu GestionBalle.contact 
      (fun etatJeuEvent ->
        let etatBalle = EtatJeu.balle etatJeuEvent in
        let etatRaquette = EtatJeu.raquette etatJeuEvent in
        let (x, y) = EtatBalle.position etatBalle in
        if GestionBalle.contact_sol etatBalle then
          (* Si on touche le sol, on perd une vie et on relance une balle *)
          let positionBalle0 = (EtatRaquette.position etatRaquette,FormeRaquette.hauteur +. ParametresBalle.rayon) in
          let vitesse0 = (600.,300.) in
          let acceleration0 = (0.,-90.81) in
          let etatBalle = EtatBalle.initialiser positionBalle0 vitesse0 acceleration0 in
          (EtatJeu.initialiser etatBalle etatBrique etatRaquette score (nbVies-1)), true
        else if GestionBalle.contact_raquette etatBalle etatRaquette then
          (* Si on touche la raquette, on rebondie dans une direction défini par rebond_raquette *)
          let nvEtatBalle = GestionBalle.rebond_raquette etatBalle etatRaquette (0.5, 0.5) in
          (EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies) , false
        else if collision_avec_brique etatBrique (x, y, ParametresBalle.rayon) then
          let briques = query etatBrique (x-. ParametresBalle.rayon, y-.ParametresBalle.rayon, ParametresBalle.rayon*.2., ParametresBalle.rayon*.2.) in
          let brique_retiree = List.hd briques in
          let nvBriques = retirer_brique etatBrique brique_retiree in
          let nvEtatBalle = GestionBalle.rebond_brique etatBalle brique_retiree (0.5, 0.5) in
            (EtatJeu.initialiser nvEtatBalle nvBriques etatRaquette (score+100) nbVies) , false
        else
          (* En cas de rebond contre un mur, on augmente l'accélération de 0.05*)
          let nvEtatBalle = GestionBalle.rebond etatBalle (0.5, 0.5) in
          (EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies) , false
      )
      (fun etatJeuNext balleCollee -> run etatJeuNext balleCollee))
end

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