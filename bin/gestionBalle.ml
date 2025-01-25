(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique
open Brique
open ParametresJeu

(* Module proposant des fonctions utilitaire pour gérer l'état d'une balle dans un cadre de jeu. *)

(* Fonction qui détecte un contact en une dimension. *)
(* Si la balle touche une des bornes, elle retourne vrai. *)
(* Sinon, elle retourne faux. *)
(* paramètres:                                                                          *)
(* (inf_x, sup_x) : float * float - l'intervalle du cadre                               *)
(* x : float - la position de la balle                                                  *)
(* dx : float - la vitesse de la balle                                                  *)
let contact_1d inf_x sup_x x dx = (x < inf_x && dx < 0.) || (x > sup_x && dx > 0.)

(* Fonction qui calcule la nouvelle vitesse de la balle après un rebond sur une brique. *)
(* paramètres:                                                                          *)
(* ((xbr, ybr), w, h, _) : brique - les coordonnées (x, y) de la brique, sa largeur (w), sa hauteur (h) *)
(* (xb, yb) : position - la position de la balle                                       *)
(* (dx, dy) : vitesse - la vitesse de la balle                                         *)
let getVitesseRebondBrique (((xbr, ybr), w, h, _):brique) ((xb,yb):position) ((dx, dy):vitesse) =
  let augmentation = 3. in
  let ybr = ybr-.augmentation in
  let xbr = xbr-.augmentation in
  let w = w +.2.*.augmentation in
  let h = h +.2.*.augmentation in

  let xb = xb-.xbr in
  let yb = yb-.ybr in

  (*let diagA = (-.(h/.w)*.xb +. (ybr+.((h/.w))*.xbr)) in
  let diagB = ((h/.w)*.xb +. ((ybr-.h)-.(h/.w)*.xbr)) in*)

  let diagA = (((h/.2.)-.(h))/.((w/.2.)))*.xb +.h in
  let diagB = (((h/.2.))/.((w/.2.)))*.xb  in

  if ((yb>diagA && yb>diagB)||(yb<diagA && yb<diagB)) then (dx,-.dy)
  else  (-.dx, dy)
  



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

(* Fonction qui fait rebondir la balle sur une brique. *)
(* paramètres:                                                                                     *)
(* etatBalle : etatBalle - Etat de la balle                                                       *)
(* brique : brique - La brique touchée                                                             *)
(* (addAccX, addAccY) : (float * float) - Quantité d'ajout à l'accélération                       *)
let rebond_brique etatBalle brique (addAccX, addAccY) : etatBalle =
  let (ddx,ddy) = EtatBalle.acceleration etatBalle in
  EtatBalle.initialiser
    (EtatBalle.position etatBalle)
    (getVitesseRebondBrique brique (EtatBalle.position etatBalle) (EtatBalle.vitesse etatBalle))
    (ddx+.addAccX, ddy+.addAccY)

(* Fonction qui fait rebondir la balle sur la raquette. *)
(* paramètres:                                                                   *)
(* etatBalle : etatBalle - Etat de la balle                                      *)
(* etatRaquette : etatRaquette - Etat de la raquette                             *)
(* (addAccX, addAccY) : (float * float) - Quantité d'ajout à l'accélération      *)
let rebond_raquette etatBalle etatRaquette (addAccX, addAccY): etatBalle =
  let (xb,yb) = EtatBalle.position etatBalle in
  let (dxb,dyb) = EtatBalle.vitesse etatBalle in
  let (ddxb,ddyb) = EtatBalle.acceleration etatBalle in
  let xr = EtatRaquette.position etatRaquette in

  (* La vitesse horizontale de la balle est modifiée selon la zone qu'elle touche de la raquette : *)
  (* si c'est au centre, vitesse horizontale vaut 0, si c'est à gauche, la vitesse horizontale est négative, *)
  (* si c'est à droite, la vitesse horizontale est positive *)
  let coeff_direction = (abs_float (xb -. xr))/.(FormeRaquette.longeur/.2.) in
  let somme_dxb_dyb = abs_float dxb +. abs_float dyb in
  let new_dxb =
    if (xb -. xr) < 0. then
      -.(somme_dxb_dyb*.coeff_direction)
    else somme_dxb_dyb*.coeff_direction
  in
  EtatBalle.initialiser (xb, yb) (new_dxb, somme_dxb_dyb*.(1.-.coeff_direction)) (ddxb+.addAccX, ddyb+.addAccY)
