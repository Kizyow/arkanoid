(* Module du modèle dynamique d'une balle en 2D.              *)
(* A partir d'un état initial, run produit le flux des états  *)
(* successifs de la balle, qui pourra être affiché            *)

(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique
open Brique
open ParametresJeu

let (|+|) (x1,y1) (x2,y2) = x1+.x2,y1+.y2

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

(* Construit un flux d'une balle en chute libre à partir de l'état initiale d'une balle.*)
let run : etatBalle -> etatBalle flux = 
  fun etat0 ->
  let secousseFlux= Flux.constant (0.,0.) in
  let accFlux= Flux.map ((|+|) (EtatBalle.acceleration etat0)) (integre Init.dt secousseFlux) in
  let vitesseFlux = Flux.map ((|+|) (EtatBalle.vitesse etat0)) (integre Init.dt accFlux) in
  let positionFlux = Flux.(map ((|+|) (EtatBalle.position etat0))) (integre Init.dt vitesseFlux) in
  Flux.map3 (fun pos vit acc -> EtatBalle.initialiser pos vit acc) positionFlux vitesseFlux accFlux