(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique
open Brique
open ParametresJeu

(* ouvre la bibliotheque de modules definis dans bin/ *)
open FreeFall
open GestionBalle

(* Module qui gère le déroulement du jeu (hors graphiques) *)

(* Fonction qui initialise l'état du jeu avec une nouvelle balle collée à la raquette. *)
(* paramètres:                                                                         *)
(* etatBrique : etatBrique - Etat des briques                                          *)
(* raquette : etatRaquette - Etat de la raquette                                       *)
(* score : int - Score actuel du jeu                                                   *)
(* nbVies : int - Nombre de vies restantes                                             *)
(* valeur de retour : etatJeu - L'état du jeu avec la balle sur la raquette            *)
let initialiser_jeu_avec_balle_collee etatBrique raquette score nbVies =
  let positionBalle0 = (EtatRaquette.position raquette, FormeRaquette.hauteur +. ParametresBalle.rayon) in
  let vitesse0 = ParametresBalle.vitesse_initiale in
  let acceleration0 = ParametresBalle.acceleration_initiale in
  let etatBalle = EtatBalle.initialiser positionBalle0 vitesse0 acceleration0 in
  EtatJeu.initialiser etatBalle etatBrique raquette score nbVies

(* Fonction qui gère le contact de la balle avec le sol. *)
(* paramètres:                                                                  *)
(* etatJeuEvent : etatJeu - Etat actuel du jeu                                  *)
(* etatBrique : etatBrique - Etat des briques                                   *)
(* valeur de retour : etatJeu - L'état du jeu avec la balle sur la raquette *)
let gerer_contact_sol etatJeuEvent etatBrique : etatJeu =
  (* On perd une vie et on relance une nouvelle balle. *)
  let etatRaquette = EtatJeu.raquette etatJeuEvent in
  let score = EtatJeu.score etatJeuEvent in
  let nbVies = EtatJeu.vies etatJeuEvent in
  initialiser_jeu_avec_balle_collee etatBrique etatRaquette score (nbVies - 1) 

(* Fonction qui gère le contact de la balle avec la raquette.                *)
(* paramètres:                                                               *)
(* etatJeuEvent : etatJeu - Etat actuel du jeu                               *)
(* etatBrique : etatBrique - Etat des briques                                *)
(* valeur de retour : etatJeu - L'état du jeu avec la balle qui est rebondit *)
let gerer_contact_raquette etatJeuEvent etatBrique : etatJeu =
  let etatBalle = EtatJeu.balle etatJeuEvent in
  let etatRaquette = EtatJeu.raquette etatJeuEvent in
  let score = EtatJeu.score etatJeuEvent in
  let nbVies = EtatJeu.vies etatJeuEvent in
  (* La balle rebondit dans une direction définie par rebond_raquette. Elle gagne en accélération.*)
  let nvEtatBalle = GestionBalle.rebond_raquette etatBalle etatRaquette (0.5, 0.5) in
  EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies

(* Fonction qui gère le contact de la balle avec une brique.                                  *)
(* paramètres:                                                                                *)
(* etatJeuEvent : etatJeu - Etat actuel du jeu                                                *)
(* etatBrique : etatBrique - Etat des briques                                                 *)
(* valeur de retour : etatJeu - L'état du jeu avec la balle sur la raquette                   *)
let gerer_contact_brique etatJeuEvent etatBrique : etatJeu =
  let etatBalle = EtatJeu.balle etatJeuEvent in
  let etatRaquette = EtatJeu.raquette etatJeuEvent in
  let (x, y) = EtatBalle.position etatBalle in
  let score = EtatJeu.score etatJeuEvent in
  let nbVies = EtatJeu.vies etatJeuEvent in
  (* On retire la brique que la balle a touché. *)
  let briques = query etatBrique (x -. ParametresBalle.rayon, y -. ParametresBalle.rayon, ParametresBalle.rayon *. 2., ParametresBalle.rayon *. 2.) in
  let brique_retiree = List.hd briques in
  let nvBriques = retirer_brique etatBrique brique_retiree in
  (* La balle rebondit.*)
  let nvEtatBalle = GestionBalle.rebond_brique etatBalle brique_retiree (0.5, 0.5) in
  EtatJeu.initialiser nvEtatBalle nvBriques etatRaquette (score + 100) nbVies 

(* Fonction qui gère le rebond de la balle contre un mur. *)
(* paramètres:                                                        *)
(* etatJeuEvent : etatJeu - Etat actuel du jeu                        *)
(* etatBrique : etatBrique - Etat des briques                         *)
(* valeur de retour : etatJeu - L'état du jeu avec la balle rebondie  *)
let gerer_rebond_mur etatJeuEvent etatBrique =
  let etatBalle = EtatJeu.balle etatJeuEvent in
  let etatRaquette = EtatJeu.raquette etatJeuEvent in
  let score = EtatJeu.score etatJeuEvent in
  let nbVies = EtatJeu.vies etatJeuEvent in
  let nvEtatBalle = GestionBalle.rebond etatBalle (0.5, 0.5) in
  let etatJeu = EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies in
  etatJeu

(* Fonction principale pour gérer les différents types de contacts de la balle.      *)
(* paramètres:                                                                       *)
(* etatJeuEvent : etatJeu - Etat actuel du jeu                                       *)
(* etatBrique : etatBrique - Etat des briques                                        *)
(* valeur de retour : (etatJeu*bool) - L'état du jeu avec la balle modifié,
  et un booléen si la balle redémarre collée sur la raquette (on a touché le sol)  *)
let gerer_contact etatJeuEvent etatBrique =
  let etatBalle = EtatJeu.balle etatJeuEvent in
  let (x, y) = EtatBalle.position etatBalle in
  if GestionBalle.contact_sol etatBalle then
    (* Cas en contact avec le sol *)
    (gerer_contact_sol etatJeuEvent etatBrique),true
  else if GestionBalle.contact_raquette etatBalle (EtatJeu.raquette etatJeuEvent) then
    (* Cas en contact avec la raquette *)
    (gerer_contact_raquette etatJeuEvent etatBrique),false
  else if collision_avec_brique etatBrique (x, y, ParametresBalle.rayon) then
    (* Cas en contact avec une brique *)
    (gerer_contact_brique etatJeuEvent etatBrique),false
  else
    (* Cas en contact avec le mur *)
    (gerer_rebond_mur etatJeuEvent etatBrique),false

(* Fonction principale pour exécuter le jeu                                   *)
(* Cette fonction gère l'état du jeu en fonction du nombre de vies restantes. *)
(* et gère les différents types de contacts de la balle. *)
(* paramètres:                                                                *)
(* etatJeu : etatJeu - Etat actuel du jeu                                     *)
(* balleCollee : bool - Indique si la balle est collée à la raquette ou pas   *)
let rec run (etatJeu: etatJeu) (balleCollee:bool) : etatJeu flux =
  let nbVies = EtatJeu.vies etatJeu in
  if nbVies < 1 then Flux.vide (* Si on n'a plus de vies, on arrête de jouer.*)
  else
    let score = EtatJeu.score etatJeu in
    let etatBrique = EtatJeu.briques etatJeu in
    let fluxEtatRaquetteBalleCollee = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
    (* Si on est dans le cas où l'on doit lancer la balle, on attend que l'utiliseur fasse un clique souris pour la lancer*)
    Flux.unless (Flux.map
      (fun raquette ->
          initialiser_jeu_avec_balle_collee etatBrique raquette score nbVies
      ) fluxEtatRaquetteBalleCollee) (fun jeu -> (not balleCollee || EtatRaquette.clique (EtatJeu.raquette jeu)))
      (fun etatJeuDecollage ->
        (* Si la balle doit se déplacer, on construit un flux basé sur FreeFall *)
        let etatRaquetteFlux = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
        let etatBalleDecollage = if balleCollee then
            (EtatBalle.initialiser (EtatJeu.position_balle etatJeuDecollage) ParametresBalle.vitesse_initiale ParametresBalle.acceleration_initiale)
          else
            (EtatJeu.balle etatJeu)
        in
        let fluxBalle = (FreeFall.run etatBalleDecollage) in
        let fluxJeu = Flux.map2
          (fun balle raquette ->
              EtatJeu.initialiser balle etatBrique raquette score nbVies
          )
          fluxBalle etatRaquetteFlux in
        (Flux.unless_modif fluxJeu GestionBalle.contact
          (* Le flux de la balle en chute libre est arrêté si une collision est détecté. *)
          (fun etatJeuEvent ->
            gerer_contact etatJeuEvent etatBrique (* On récupère le nouvel état de jeu avec la balle modifiée*)
          )
          (fun etatJeuNext balleCollee -> run etatJeuNext balleCollee))) (* On redémarre la balle *)