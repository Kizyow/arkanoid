(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open EtatJeu
open EtatRaquette
open EtatBalle
open EtatEspaceBrique

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game
open Input


type etatBalle = EtatBalle.t
type etatJeu = EtatJeu.t
type etatRaquette = EtatRaquette.t
type etatEspaceBrique = EtatEspaceBrique.t

module Init = struct
  let dt = (1000. /. 60.)/. 600. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module FormeRaquette = struct
  let hauteur = 5.
  let longeur = 40.
end

module FormeBalle = struct
  let rayon = 5.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let dessiner_briques (quadtree:etatEspaceBrique) =
  let rec forEach (liste:rect list) =
    match liste with
    | [] -> ()
    | (x, y, w, h)::q -> Graphics.draw_rect (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h) ; forEach q
  in forEach (liste_briques quadtree)

let dessiner_raquette etatRaquette =
  let x_centre = EtatRaquette.position etatRaquette in
   let x, y, w, h = (x_centre -. FormeRaquette.longeur) /. 2.0, 0., FormeRaquette.longeur, FormeRaquette.hauteur in
  Graphics.draw_rect (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h) 

let draw_state (etat: etatJeu) =
  let (x,y) = EtatJeu.position_balle etat in
  Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float FormeBalle.rayon);
  (*dessiner_briques (EtatJeu.briques etat)*)
  dessiner_raquette (EtatJeu.raquette etat)


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
      (* Ajouter le texte pour dire le nombre de vie et le score en cours...*)
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (last_score + score etat)
    | _ -> assert false
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
(* flux : (float * float) Flux.t                              *)
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
  let run : etatBalle -> etatBalle Flux.t = 
    fun etat0 ->
    let secousseFlux= Flux.constant (0.,0.) in
    let accFlux= Flux.map ((|+|) (EtatBalle.acceleration etat0)) (integre Init.dt secousseFlux) in
    let vitesseFlux = Flux.map ((|+|) (EtatBalle.vitesse etat0)) (integre Init.dt accFlux) in
    let positionFlux = Flux.(map ((|+|) (EtatBalle.position etat0))) (integre Init.dt vitesseFlux) in
    Flux.map3 (fun pos vit acc -> EtatBalle.initialiser pos vit acc) positionFlux vitesseFlux accFlux
end

  (* Fonction qui filtre et transforme les éléments d'un flux en fonction d'une condition *)
  (* Si un élément satisfait la condition, il est transformé par une fonction donnée.     *)
  (* Sinon, il est laissé inchangé.                                                       *)
  (* paramètres:                                                                          *)
  (* flux : 'a Flux.t                                                                     *)
  (* cond : 'a -> bool                                                                    *)
  (* f_cond : 'a -> 'a Flux.t                                                             *)
  let rec unless : 'a Flux.t-> ('a -> bool) -> ('a -> 'a Flux.t) -> 'a Flux.t =
    fun flux cond f_cond ->
      Tick 
        (lazy
          (match (Flux.uncons flux) with
          |None -> None
          |Some (t,q) ->
              if (cond t) then
                Flux.uncons (f_cond t)
              else
                Some(t,unless q cond f_cond))
        )


  let rec unless_2 : 'a Flux.t-> 'b Flux.t-> ('a -> 'b -> bool) -> ('a -> 'b -> 'a Flux.t) -> 'a Flux.t =
    fun fluxBalle fluxRaquette cond f_cond ->
      Tick 
        (lazy
          (match (Flux.uncons fluxBalle) , (Flux.uncons fluxRaquette) with
          |None,_ |_,None -> None
          |Some (tb,qb), Some(tr,qr) ->
              if (cond tb tr) then
                Flux.uncons (f_cond tb tr)
              else
                Some(tb,unless_2 qb qr cond f_cond)))

(* Mettre en place une continuation pour chaque déplacement de souris
    et chaque touche de bloc pour incrémenter score et exploser bloc ?*)

    let getEtatRaquette x_raquette_precedent : etatRaquette =
      let x,_ = (Graphics.mouse_pos ()) in
      let x_souris = float_of_int x in
      let demi_raquette = (FormeRaquette.longeur /. 2.0) in
      let x_raquette = if x_souris +. demi_raquette < Box.infx then Box.infx +. demi_raquette
      else if x_souris +. demi_raquette > Box.supx then Box.supx -. demi_raquette
      else x_souris in
      EtatRaquette.initialiser x_raquette ((x_raquette -. x_raquette_precedent) /. Init.dt)
  
    let flux_etat_raquette (etatRaquette:etatRaquette) =
      let etatInit = getEtatRaquette (EtatRaquette.position etatRaquette) in
      Flux.unfold (fun etat -> Some(etat, (getEtatRaquette (EtatRaquette.position etat)))) etatInit

(* Module représentant une balle qui se déplace dans l'espace et rebondit sur les bords du cadre de jeu. *)
module Bouncing =
struct

  (* Fonction qui détecte un contact en une dimension. *)
  (* Si la balle touche une des bornes, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                                                                          *)
  (* (inf_x, sup_x) : float * float - l'intervalle du cadre                               *)
  (* x : float - la position de la balle                                                  *)
  (* dx : float - la vitesse de la balle                                                  *)
  let contact_1d inf_x sup_x x dx = (x < inf_x && dx < 0.) || (x > sup_x && dx > 0.)

  (* Fonction qui vérifie si la balle touche la raquette. *)
  (* Si la balle touche la raquette, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float) - Etat de la balle *)
  (* ((x, y), (dx, dy) : (float * float) * (float * float) - Etat de la raquette *)
  let contact_raquette etatBalle etatRaquette = 
    let (xb,yb) = EtatBalle.position etatBalle in
    let xr = EtatRaquette.position etatRaquette in
    let (dx,dy) = EtatBalle.vitesse etatBalle in
    let demi_raquette = (FormeRaquette.longeur/. 2.) in
    (xb<=(xr +. demi_raquette)) && (xb>=(xr -. demi_raquette)) && (yb <= FormeRaquette.hauteur +. (FormeRaquette.hauteur/.2.+.FormeBalle.rayon)) && (dy<0.)

  (* Fonction qui détecte un contact en deux dimensions. *)
  (* Si la balle touche une des bornes du cadre de jeu, elle retourne vrai. *)
  (* Sinon, elle retourne faux. *)
  (* paramètres:                *)
  (* ((x, y), (dx, dy), (ddx, ddy)) : (float * float) * (float * float) * (float * float)  - Etat de la balle *)
  let contact etatBalle etatRaquette = 
    let (x,y) = EtatBalle.position etatBalle  in
    let (dx,dy) = EtatBalle.vitesse etatBalle  in
    (contact_1d Box.infx Box.supx x dx) || (contact_1d Box.infy Box.supy y dy) || (contact_raquette etatBalle etatRaquette)

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
  (* (addAccX, addAccY) : (float * float) - Quantité d'ajout à l'accélération     *)
  let rebond etatBalle (addAccX, addAccY): etatBalle = 
    let (x,y) = EtatBalle.position etatBalle in
    let (dx,dy) = EtatBalle.vitesse etatBalle in
    let (ddx,ddy) = EtatBalle.acceleration etatBalle in
    EtatBalle.initialiser 
      (x, y)
      ((if contact_1d Box.infx Box.supx x dx then -.dx else dx),
      (if contact_1d Box.infy Box.supy y dy then -.dy else dy))
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
    
    
  (* Fonction récursive qui simule le mouvement de la balle. *)
  (* Si la balle touche une des bornes, elle rebondit. *)
  (* Si la balle touche une des bornes, elle rebondit. *)
  (* Si la balle touche le bas du cadre, le jeu s'arrête. *)
  (* Sinon, elle continue son mouvement. *)
  (* paramètres:                                                                          *)
  (* etatBalle0 : (float * float) * (float * float)  * (float * float)                         *)
  (*let rec run (etatJeu: etatJeu) : etatJeu Flux.t =
    let etatRaquetteFlux = flux_etat_raquette (EtatJeu.raquette etatJeu) in
    let etatBrique = EtatJeu.briques etatJeu in
    let score = EtatJeu.score etatJeu in
    let vies = EtatJeu.vies etatJeu in
    Flux.map2 
    (fun etatBalle etatRaquette -> EtatJeu.initialiser etatBalle etatBrique etatRaquette score vies)
    (unless_2 (FreeFall.run (EtatJeu.balle etatJeu)) etatRaquetteFlux contact (fun etatBalle etatRaquette ->
      if contact_sol etatBalle then
        Flux.vide
        (* Si on touche le sol, on perd une vie et on relance une balle
        let acc0 = (0., -9.81) in
        let position0 = (200., 300.) in
        let vitesse0 = (20., -100.) in
        let etatBalle0 = EtatBalle.initialiser position0 vitesse0 acc0 in
        (run etatBalle0 (score+10)) *)
      else if contact_raquette etatBalle (EtatJeu.raquette etatJeu) then
        let nvEtatBalle = (rebond_raquette etatBalle (EtatJeu.raquette etatJeu) (0.5, 0.5)) in
        let nvEtatJeu = EtatJeu.initialiser nvEtatBalle (EtatJeu.briques etatJeu) etatRaquette score vies in
        run nvEtatJeu

      else
        (* En cas de rebond contre un mur, on augmente l'accélération de 0.05*)
        let nvEtatBalle = rebond etatBalle (0.5, 0.5) in
        let nvEtatJeu = EtatJeu.initialiser nvEtatBalle (EtatJeu.briques etatJeu) etatRaquette score vies in
        run nvEtatJeu
    ))
    etatRaquetteFlux*)
    let rec run (etatJeu: etatJeu) : etatJeu Flux.t =
      let etatRaquetteFlux = flux_etat_raquette (EtatJeu.raquette etatJeu) in
      let etatBrique = EtatJeu.briques etatJeu in
      let score = EtatJeu.score etatJeu in
      let vies = EtatJeu.vies etatJeu in
    
      Flux.map2
        (fun etatBalle etatRaquette ->
          if contact_sol etatBalle then
            (* Si on touche le sol, on perd une vie et on relance une balle *)
            let acc0 = (0., -9.81) in
            let position0 = (200., 300.) in
            let vitesse0 = (20., -100.) in
            let etatBalle0 = EtatBalle.initialiser position0 vitesse0 acc0 in
            let nvVies = vies - 1 in
            EtatJeu.initialiser etatBalle0 etatBrique etatRaquette score nvVies
          else if contact_raquette etatBalle etatRaquette then
            let nvEtatBalle = rebond_raquette etatBalle etatRaquette (0.5, 0.5) in
            EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score vies
          else
            (* En cas de rebond contre un mur, on augmente l'accélération de 0.05*)
            let nvEtatBalle = rebond etatBalle (0.5, 0.5) in
            EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score vies
        )
        (FreeFall.run (EtatJeu.balle etatJeu))
        etatRaquetteFlux

end

module Brique = 
  struct

  let creer_espace_briques start_x start_y distance_x distance_y num_cols num_rows =
    let rec aux current_x current_y row col acc =
      if row = num_rows then acc
      else if col = num_cols then
        aux start_x (current_y +. distance_y) (row + 1) 0 acc
      else
        let new_x = current_x +. distance_x in
        aux new_x current_y row (col + 1) ((current_x, current_y) :: acc)
    in
    List.rev (aux start_x start_y 0 0 [])


  let nb_cols = 16
  let nb_lignes = 4
  let width = (Box.supx -. Box.infx) /. float_of_int(nb_cols)
  let height = ((Box.supy -. Box.infy) /. float_of_int(nb_lignes)) /. 2.0
  let start_x = Box.marge +. (width /. 2.0)
  let start_y = Box.marge +. (height /. 2.0)

  let briques_geogebra = creer_espace_briques start_x start_y width height nb_cols nb_lignes
end
(*
module Jeu =
struct 
  
  let rec run (etatJeu:etatJeu) : etatJeu Flux.t = 
    (* On créer un unless ici pour gérer si il y a colision avec une brique, le fcond c'est retirer la brique, faire rebondir la balle et continuer le jeu *)
    let nbVies = EtatJeu.vies etatJeu in
    if nbVies<1 then Flux.vide
    else
      let etatJeuFlux = Bouncing.run etatJeu in
      let score = EtatJeu.score etatJeu in
      let etatBriqueInit = EtatEspaceBrique.initialiser (Box.infx, Box.infy, Box.supx, Box.supy) in 
      (*let etatBriqueInit = EtatEspaceBrique.ajouter_briques etatBriqueInit Brique.briques_geogebra in*)
      let etatRaquetteInit = EtatJeu.raquette etatJeu in
      let balleInit = EtatJeu.balle etatJeu in
      (* Il faut initaliser la brique ici et récupérer de état jeu aussi et faire les flux blabla*)
      let etatJeuBalleSuivante = EtatJeu.initialiser balleInit etatBriqueInit etatRaquetteInit (score+10) (nbVies-1) in
      let machin = (run etatJeuBalleSuivante) in
      Flux.append etatJeuFlux machin
end
*)
module Jeu = struct

  let rec run (etatJeu: etatJeu) : etatJeu Flux.t =
    let nbVies = EtatJeu.vies etatJeu in
    if nbVies < 1 then Flux.vide
    else
      let etatRaquetteFlux = flux_etat_raquette (EtatJeu.raquette etatJeu) in
      let etatBrique = EtatJeu.briques etatJeu in
      let score = EtatJeu.score etatJeu in

      Flux.map2
        (fun etatBalle etatRaquette ->
          if Bouncing.contact_sol etatBalle then
            (* Si on touche le sol, on perd une vie et on relance une balle *)
            let etatBalle0 = EtatJeu.balle etatJeu in
            let nvVies = nbVies - 1 in
            EtatJeu.initialiser etatBalle0 etatBrique etatRaquette score nvVies
          else if Bouncing.contact_raquette etatBalle etatRaquette then
            let nvEtatBalle = Bouncing.rebond_raquette etatBalle etatRaquette (0.5, 0.5) in
            EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies
          else
            (* En cas de rebond contre un mur, on augmente l'accélération de 0.05*)
            let nvEtatBalle = Bouncing.rebond etatBalle (0.5, 0.5) in
            EtatJeu.initialiser nvEtatBalle etatBrique etatRaquette score nbVies
        )
        (FreeFall.run (EtatJeu.balle etatJeu))
        etatRaquetteFlux
end



let () = 
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize true;
  let x,_ = (Graphics.mouse_pos ()) in
  let x_souris = float_of_int x in
  let etatRaquette = getEtatRaquette x_souris in
  let positionBalle0 = (EtatRaquette.position etatRaquette,FormeRaquette.hauteur +. FormeBalle.rayon) in
  let vitesse0 = (600.,300.) in
  let acceleration0 = (0.,-90.81) in
  let etatBalle = EtatBalle.initialiser positionBalle0 vitesse0 acceleration0 in
  let etatBrique = EtatEspaceBrique.initialiser (Box.infx,Box.infy, Box.supx, Box.supy) in 
  let etatJeu = (EtatJeu.initialiser etatBalle etatBrique etatRaquette 0 500) in
  draw (Jeu.run etatJeu)