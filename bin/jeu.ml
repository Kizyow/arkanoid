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

let rec run (etatJeu: etatJeu) (balleCollee:bool) : etatJeu flux =
  let nbVies = EtatJeu.vies etatJeu in
  if nbVies < 1 then Flux.vide
  else
    let score = EtatJeu.score etatJeu in
    let nbVies = EtatJeu.vies etatJeu in
    let etatBrique = EtatJeu.briques etatJeu in
    let fluxEtatRaquetteBalleCollee = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
      Flux.unless (Flux.map
    (fun raquette ->
        EtatJeu.initialiser (EtatBalle.initialiser ((EtatRaquette.position raquette),FormeRaquette.hauteur) (0.,0.) (0.,0.)) etatBrique raquette score nbVies
    ) fluxEtatRaquetteBalleCollee) (fun jeu -> (not balleCollee || EtatRaquette.clique (EtatJeu.raquette jeu))) 

   (fun etatJeuDecollage ->
    let etatRaquetteFlux = flux_etat_raquette Box.infx Box.supx FormeRaquette.longeur in
    let etatBalleDecollage = if balleCollee then 
      (EtatBalle.initialiser (EtatJeu.position_balle etatJeuDecollage) ParametresBalle.vitesse_initiale ParametresBalle.acceleration_initiale )
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
    (fun etatJeuEvent ->
      let etatBalle = EtatJeu.balle etatJeuEvent in
      let etatRaquette = EtatJeu.raquette etatJeuEvent in
      let (x, y) = EtatBalle.position etatBalle in
      if GestionBalle.contact_sol etatBalle then
        (* Si on touche le sol, on perd une vie et on relance une balle *)
        let positionBalle0 = (EtatRaquette.position etatRaquette,FormeRaquette.hauteur +. ParametresBalle.rayon) in
        let vitesse0 = ParametresBalle.vitesse_initiale in
        let acceleration0 = ParametresBalle.acceleration_initiale in
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
    (fun etatJeuNext balleCollee -> run etatJeuNext balleCollee)))