
module Init = struct
  let dt = ((1. /. 120.)) (* 120 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module FormeRaquette = struct
  let hauteur = 25.
  let longeur = 200.
  (*Désigne la proportion maximale de la direction qu'une balle peut rebondir selon l'axe des x.*)
  let max_edge_shot = 0.75
end

module ParametresBalle = struct
  let rayon = 5.
  let vitesse_initiale = (0.,500.)
  let acceleration_initiale = (0.,-98.1)
  let gain_vitesse_touche_brique = 1.01
  let gain_vitesse_touche_raquette = 1.003
  let gain_vitesse_touche_mur = 1.003
end

module ParametreBrique = struct
  let nbColonnes = 16
  let nbLignes = 4
  let espace_entre_briques = false
  let voir_bordures_quadtree = true
end