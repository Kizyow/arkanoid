
module Init = struct
  let dt = ((1. /. 60.)) (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module FormeRaquette = struct
  let hauteur = 15.
  let longeur = 300.
end

module ParametresBalle = struct
  let rayon = 5.
  let vitesse_initiale = (600.,300.)
  let acceleration_initiale = (0.,-90.81)
end

module ParametreBrique = struct
  let nbColonnes = 16
  let nbLignes = 4
  let espace_entre_briques = false
  let voir_bordures_quadtree = true
end