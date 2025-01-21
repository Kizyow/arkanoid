open EtatBalle
open EtatEspaceBrique

type score = int
and vies = int

type t = EtatBalle.t * EtatEspaceBrique.t * score * vies
let initialiser balle briques s v = (balle, briques, s, v)
let balle ((bl, _, _, _) : t) = bl
let briques ((_, br, _, _) : t) = br
let score ((_, _, s, _) : t) = s
let vies ((_, _, _, v) : t) = v
let etat_balle etat = balle etat
let position_balle etat = position (balle etat)
let vitesse_balle etat = vitesse (balle etat)
let acceleration_balle etat = acceleration (balle etat)
