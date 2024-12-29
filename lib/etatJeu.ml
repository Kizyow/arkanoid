open EtatBalle

type score = int
and vies = int

type t = EtatBalle.t * score * vies
let initialiser balle s v = (balle, s, v)
let balle ((balle, _, _) : t) = balle
let score ((_, s, _) : t) = s
let vies ((_, _, v) : t) = v
let etat_balle etat = balle etat
let position_balle etat = position (balle etat)
let vitesse_balle etat = vitesse (balle etat)
let acceleration_balle etat = acceleration (balle etat)
