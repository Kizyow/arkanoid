
open EtatBalle
open EtatEspaceBrique
open EtatRaquette

type score = int
and vies = int

type t = EtatBalle.t * EtatEspaceBrique.t * EtatRaquette.t * score * vies
let initialiser (ba:EtatBalle.t) (br:EtatEspaceBrique.t) (ra:EtatRaquette.t) s v : t = (ba, br, ra, s, v)
let balle ((bl, _, _, _, _) : t) = bl
let briques ((_, br, _, _, _) : t) = br
let raquette ((_, _, r, _, _) : t) = r
let score ((_, _, _, s, _) : t) = s
let vies ((_, _, _, _, v) : t) = v
let etat_balle etat = balle etat
let position_balle etat = EtatBalle.position (balle etat)
let vitesse_balle etat = EtatBalle.vitesse (balle etat)
let acceleration_balle etat = acceleration (balle etat)
