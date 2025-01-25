
open EtatBalle
open EtatEspaceBrique
open EtatRaquette
(* Définition des types pour les composantes de l'état du jeu *)

(* Définition du type score représentant le score du joueur *)
type score = int

(* Définition du type vies représentant le nombre de vies restantes du joueur *)
and vies = int

(* Définition du type etatJeu représentant l'état global du jeu *)
type etatJeu = etatBalle * etatEspaceBrique * etatRaquette * score * vies

(* Fonction pour initialiser l'état global du jeu
   Paramètres :
   - ba : etatBalle : L'état de la balle.
   - br : etatEspaceBrique : L'état de l'espace de briques.
   - ra : etatRaquette : L'état de la raquette.
   - s : score : Le score du joueur.
   - v : vies : Le nombre de vies restantes du joueur.
   Valeur de retour :
   - etatJeu : L'état global du jeu créé. *)
let initialiser (ba:etatBalle) (br:etatEspaceBrique) (ra:etatRaquette) s v : etatJeu = (ba, br, ra, s, v)

(* Fonction pour obtenir l'état de la balle à partir de l'état global du jeu *)
let balle ((bl, _, _, _, _) : etatJeu) = bl

(* Fonction pour obtenir l'état de l'espace de briques à partir de l'état global du jeu *)
let briques ((_, br, _, _, _) : etatJeu) = br

(* Fonction pour obtenir l'état de la raquette à partir de l'état global du jeu *)
let raquette ((_, _, r, _, _) : etatJeu) = r

(* Fonction pour obtenir le score du joueur à partir de l'état global du jeu *)
let score ((_, _, _, s, _) : etatJeu) = s

(* Fonction pour obtenir le nombre de vies restantes du joueur à partir de l'état global du jeu *)
let vies ((_, _, _, _, v) : etatJeu) = v

(* Fonction pour obtenir l'état de la balle à partir de l'état global du jeu *)
let etat_balle etat = balle etat

(* Fonction pour obtenir la position de la balle à partir de l'état global du jeu *)
let position_balle etat = EtatBalle.position (balle etat)

(* Fonction pour obtenir la vitesse de la balle à partir de l'état global du jeu *)
let vitesse_balle etat = EtatBalle.vitesse (balle etat)

(* Fonction pour obtenir l'accélération de la balle à partir de l'état global du jeu *)
let acceleration_balle etat = acceleration (balle etat)

