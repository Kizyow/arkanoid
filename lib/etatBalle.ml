
(* Définition des types pour les composantes de l'état de la balle *)

(* Définition du type position représentant la position de la balle *)
type position = float * float

(* Définition du type vitesse représentant la vitesse de la balle *)
type vitesse = float * float

(* Définition du type acceleration représentant l'accélération de la balle *)
type acceleration = float * float

(* Définition du type etatBalle représentant l'état de la balle *)
type etatBalle = position * vitesse * acceleration

(* Fonction pour initialiser l'état de la balle
   Paramètres :
   - pos : position : La position de la balle.
   - vit : vitesse : La vitesse de la balle.
   - acc : acceleration : L'accélération de la balle.
   Valeur de retour :
   - etatBalle : L'état de la balle créé. *)
let initialiser pos vit acc : etatBalle = (pos, vit, acc)

(* Fonction pour obtenir la position de la balle à partir de son état *)
let position ((pos, _, _) : etatBalle) = pos

(* Fonction pour obtenir la vitesse de la balle à partir de son état *)
let vitesse ((_, vit, _) : etatBalle) = vit

(* Fonction pour obtenir l'accélération de la balle à partir de son état *)
let acceleration ((_, _, acc) : etatBalle) = acc


