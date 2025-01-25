
(* Définition des types pour les composantes de l'état de la balle *)

(* Définition du type alias position représentant la position de la balle *)
type position = float * float

(* Définition du type vitesse représentant la vitesse de la balle *)
type vitesse = float * float

(* Définition du type acceleration représentant l'accélération de la balle *)
type acceleration = float * float

(* Définition du type t représentant l'état de la balle *)
type t = position * vitesse * acceleration

(* Fonction pour initialiser l'état de la balle
   Paramètres :
   - pos : position : La position de la balle.
   - vit : vitesse : La vitesse de la balle.
   - acc : acceleration : L'accélération de la balle.
   Valeur de retour :
   - t : L'état de la balle créé. *)
let initialiser pos vit acc : t = (pos, vit, acc)

(* Fonction pour obtenir la position de la balle à partir de son état *)
let position ((pos, _, _) : t) = pos

(* Fonction pour obtenir la vitesse de la balle à partir de son état *)
let vitesse ((_, vit, _) : t) = vit

(* Fonction pour obtenir l'accélération de la balle à partir de son état *)
let acceleration ((_, _, acc) : t) = acc


