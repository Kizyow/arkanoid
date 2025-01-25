
open EtatBalle
open EtatEspaceBrique
open EtatRaquette
(* Définition des types pour les composantes de l'état du jeu *)

type score = int
and vies = int

type etatJeu
val initialiser : etatBalle -> etatEspaceBrique -> etatRaquette -> score -> vies -> etatJeu
val balle : etatJeu -> etatBalle
val briques : etatJeu -> etatEspaceBrique
val raquette : etatJeu -> etatRaquette
val score : etatJeu -> score
val vies : etatJeu -> vies
val position_balle : etatJeu -> position
val vitesse_balle : etatJeu -> vitesse
val acceleration_balle : etatJeu -> acceleration