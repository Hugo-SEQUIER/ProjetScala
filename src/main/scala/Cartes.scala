object Cartes :

enum Valeur :
  case AS
  case DEUX
  case TROIS
  case QUATRE
  case CINQ
  case SIX
  case SEPT
  case HUIT
  case NEUF
  case DIX
  case VALET
  case DAME
  case ROI

enum Couleur :
  case PIQUE
  case TREFLE
  case CARREAUX
  case COEUR

enum Cartes:
  case P(v: Valeur, c: Couleur)

def valueOf(s: String): Cartes = s match
  case "AS ♥" => Cartes.P(Valeur.AS,Couleur.COEUR)
  case "2 ♥" => Cartes.P(Valeur.DEUX,Couleur.COEUR)
  case "3 ♥" => Cartes.P(Valeur.TROIS,Couleur.COEUR)
  case "4 ♥" => Cartes.P(Valeur.QUATRE,Couleur.COEUR)
  case "5 ♥" => Cartes.P(Valeur.CINQ,Couleur.COEUR)
  case "6 ♥" => Cartes.P(Valeur.SIX,Couleur.COEUR)
  case "7 ♥" => Cartes.P(Valeur.SEPT,Couleur.COEUR)
  case "8 ♥" => Cartes.P(Valeur.HUIT,Couleur.COEUR)
  case "9 ♥" => Cartes.P(Valeur.NEUF,Couleur.COEUR)
  case "10 ♥" => Cartes.P(Valeur.DIX,Couleur.COEUR)
  case "VALET ♥" => Cartes.P(Valeur.VALET,Couleur.COEUR)
  case "DAME ♥" => Cartes.P(Valeur.DAME,Couleur.COEUR)
  case "ROI ♥" => Cartes.P(Valeur.ROI,Couleur.COEUR)
  case "AS ☘" => Cartes.P(Valeur.AS,Couleur.TREFLE)
  case "2 ☘" => Cartes.P(Valeur.DEUX,Couleur.TREFLE)
  case "3 ☘" => Cartes.P(Valeur.TROIS,Couleur.TREFLE)
  case "4 ☘" => Cartes.P(Valeur.QUATRE,Couleur.TREFLE)
  case "5 ☘" => Cartes.P(Valeur.CINQ,Couleur.TREFLE)
  case "6 ☘" => Cartes.P(Valeur.SIX,Couleur.TREFLE)
  case "7 ☘" => Cartes.P(Valeur.SEPT,Couleur.TREFLE)
  case "8 ☘" => Cartes.P(Valeur.HUIT,Couleur.TREFLE)
  case "9 ☘" => Cartes.P(Valeur.NEUF,Couleur.TREFLE)
  case "10 ☘" => Cartes.P(Valeur.DIX,Couleur.TREFLE)
  case "VALET ☘" => Cartes.P(Valeur.VALET,Couleur.TREFLE)
  case "DAME ☘" => Cartes.P(Valeur.DAME,Couleur.TREFLE)
  case "ROI ☘" => Cartes.P(Valeur.ROI,Couleur.TREFLE)
  case "AS ♠" => Cartes.P(Valeur.AS,Couleur.PIQUE)
  case "2 ♠" => Cartes.P(Valeur.DEUX,Couleur.PIQUE)
  case "3 ♠" => Cartes.P(Valeur.TROIS,Couleur.PIQUE)
  case "4 ♠" => Cartes.P(Valeur.QUATRE,Couleur.PIQUE)
  case "5 ♠" => Cartes.P(Valeur.CINQ,Couleur.PIQUE)
  case "6 ♠" => Cartes.P(Valeur.SIX,Couleur.PIQUE)
  case "7 ♠" => Cartes.P(Valeur.SEPT,Couleur.PIQUE)
  case "8 ♠" => Cartes.P(Valeur.HUIT,Couleur.PIQUE)
  case "9 ♠" => Cartes.P(Valeur.NEUF,Couleur.PIQUE)
  case "10 ♠" => Cartes.P(Valeur.DIX,Couleur.PIQUE)
  case "VALET ♠" => Cartes.P(Valeur.VALET,Couleur.PIQUE)
  case "DAME ♠" => Cartes.P(Valeur.DAME,Couleur.PIQUE)
  case "ROI ♠" => Cartes.P(Valeur.ROI,Couleur.PIQUE)
  case "AS ♦" => Cartes.P(Valeur.AS,Couleur.CARREAUX)
  case "2 ♦" => Cartes.P(Valeur.DEUX,Couleur.CARREAUX)
  case "3 ♦" => Cartes.P(Valeur.TROIS,Couleur.CARREAUX)
  case "4 ♦" => Cartes.P(Valeur.QUATRE,Couleur.CARREAUX)
  case "5 ♦" => Cartes.P(Valeur.CINQ,Couleur.CARREAUX)
  case "6 ♦" => Cartes.P(Valeur.SIX,Couleur.CARREAUX)
  case "7 ♦" => Cartes.P(Valeur.SEPT,Couleur.CARREAUX)
  case "8 ♦" => Cartes.P(Valeur.HUIT,Couleur.CARREAUX)
  case "9 ♦" => Cartes.P(Valeur.NEUF,Couleur.CARREAUX)
  case "10 ♦" => Cartes.P(Valeur.DIX,Couleur.CARREAUX)
  case "VALET ♦" => Cartes.P(Valeur.VALET,Couleur.CARREAUX)
  case "DAME ♦" => Cartes.P(Valeur.DAME,Couleur.CARREAUX)
  case "ROI ♦" => Cartes.P(Valeur.ROI,Couleur.CARREAUX)

class PlayingCardValue extends Ordering[Cartes]{
  override def compare(x: Cartes, y: Cartes): Int = (x,y) match
    case (Cartes.P(v,c),Cartes.P(vs,cs)) => v > vs
}