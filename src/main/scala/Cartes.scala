object Cartes:

  enum Valeur :
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
    case AS

  enum Couleur :
    case PIQUE
    case TREFLE
    case CARREAU
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
    case "AS ♦" => Cartes.P(Valeur.AS,Couleur.CARREAU)
    case "2 ♦" => Cartes.P(Valeur.DEUX,Couleur.CARREAU)
    case "3 ♦" => Cartes.P(Valeur.TROIS,Couleur.CARREAU)
    case "4 ♦" => Cartes.P(Valeur.QUATRE,Couleur.CARREAU)
    case "5 ♦" => Cartes.P(Valeur.CINQ,Couleur.CARREAU)
    case "6 ♦" => Cartes.P(Valeur.SIX,Couleur.CARREAU)
    case "7 ♦" => Cartes.P(Valeur.SEPT,Couleur.CARREAU)
    case "8 ♦" => Cartes.P(Valeur.HUIT,Couleur.CARREAU)
    case "9 ♦" => Cartes.P(Valeur.NEUF,Couleur.CARREAU)
    case "10 ♦" => Cartes.P(Valeur.DIX,Couleur.CARREAU)
    case "VALET ♦" => Cartes.P(Valeur.VALET,Couleur.CARREAU)
    case "DAME ♦" => Cartes.P(Valeur.DAME,Couleur.CARREAU)
    case "ROI ♦" => Cartes.P(Valeur.ROI,Couleur.CARREAU)

  enum Hand:
    case Hauteur
    case Paire
    case DoublePaire
    case Brelan
    case Quinte
    case Couleur
    case Full
    case Carre
    case QuinteFlush
    case QuinteFlushRoyale

  class PlayingCardValue extends Ordering[Cartes]{
    override def compare(x: Cartes, y: Cartes): Int = (x,y) match
      case (Cartes.P(v,c),Cartes.P(vs,cs)) => if v.ordinal > vs.ordinal then 1 else if v.ordinal == vs.ordinal then 0 else -1
  }

  class PlayingHandRanking extends Ordering[Hand]{
    override def compare(x:Hand, y:Hand): Int = (x,y) match
      case (a:Hand,b:Hand) => if a.ordinal > b.ordinal then 1 else if a.ordinal == b.ordinal then 0 else -1
  }
