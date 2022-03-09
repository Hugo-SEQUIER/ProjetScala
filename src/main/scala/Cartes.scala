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
  

