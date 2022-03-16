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

  def nbOccurence(l: List[Cartes],v : Valeur) : Int = l match
    case Nil => 0
    case Cartes.P(va,c)::Nil => if va == v then 1 else 0
    case Cartes.P(va,c)::xs => if va == v then 1+nbOccurence(xs,v) else nbOccurence(xs,v)

  def deleteOccurrence(l: List[Cartes], v : Valeur) : List[Cartes] =
    if (nbOccurence(l,v) == 0) then l else l match
      case x::Nil => Nil
      case Cartes.P(va,c)::xs => if va == v then xs else Cartes.P(va,c)::deleteOccurrence(xs,v)

  def deleteAllExcept(l:List[Cartes], v:Valeur) : List[Cartes] =
    if (nbOccurence(l,v) == 0) then Nil else l match
      case x::Nil => x::Nil
      case Cartes.P(va,c)::xs => if va == v then Cartes.P(va,c)::deleteAllExcept(xs,v) else deleteAllExcept(xs,v)

  /** Je ne fais pas de cas Nil car il ne peut pas avoir de list vide rentré en paramètre */
  def reValueCard(l:List[Cartes]): Valeur = l match
    case Cartes.P(va,c)::xs => va

  def isPair(l: List[Cartes]): Boolean = l match
    case Nil => false
    case x::Nil => false
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==2 then true else isPair(xs)

  def rePair(l: List[Cartes]): List[Cartes] =
    if isPair(l) then l match
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==2 then deleteAllExcept(l,v) else rePair(xs)
    else Nil

  def isTwoPair(l: List[Cartes], pair : Int, nbPair: Int) : Boolean =
      if (nbPair == 2) then true else l match
        case Nil => false
        case x::Nil => false
        case Cartes.P(v,c)::xs => if nbOccurence(xs,v) ==2 then if v.ordinal != pair then isTwoPair(xs,v.ordinal,nbPair+1) else isTwoPair(xs,v.ordinal,nbPair) else false

  def isBrelan(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 3 then true else isBrelan(xs)

  def reBrelan(l: List[Cartes]) : List[Cartes] =
    if isBrelan(l) then l match
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==3 then deleteAllExcept(l,v) else reBrelan(xs)
    else Nil

  def isQuinte(l: List[Cartes], v: Int, nbSui : Int) : Boolean = (l,v) match
    case (Cartes.P(va,c)::xs,-1) => isQuinte(xs,va.ordinal,0)
    case (_,va) => if va != -1 then l match
      case Nil => false
      case Cartes.P(va,c)::Nil =>  (va.ordinal - v == 1 && nbSui >= 4)
      case Cartes.P(va,c)::xs => if va.ordinal - v == 1 then isQuinte(xs,va.ordinal,nbSui+1) else isQuinte(xs,va.ordinal,nbSui)
      else false

  def reQuinte(l : List[Cartes]): List[Cartes] =
    if isQuinte(l,-1,0) then l match
      case Cartes.P(va,_)::Cartes.P(vb,c)::Nil => if vb.ordinal - va.ordinal == 1 then Cartes.P(vb,c) else Nil
      case Cartes.P(va,c)::Cartes.P(vb,_)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinte(xs) else reQuinte(xs)
    else Nil

  def isFlush(l: List[Cartes], nbCoeur: Int, nbTrefle: Int, nbCarreau: Int, nbPique: Int) : Boolean =
    if (nbCoeur >= 5 || nbTrefle >= 5 || nbCarreau >= 5 || nbPique >= 5) then true else l match
      case Nil => false
      case Cartes.P(va,c)::Nil => if c == Couleur.COEUR then isFlush(Nil,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else if c == Couleur.TREFLE then isFlush(Nil,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else if c == Couleur.CARREAU then isFlush(Nil,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(Nil,nbCoeur,nbTrefle,nbCarreau,nbPique+1)
      case Cartes.P(va,c)::xs => if c == Couleur.COEUR then isFlush(xs,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else if c == Couleur.TREFLE then isFlush(xs,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else if c == Couleur.CARREAU then isFlush(xs,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(xs,nbCoeur,nbTrefle,nbCarreau,nbPique+1)

  def isFull(l: List[Cartes]): Boolean =
    if isBrelan(l) then isPair(deleteOccurrence(l,reValueCard(reBrelan(l)))) else false

  def isCarre(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 4 then true else isCarre(xs)

  def isQuinteFlush(l: List[Cartes]) : Boolean =
    if isQuinte(l,-1,0) then isFlush(reQuinte(l),0,0,0,0)
    else false

