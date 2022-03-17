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

  def rePair(l: List[Cartes]): List[Cartes] = l match
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==2 then deleteAllExcept(l,v) else rePair(xs)

  def isTwoPair(l: List[Cartes], nbPair: Int) : Boolean =
    if (nbPair == 2) then true else l match
      case Nil => false
      case x::Nil => false
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v) ==1 then isTwoPair(xs,nbPair+1) else isTwoPair(xs,nbPair)

  def calculPair(l:List[Cartes]) : List[Cartes] =
    if l.length > 4 then l match
      case Cartes.P(v,c)::xs => deleteOccurrence(l,v)
    else l

  def re2TwoPair(l:List[Cartes]) : List[Cartes] = l match
    case Cartes.P(va,cb)::Cartes.P(vb,c)::Nil => if va.ordinal == vb.ordinal then Cartes.P(va,cb)::Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,cb)::xs => if nbOccurence(xs,va) == 1 then deleteAllExcept(l,va):::re2TwoPair(deleteOccurrence(xs,va)) else re2TwoPair(xs)

  def reTwoPair(l:List[Cartes]) : List[Cartes] = calculPair(re2TwoPair(l))

  def isBrelan(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 2 then true else isBrelan(xs)

  def reBrelan(l: List[Cartes]) : List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==2 then deleteAllExcept(l,v) else reBrelan(xs)

  def isQuinte(l: List[Cartes], v: Int, nbSui : Int) : Boolean = (l,v) match
    case (Cartes.P(va,c)::xs,-1) => isQuinte(xs,va.ordinal,0)
    case (_,va) =>
      if va != -1 then l match
        case Nil => false
        case Cartes.P(va,c)::Nil =>  (va.ordinal - v == 1 && nbSui >= 4)
        case Cartes.P(va,c)::xs => if va.ordinal - v == 1 then isQuinte(xs,va.ordinal,nbSui+1) else isQuinte(xs,va.ordinal,nbSui)
      else false

  def reQuinte(l : List[Cartes]): List[Cartes] = l match
    case Cartes.P(va,_)::Cartes.P(vb,c)::Nil => if vb.ordinal - va.ordinal == 1 then Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,c)::Cartes.P(vb,_)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinte(xs) else reQuinte(xs)


  def isFlush(l: List[Cartes], nbCoeur: Int, nbTrefle: Int, nbCarreau: Int, nbPique: Int) : Boolean =
    if (nbCoeur >= 5 || nbTrefle >= 5 || nbCarreau >= 5 || nbPique >= 5) then true else l match
      case Nil => false
      case Cartes.P(va,c)::Nil => if c == Couleur.COEUR then isFlush(Nil,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
        if c == Couleur.TREFLE then isFlush(Nil,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
          if c == Couleur.CARREAU then isFlush(Nil,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(Nil,nbCoeur,nbTrefle,nbCarreau,nbPique+1)
      case Cartes.P(va,c)::xs => if c == Couleur.COEUR then isFlush(xs,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
        if c == Couleur.TREFLE then isFlush(xs,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
          if c == Couleur.CARREAU then isFlush(xs,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(xs,nbCoeur,nbTrefle,nbCarreau,nbPique+1)

  def reCoulFlush(l: List[Cartes], nbCoeur: Int, nbTrefle: Int, nbCarreau: Int, nbPique: Int) : Couleur =
    if isFlush(l,nbCoeur,nbTrefle,nbCarreau,nbPique) then
      if nbCoeur >= 5 then Couleur.COEUR else
        if nbTrefle >= 5 then Couleur.TREFLE else
          if nbCarreau >= 5 then Couleur.CARREAU else
            if nbPique >= 5 then Couleur.PIQUE else null
    else null

  def reFlush(l:List[Cartes],coul: Couleur) : List[Cartes] =  l match
    case Cartes.P(v,c)::Nil => if c == coul then Cartes.P(v,c)::Nil else Nil
    case Cartes.P(v,c)::xs => if c == coul then Cartes.P(v,c)::reFlush(xs,coul) else reFlush(xs,coul)

  def isFull(l: List[Cartes]): Boolean =
    if isBrelan(l) then isPair(deleteOccurrence(l,reValueCard(reBrelan(l)))) else false

  /** Prends une liste de 2 paires */
  def triPair(l: List[Cartes]): List[Cartes] = l match
    case Cartes.P(v,c)::xs => deleteOccurrence(l,v)

  def reFull(l:List[Cartes], bre:List[Cartes],n :Int): List[Cartes] =
    if n < 0 then bre match
      case Nil => reFull(l,reBrelan(l),n)
      case Cartes.P(v,c)::_ => reFull(deleteOccurrence(l,v),bre,1)
    else if isTwoPair(l,0) then bre:::triPair(l) else bre:::rePair(l)

  def isCarre(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 3 then true else isCarre(xs)

  def reCarre(l: List[Cartes]): List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==3 then deleteAllExcept(l,v) else reCarre(xs)

  def isQuinteFlush(l: List[Cartes]) : Boolean =
    if isQuinte(l,-1,0) then isFlush(reQuinte(l),0,0,0,0)
    else false

  /** On rentre en paramètre une liste qui contiendra après le premier appel seulement des cartes de la Flush */
  def reQuinteFlush(l:List[Cartes], n:Int): List[Cartes] =
    if n < 0 then reQuinteFlush(reFlush(l,reCoulFlush(l,0,0,0,0)),1) else l match
      case Cartes.P(va,_)::Cartes.P(vb,c)::Nil => if vb.ordinal - va.ordinal == 1 then Cartes.P(vb,c)::Nil else Nil
      case Cartes.P(va,c)::Cartes.P(vb,_)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinteFlush(xs,1) else reQuinteFlush(xs,1)

  def isQuinteFlushRoyale(l: List[Cartes], col : Couleur, start : Int, nbSuit : Int) : Boolean =
    if start <0 then isQuinteFlushRoyale(l,reCoulFlush(l,0,0,0,0),1,0) else l match
      case Cartes.P(va,c)::Nil =>  va == Valeur.AS && c == col && nbSuit>=4
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then isQuinteFlushRoyale(xs,col,1,nbSuit+1) else isQuinteFlushRoyale(xs,col,1,nbSuit)

  def reQuinteFlushRoyale(l: List[Cartes], n:Int, nbSuit: Int) : List[Cartes] =
    if n<0 then reQuinteFlushRoyale(reFlush(l,reCoulFlush(l,0,0,0,0)),1,0) else l match
      case Cartes.P(va,c)::Nil => if va == Valeur.AS then Cartes.P(va,c)::Nil else return Nil
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then Cartes.P(va,c)::reQuinteFlushRoyale(xs,1,nbSuit+1) else reQuinteFlushRoyale(xs,1,nbSuit)

  def bestCard(l:List[Cartes]) : List[Cartes] = l match
    case Cartes.P(va,c)::Nil => Cartes.P(va,c)::Nil
    case x::xs => bestCard(xs)

  def bestHand(l:List[Cartes]) : List[Cartes] =
    if isQuinteFlush(l) then
      if isQuinteFlushRoyale(l,Couleur.CARREAU,-1,0) then reQuinteFlushRoyale(l,-1,0) else reQuinteFlush(l,-1) else
        if isCarre(l) then reCarre(l) else
          if isFull(l) then reFull(l,Nil,-1) else
            if isFlush(l,0,0,0,0) then reFlush(l,reCoulFlush(l,0,0,0,0)) else
              if isQuinte(l,-1,0) then reQuinte(l) else
                if isBrelan(l) then reBrelan(l) else
                  if isTwoPair(l,0) then reTwoPair(l) else bestCard(l)
