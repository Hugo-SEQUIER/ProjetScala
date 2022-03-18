import Cartes.Cartes

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

 //entrée : rien
 //sortie : liste contenant toutes les cartes du jeu (52 cartes)
  def touteLesCartes() : List[Cartes] = List(
    Cartes.P(Valeur.AS,Couleur.COEUR),
    Cartes.P(Valeur.DEUX,Couleur.COEUR),
    Cartes.P(Valeur.TROIS,Couleur.COEUR),
    Cartes.P(Valeur.QUATRE,Couleur.COEUR),
    Cartes.P(Valeur.CINQ,Couleur.COEUR),
    Cartes.P(Valeur.SIX,Couleur.COEUR),
    Cartes.P(Valeur.SEPT,Couleur.COEUR),
    Cartes.P(Valeur.HUIT,Couleur.COEUR),
    Cartes.P(Valeur.NEUF,Couleur.COEUR),
    Cartes.P(Valeur.DIX,Couleur.COEUR),
    Cartes.P(Valeur.VALET,Couleur.COEUR),
    Cartes.P(Valeur.DAME,Couleur.COEUR),
    Cartes.P(Valeur.ROI,Couleur.COEUR),
    Cartes.P(Valeur.AS,Couleur.TREFLE),
    Cartes.P(Valeur.DEUX,Couleur.TREFLE),
    Cartes.P(Valeur.TROIS,Couleur.TREFLE),
    Cartes.P(Valeur.QUATRE,Couleur.TREFLE),
    Cartes.P(Valeur.CINQ,Couleur.TREFLE),
    Cartes.P(Valeur.SIX,Couleur.TREFLE),
    Cartes.P(Valeur.SEPT,Couleur.TREFLE),
    Cartes.P(Valeur.HUIT,Couleur.TREFLE),
    Cartes.P(Valeur.NEUF,Couleur.TREFLE),
    Cartes.P(Valeur.DIX,Couleur.TREFLE),
    Cartes.P(Valeur.VALET,Couleur.TREFLE),
    Cartes.P(Valeur.DAME,Couleur.TREFLE),
    Cartes.P(Valeur.ROI,Couleur.TREFLE),
    Cartes.P(Valeur.AS,Couleur.PIQUE),
    Cartes.P(Valeur.DEUX,Couleur.PIQUE),
    Cartes.P(Valeur.TROIS,Couleur.PIQUE),
    Cartes.P(Valeur.QUATRE,Couleur.PIQUE),
    Cartes.P(Valeur.CINQ,Couleur.PIQUE),
    Cartes.P(Valeur.SIX,Couleur.PIQUE),
    Cartes.P(Valeur.SEPT,Couleur.PIQUE),
    Cartes.P(Valeur.HUIT,Couleur.PIQUE),
    Cartes.P(Valeur.NEUF,Couleur.PIQUE),
    Cartes.P(Valeur.DIX,Couleur.PIQUE),
    Cartes.P(Valeur.VALET,Couleur.PIQUE),
    Cartes.P(Valeur.DAME,Couleur.PIQUE),
    Cartes.P(Valeur.ROI,Couleur.PIQUE),
    Cartes.P(Valeur.AS,Couleur.CARREAU),
    Cartes.P(Valeur.DEUX,Couleur.CARREAU),
    Cartes.P(Valeur.TROIS,Couleur.CARREAU),
    Cartes.P(Valeur.QUATRE,Couleur.CARREAU),
    Cartes.P(Valeur.CINQ,Couleur.CARREAU),
    Cartes.P(Valeur.SIX,Couleur.CARREAU),
    Cartes.P(Valeur.SEPT,Couleur.CARREAU),
    Cartes.P(Valeur.HUIT,Couleur.CARREAU),
    Cartes.P(Valeur.NEUF,Couleur.CARREAU),
    Cartes.P(Valeur.DIX,Couleur.CARREAU),
    Cartes.P(Valeur.VALET,Couleur.CARREAU),
    Cartes.P(Valeur.DAME,Couleur.CARREAU),
    Cartes.P(Valeur.ROI,Couleur.CARREAU)
  )

  //entrée : une liste de carte et une carte
  //sortie : la liste de carte sans la carte c1
  def retirerUneCarte(l1:List[Cartes],c1:Cartes) : List[Cartes]= (l1,c1) match
     case (Nil,_) => l1
     case (x::y,_) => if(x==c1) then y else x::retirerUneCarte(y,c1)

  //entrée : une liste de carte et le jeu complet
  //sortie : une liste contenant toute les cartes moins les cartes qui sont dans l1
  def retirerDesCartes(l1:List[Cartes]) : List[Cartes] = l1 match
      case Nil => touteLesCartes()
      case x::y => retirerUneCarte(retirerDesCartes(y),x)

  //entrée : une liste de cartes de taille 6 et la liste de toutes les cartes moins les 6 cartes
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour6(l1:List[Cartes],l2:List[Cartes]) : List[List[Cartes]] = l2 match
      case Nil => Nil
      case c::d => List(l1:::c::Nil):::toutesPossibilitesPour6(l1,d)

  //entrée : une liste de cartes de taille 6
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour6V2(l1:List[Cartes]) : List[List[Cartes]] = toutesPossibilitesPour6(l1,retirerDesCartes(l1))

  //entrée : une liste de carte de taille 5, une liste de carte contenant toutes les cartes du jeu sans les cartes
  //de l1 et une liste de cartes contenant toutes les cartes du jeu moins celles de l1 et moins la première
  def toutesPossibilitesPour5(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes]) : List[List[Cartes]] = (l2,l3) match
    case (a::Nil,Nil) => Nil
    case (a::b,Nil) => toutesPossibilitesPour5(l1,b,suivant(b))
    case (a::b,c::d) => List(l1:::a::Nil:::c::Nil):::toutesPossibilitesPour5(l1,l2,d)

  //entrée : une liste de cartes de taille 5
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour5V2(l1:List[Cartes]) : List[List[Cartes]] = toutesPossibilitesPour5(l1,retirerDesCartes(l1),retirerDesCartesEtLaPremiere(l1))

  //entrée : une liste de cartes
  //sortie : une liste contenant toutes les cartes du jeu moins celles de l1 et sans le premier élément
  def retirerDesCartesEtLaPremiere(l1:List[Cartes]) : List[Cartes] = l1 match
    case (x::y) => retirerDesCartes(y)

  //entrée : une liste de cartes
  //sortie : la même liste sans la première carte
  def suivant(l1:List[Cartes]) : List[Cartes] = l1 match
    case (x::y) => y

    def nbOccurence(l: List[Cartes],v : Valeur) : Int = l match
    case Nil => 0
    case Cartes.P(va,c)::Nil => if va == v then 1 else 0
    case Cartes.P(va,c)::xs => if va == v then 1+nbOccurence(xs,v) else nbOccurence(xs,v)

  def deleteOccurrence(l: List[Cartes], v : Valeur) : List[Cartes] =
    if (nbOccurence(l,v) == 0) then l else l match
      case Cartes.P(va,c)::Nil => if va == v then Nil else Cartes.P(va,c)::Nil
      case Cartes.P(va,c)::xs => if va == v then deleteOccurrence(xs,v) else Cartes.P(va,c)::deleteOccurrence(xs,v)

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
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==1 then true else isPair(xs)

  def rePair(l: List[Cartes]): List[Cartes] = l match
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==1 then deleteAllExcept(l,v) else rePair(xs)

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
    case Nil => Nil
    case Cartes.P(va,cb)::Cartes.P(vb,c)::Nil => if va.ordinal == vb.ordinal then Cartes.P(va,cb)::Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,cb)::xs => if nbOccurence(xs,va) == 1 then deleteAllExcept(l,va):::re2TwoPair(deleteOccurrence(xs,va)) else re2TwoPair(xs)

  def reTwoPair(l:List[Cartes]) : List[Cartes] = calculPair(re2TwoPair(l))

  def isBrelan(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 2 then true else isBrelan(xs)

  def reBrelan(l: List[Cartes]) : List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==2 then deleteAllExcept(l,v) else reBrelan(xs)

  def isQuinte(l: List[Cartes], nbSui : Int) : Boolean = l match
    case Nil => nbSui >=4
    case Cartes.P(va,c)::Cartes.P(vb,ca)::Nil => if vb.ordinal - va.ordinal == 1 then isQuinte(Nil,nbSui+1) else isQuinte(Nil,nbSui)
    case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then isQuinte(Cartes.P(vb,ca)::xs,nbSui+1) else isQuinte(Cartes.P(vb,ca)::xs,nbSui)

  def reQuinteBis(l : List[Cartes],va :Int ): List[Cartes] = l match
    case Cartes.P(vb,c)::Nil => if vb.ordinal - va == 1 then Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinteBis(Cartes.P(vb,ca)::xs,va.ordinal) else reQuinteBis(Cartes.P(vb,ca)::xs,va.ordinal)

  def reQuinte(l: List[Cartes]) : List[Cartes] = reQuinteBis(l,0)

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
    if nbCoeur >= 5 then Couleur.COEUR else
      if nbTrefle >= 5 then Couleur.TREFLE else
        if nbCarreau >= 5 then Couleur.CARREAU else
          if nbPique >= 5 then Couleur.PIQUE else l match
            case Nil => null
            case Cartes.P(va,c)::Nil => if c == Couleur.COEUR then reCoulFlush(Nil,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
              if c == Couleur.TREFLE then reCoulFlush(Nil,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
                if c == Couleur.CARREAU then reCoulFlush(Nil,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else reCoulFlush(Nil,nbCoeur,nbTrefle,nbCarreau,nbPique+1)
            case Cartes.P(va,c)::xs => if c == Couleur.COEUR then reCoulFlush(xs,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
              if c == Couleur.TREFLE then reCoulFlush(xs,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
                if c == Couleur.CARREAU then reCoulFlush(xs,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else reCoulFlush(xs,nbCoeur,nbTrefle,nbCarreau,nbPique+1)


  def reFlushBis(l:List[Cartes],coul: Couleur) : List[Cartes] =  l match
    case Cartes.P(v,c)::Nil => if c == coul then Cartes.P(v,c)::Nil else Nil
    case Cartes.P(v,c)::xs => if c == coul then Cartes.P(v,c)::reFlushBis(xs,coul) else reFlushBis(xs,coul)

  def reFlush(l:List[Cartes]): List[Cartes] = triQuinte(reFlushBis(l,reCoulFlush(l,0,0,0,0)))

  def isFull(l: List[Cartes]): Boolean = l match
      case Nil => false
      case x::Nil => false
      case Cartes.P(v,c)::xs => if nbOccurence(xs,v) == 2  then isPair(deleteOccurrence(l,v)) else
        if nbOccurence(xs,v) == 1 then isBrelan(deleteOccurrence(l,v)) else isFull(xs)

  /** Prends une liste de 2 paires */
  def triPair(l: List[Cartes]): List[Cartes] = l.drop(2)


  def reFull(l:List[Cartes], bre:List[Cartes], pair:List[Cartes], bretrouver: Int): List[Cartes] = l match
    case Nil => bre:::pair
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v) == 2 && bretrouver != 1 then reFull(deleteOccurrence(l,v),reBrelan(l),pair,1) else
      if nbOccurence(xs,v) >= 1 then reFull(deleteOccurrence(l,v),bre,rePair(l),bretrouver) else reFull(xs,bre,pair,bretrouver)

  def isCarre(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurence(xs,va) == 3 then true else isCarre(xs)

  def reCarre(l: List[Cartes]): List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurence(xs,v)==3 then deleteAllExcept(l,v) else reCarre(xs)

  def isQuinteFlush(l: List[Cartes]) : Boolean =
    if isFlush(l,0,0,0,0) then isQuinte(reFlush(l),0)
    else false

  def triQuinte(l: List[Cartes]) : List[Cartes] = l.drop(l.length-5)

  /** On rentre en paramètre une liste qui contiendra après le premier appel seulement des cartes de la Flush */
  def reQuinteFlushBis(l:List[Cartes], n:Int, va:Int): List[Cartes] =
    if n < 0 then reQuinteFlushBis(reFlush(l),1,0) else l match
      case Cartes.P(vb,c)::Nil => if vb.ordinal - va == 1 then Cartes.P(vb,c)::Nil else Nil
      case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinteFlushBis(Cartes.P(vb,ca)::xs,1,va.ordinal) else reQuinteFlushBis(Cartes.P(vb,ca)::xs,1,va.ordinal)

  def reQuinteFlush(l:List[Cartes]) : List[Cartes] = triQuinte(reQuinteFlushBis(l,-1,0))

  def isQuinteFlushRoyale(l: List[Cartes], col : Couleur, start : Int, nbSuit : Int) : Boolean =
    if start <0 then isQuinteFlushRoyale(l,reCoulFlush(l,0,0,0,0),1,0) else l match
      case Cartes.P(va,c)::Nil =>  va == Valeur.AS && c == col && nbSuit>=4
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then isQuinteFlushRoyale(xs,col,1,nbSuit+1) else isQuinteFlushRoyale(xs,col,1,nbSuit)

  def reQuinteFlushRoyale(l: List[Cartes], n:Int, nbSuit: Int) : List[Cartes] =
    if n<0 then reQuinteFlushRoyale(reFlush(l),1,0) else l match
      case Cartes.P(va,c)::Nil => if va == Valeur.AS then Cartes.P(va,c)::Nil else Nil
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then Cartes.P(va,c)::reQuinteFlushRoyale(xs,1,nbSuit+1) else reQuinteFlushRoyale(xs,1,nbSuit)

  def bestCard(l:List[Cartes]) : List[Cartes] = l.takeRight(1)

  def bestHand(l:List[Cartes]) : List[Cartes] =
    if isQuinteFlush(l) then
      if isQuinteFlushRoyale(l,Couleur.CARREAU,-1,0) then reQuinteFlushRoyale(l,-1,0) else reQuinteFlush(l)
    else
        if isCarre(l) then reCarre(l)
        else
          if isFull(l) then reFull(l,Nil,Nil,-1)
          else
            if isFlush(l,0,0,0,0) then reFlush(l)
            else
              if isQuinte(l,0) then reQuinte(l)
              else
                if isBrelan(l) then reBrelan(l)
                else
                  if isTwoPair(l,0) then reTwoPair(l)
                  else
                    if isPair(l) then rePair(l)
                    else
                      bestCard(l)


  def probabilityHighCard(l:List[Cartes],m:Int) : Int = 1;

