import Cartes.Cartes

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Cartes:

  enum Valeur :
    case UN
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

  def valueOf(s: Cartes): String = s match
    case Cartes.P(Valeur.UN,Couleur.COEUR) => "AS♥"
    case Cartes.P(Valeur.AS,Couleur.COEUR) => "AS♥"
    case Cartes.P(Valeur.DEUX,Couleur.COEUR) => "2♥"
    case Cartes.P(Valeur.TROIS,Couleur.COEUR) => "3♥"
    case Cartes.P(Valeur.QUATRE,Couleur.COEUR)=> "4♥"
    case Cartes.P(Valeur.CINQ,Couleur.COEUR)=> "5♥"
    case Cartes.P(Valeur.SIX,Couleur.COEUR) => "6♥"
    case Cartes.P(Valeur.SEPT,Couleur.COEUR) => "7♥"
    case Cartes.P(Valeur.HUIT,Couleur.COEUR) => "8♥"
    case Cartes.P(Valeur.NEUF,Couleur.COEUR) => "9♥"
    case Cartes.P(Valeur.DIX,Couleur.COEUR) => "10♥"
    case Cartes.P(Valeur.VALET,Couleur.COEUR) => "VALET♥"
    case Cartes.P(Valeur.DAME,Couleur.COEUR) => "DAME♥"
    case Cartes.P(Valeur.ROI,Couleur.COEUR) => "ROI♥"
    case Cartes.P(Valeur.AS,Couleur.TREFLE) =>"AS☘"
    case Cartes.P(Valeur.UN,Couleur.TREFLE) => "AS☘"
    case Cartes.P(Valeur.DEUX,Couleur.TREFLE) => "2☘"
    case Cartes.P(Valeur.TROIS,Couleur.TREFLE )=> "3☘"
    case Cartes.P(Valeur.QUATRE,Couleur.TREFLE) => "4☘"
    case Cartes.P(Valeur.CINQ,Couleur.TREFLE) => "5☘"
    case Cartes.P(Valeur.SIX,Couleur.TREFLE) => "6☘"
    case Cartes.P(Valeur.SEPT,Couleur.TREFLE) => "7☘"
    case Cartes.P(Valeur.HUIT,Couleur.TREFLE) => "8☘"
    case Cartes.P(Valeur.NEUF,Couleur.TREFLE) => "9☘"
    case Cartes.P(Valeur.DIX,Couleur.TREFLE) => "10☘"
    case Cartes.P(Valeur.VALET,Couleur.TREFLE) => "VALET☘"
    case Cartes.P(Valeur.DAME,Couleur.TREFLE) => "DAME☘"
    case Cartes.P(Valeur.ROI,Couleur.TREFLE) => "ROI☘"
    case Cartes.P(Valeur.AS,Couleur.PIQUE) => "AS♠"
    case Cartes.P(Valeur.UN,Couleur.PIQUE) => "AS♠"
    case Cartes.P(Valeur.DEUX,Couleur.PIQUE) => "2♠"
    case Cartes.P(Valeur.TROIS,Couleur.PIQUE) => "3♠"
    case Cartes.P(Valeur.QUATRE,Couleur.PIQUE) => "4♠"
    case Cartes.P(Valeur.CINQ,Couleur.PIQUE) => "5♠"
    case Cartes.P(Valeur.SIX,Couleur.PIQUE) => " 6♠"
    case Cartes.P(Valeur.SEPT,Couleur.PIQUE) => "7♠"
    case Cartes.P(Valeur.HUIT,Couleur.PIQUE) => "8♠"
    case Cartes.P(Valeur.NEUF,Couleur.PIQUE) => "9♠"
    case Cartes.P(Valeur.DIX,Couleur.PIQUE) => "10♠"
    case Cartes.P(Valeur.VALET,Couleur.PIQUE) => "VALET♠"
    case Cartes.P(Valeur.DAME,Couleur.PIQUE) => "DAME♠"
    case Cartes.P(Valeur.ROI,Couleur.PIQUE) => "ROI♠"
    case Cartes.P(Valeur.AS,Couleur.CARREAU) => "AS♦"
    case Cartes.P(Valeur.UN,Couleur.CARREAU) => "AS♦"
    case Cartes.P(Valeur.DEUX,Couleur.CARREAU) => "2♦"
    case Cartes.P(Valeur.TROIS,Couleur.CARREAU) => "3♦"
    case Cartes.P(Valeur.QUATRE,Couleur.CARREAU) => "4♦"
    case Cartes.P(Valeur.CINQ,Couleur.CARREAU) => "5♦"
    case Cartes.P(Valeur.SIX,Couleur.CARREAU) => "6♦"
    case Cartes.P(Valeur.SEPT,Couleur.CARREAU) => "7♦"
    case Cartes.P(Valeur.HUIT,Couleur.CARREAU) => "8♦"
    case Cartes.P(Valeur.NEUF,Couleur.CARREAU) => "9♦"
    case Cartes.P(Valeur.DIX,Couleur.CARREAU) => "10♦"
    case Cartes.P(Valeur.VALET,Couleur.CARREAU) => "VALET♦"
    case Cartes.P(Valeur.DAME,Couleur.CARREAU)=> "DAME♦"
    case Cartes.P(Valeur.ROI,Couleur.CARREAU) => "ROI♦"

  //entrée : une liste de cartes
  //sortie : un string correspondant aux cartes
  def afficheValeur(l : List[Cartes]): String = l match
    case Nil => ""
    case x::xs => valueOf(x) + " " + afficheValeur(xs)

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

  /** fonctions supports pour nos futurs fonctions ---------------------------------------------------------------------------------------*/

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
     case (x::y,_) => if x==c1 then y else x::retirerUneCarte(y,c1)

  //entrée : une liste de carte et le jeu complet
  //sortie : une liste contenant toute les cartes moins les cartes qui sont dans l1
  def retirerDesCartes(l1:List[Cartes]) : List[Cartes] = l1 match
      case Nil => touteLesCartes()
      case x::y => retirerUneCarte(retirerDesCartes(y),x)

  //entrée : une liste de cartes
  //sortie : la même liste sans la première carte
  def suivant(l1:List[Cartes]) : List[Cartes] = l1 match
    case x::y => y

  /** fonctions de définission de toutes les possibilités en fonction du moment de la partie ----------------------------------------------*/

  //entrée : une liste de cartes de taille 6 et la liste de toutes les cartes moins les 6 cartes
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour6(l1:List[Cartes],l2:List[Cartes]) : List[List[Cartes]] = l2 match
      case Nil => Nil
      case c::d => List(l1:::c::Nil):::toutesPossibilitesPour6(l1,d)

  //entrée : une liste de cartes de taille 6
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour6V2(l1:List[Cartes]) : List[List[Cartes]] = toutesPossibilitesPour6(l1,retirerDesCartes(l1))

  //entrée : liste de 6 cartes de la main du joueur (et du jeu) et une liste contenant toutes les cartes des adversaires
  //sortie : toutes les combinaisons qu'il peut obtenir à la fin de la distribution
  def toutesPossibilitesPour6V3(l1:List[Cartes],l2:List[Cartes]): List[List[Cartes]] = toutesPossibilitesPour6(l1,retirerDesCartes(l1:::l2))

  //entrée : une liste de carte de taille 5, une liste de carte contenant toutes les cartes du jeu sans les cartes
  //de l1 et une liste de cartes contenant toutes les cartes du jeu moins celles de l1 et moins la première
  def toutesPossibilitesPour5(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes]) : List[List[Cartes]] = (l2,l3) match
    case (a::Nil,Nil) => Nil
    case (a::b,c::Nil) => toutesPossibilitesPour5(l1,b,suivant(b))
    case (a::b,c::d) => List(l1:::a::c::Nil):::toutesPossibilitesPour5(l1,l2,d)

  //entrée : une liste de carte de taille 5, une liste de carte contenant toutes les cartes du jeu sans les cartes
  //de l1 et une liste de cartes contenant toutes les cartes du jeu moins celles de l1 et moins la première
  def toutesPossibilitesPour4(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes],l4:List[Cartes]) : List[List[Cartes]] = (l2,l3,l4) match
    case (e::f::Nil,h::Nil,Nil) => Nil
    case (a::b,c::d::Nil,e::Nil) => toutesPossibilitesPour4(l1,suivant(b),suivant(suivant(b)),suivant(suivant(suivant(b))))
    case (a::b,c::d,e::Nil) => toutesPossibilitesPour4(l1,b,suivant(b),suivant(suivant(b)))
    case (a::b,c::d,e::f) => List(l1:::a::c::e::Nil):::toutesPossibilitesPour4(l1,l2,l3,f)

  //entrée : une liste de cartes de taille 5
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour5V2(l1:List[Cartes]) : List[List[Cartes]] = toutesPossibilitesPour5(l1,retirerDesCartes(l1),retirerDesCartesEtLaPremiere(l1))

  //entrée : liste de 5 cartes de la main du joueur (et du jeu) et une liste contenant toutes les cartes des adversaires
  //sortie : toutes les combinaisons qu'il peut obtenir à la fin de la distribution
  def toutesPossibilitesPour5V3(l1:List[Cartes],l2:List[Cartes]): List[List[Cartes]] = toutesPossibilitesPour5(l1,retirerDesCartes(l1:::l2),retirerDesCartesEtLaPremiere(l1:::l2))

  /** fonctions qui ne marche pas ------------------------------------------------------------------------------------------------------*/

  //entrée : une liste de cartes de taille 2 et des listes initialialement parmi le jeu complet
  //sortie : une liste de listes de taille 7 incluant la liste d'avant avec toutes les combinaisons possibles
  def toutesPossibilitesPour2(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes],l4:List[Cartes],l5:List[Cartes],l6:List[Cartes]) : List[List[Cartes]] = (l2,l3,l4,l5,l6) match
    case (g::h::i::j::Nil,d::e::f::Nil,b::c::Nil,a::Nil,Nil) => Nil
    case (a::b,c::d::e::f::Nil,g::h::i::Nil,j::k::Nil,l::Nil) => toutesPossibilitesPour2(l1,b,suivant(b),suivant(suivant(b)),suivant(suivant(suivant(b))),suivant(suivant(suivant(suivant(b)))))
    case (_,a::b,c::d::e::Nil,f::g::Nil,h::Nil) => toutesPossibilitesPour2(l1,l2,b,suivant(b),suivant(suivant(b)),suivant(suivant(suivant(b))))
    case (_,_,a::b,c::d::Nil,e::Nil) => toutesPossibilitesPour2(l1,l2,l3,b,suivant(b),suivant(suivant(b)))
    case (_,_,_,a::b,c::Nil) => toutesPossibilitesPour2(l1,l2,l3,l4,b,suivant(b))
    case (a::b,c::d,e::f,g::h,i::j) => List(l1:::a::Nil:::c::Nil:::e::Nil:::g::Nil:::i::Nil):::toutesPossibilitesPour2(l1,l2,l3,l4,l5,j)

  def toutesPossibilitesPour2V2(l1:List[Cartes]) : List[List[Cartes]] = toutesPossibilitesPour2(l1,retirerDesCartes(l1),retirerDesCartesEtLaPremiere(l1),retirerDesCartesEtLes2Premieres(l1),retirerDesCartesEtLes3Premieres(l1),retirerDesCartesEtLes4Premieres(l1))

  //nous n'avons pas cherché à faire la version 3 puisque la 2 et la 1 ne marche pas

  /** fonctions d'aide pour trouver les possibilités du dessus ---------------------------------------------------------------------------*/

  //entrée : une liste de cartes
  //sortie : une liste contenant toutes les cartes du jeu moins celles de l1 et sans le premier élément
  def retirerDesCartesEtLaPremiere(l1:List[Cartes]) : List[Cartes] = l1 match
    case Nil => Nil
    case x::y => retirerDesCartes(y)

  def retirerDesCartesEtLes2Premieres(l1:List[Cartes]) : List[Cartes] = l1 match
    case Nil => Nil
    case x::y => retirerDesCartesEtLaPremiere(y)

  def retirerDesCartesEtLes3Premieres(l1:List[Cartes]) : List[Cartes] = l1 match
    case Nil => Nil
    case x::y => retirerDesCartesEtLes2Premieres(y)

  def retirerDesCartesEtLes4Premieres(l1:List[Cartes]) : List[Cartes] = l1 match
    case Nil => Nil
    case x::y => retirerDesCartesEtLes3Premieres(y)

  /** fonctions de comptage de mains dans les possibilités trouvées -----------------------------------------------------------------------*/

  //entrée : une liste de listes de 7 cartes et un compteur initialement à 0
  //sortie : le nombre de liste qui contiennent une paire dans la liste de liste
  @tailrec
  def nombrePaireDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isPair(x) then nombrePaireDansList(y,compteur+1) else nombrePaireDansList(y,compteur)
  
  @tailrec
  def nombreDoublePaireDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isTwoPair(x,0) then nombreDoublePaireDansList(y,compteur+1) else nombreDoublePaireDansList(y,compteur)

  @tailrec
  def nombreBrelanDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isBrelan(x) then nombreBrelanDansList(y,compteur+1) else nombreBrelanDansList(y,compteur)

  @tailrec
  def nombreQuinteDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isQuinte(x,0) then nombreQuinteDansList(y,compteur+1) else nombreQuinteDansList(y,compteur)

  @tailrec
  def nombreCouleurDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isFlush(x,0,0,0,0) then nombreCouleurDansList(y,compteur+1) else nombreCouleurDansList(y,compteur)

  @tailrec
  def nombreFullDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isFull(x) then nombreFullDansList(y,compteur+1) else nombreFullDansList(y,compteur)

  @tailrec
  def nombreCarreDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isCarre(x) then nombreCarreDansList(y,compteur+1) else nombreCarreDansList(y,compteur)

  @tailrec
  def nombreQuinteFlushDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isQuinteFlush(x) then nombreQuinteFlushDansList(y,compteur+1) else nombreQuinteFlushDansList(y,compteur)

  @tailrec
  def nombreQuinteFlushRoyaleDansList(l1:List[List[Cartes]], compteur:Int) : Int = l1 match
    case Nil => compteur
    case x::y => if isQuinteFlushRoyale(x,Couleur.CARREAU,-1,0) then nombreQuinteFlushRoyaleDansList(y,compteur+1) else nombreQuinteFlushRoyaleDansList(y,compteur)

  /** fonctions d'aide pour le calcul de probabilité --------------------------------------------------------------------------------------*/

  //calcul de probabilité quand la main est de 6 cartes ouvertes/fermées
  def probabilitePaireDans6(l1:List[Cartes]) : Double = nombrePaireDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteDoublePaireDans6(l1:List[Cartes]) : Double = nombreDoublePaireDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteBrelanDans6(l1:List[Cartes]) : Double = nombreBrelanDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteQuinteDans6(l1:List[Cartes]) : Double = nombreQuinteDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteCouleurDans6(l1:List[Cartes]) : Double = nombreCouleurDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteFullDans6(l1:List[Cartes]) : Double = nombreFullDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteCarreDans6(l1:List[Cartes]) : Double = nombreCarreDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteQuinteFlushDans6(l1:List[Cartes]) : Double = nombreQuinteFlushDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  def probabiliteQuinteFlushRoyaleDans6(l1:List[Cartes]) : Double = nombreQuinteFlushRoyaleDansList(toutesPossibilitesPour6V2(l1),0)/46.0

  //calcul de probabilité quand la main est de 5 cartes ouvertes/fermées
  def probabilitePaireDans5(l1:List[Cartes]) : Double = nombrePaireDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteDoublePaireDans5(l1:List[Cartes]) : Double = nombreDoublePaireDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteBrelanDans5(l1:List[Cartes]) : Double = nombreBrelanDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteQuinteDans5(l1:List[Cartes]) : Double = nombreQuinteDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteCouleurDans5(l1:List[Cartes]) : Double = nombreCouleurDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteFullDans5(l1:List[Cartes]) : Double = nombreFullDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteCarreDans5(l1:List[Cartes]) : Double = nombreCarreDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteQuinteFlushDans5(l1:List[Cartes]) : Double = nombreQuinteFlushDansList(toutesPossibilitesPour5V2(l1),0)/1081.0

  def probabiliteQuinteFlushRoyaleDans5(l1:List[Cartes]) : Double = nombreQuinteFlushRoyaleDansList(toutesPossibilitesPour5V2(l1),0)/1081.0
  //calcul de probabilité quand la main est de 2 cartes ouvertes/fermées
  def probabilitePaireDans2(l1:List[Cartes]) : Double = nombrePaireDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteDoublePaireDans2(l1:List[Cartes]) : Double = nombreDoublePaireDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteBrelanDans2(l1:List[Cartes]) : Double = nombreBrelanDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteQuinteDans2(l1:List[Cartes]) : Double = nombreQuinteDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteCouleurDans2(l1:List[Cartes]) : Double = nombreCouleurDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteFullDans2(l1:List[Cartes]) : Double = nombreFullDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteCarreDans2(l1:List[Cartes]) : Double = nombreCarreDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteQuinteFlushDans2(l1:List[Cartes]) : Double = nombreQuinteFlushDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  def probabiliteQuinteFlushRoyaleDans2(l1:List[Cartes]) : Double = nombreQuinteFlushRoyaleDansList(toutesPossibilitesPour2V2(l1),0)/1081.0

  /** grosse fonction numéro 2 du projet ------------------------------------------------------------------------------------------------*/

  //entrée : la main du joueur (cartes fermées et ouvertes) et la main qu'il veut avoir
  //sortie : probabilité d'avoir cette main quand toutes les cartes sont distribuées (5 cartes dans le jeu et 2 dans la main)
  def probabilite(l1:List[Cartes], main: Hand) : Double = (l1.length,main) match
    case (6,Hand.Paire) => probabilitePaireDans6(l1)
    case (5,Hand.Paire) => probabilitePaireDans5(l1)
    case (2,Hand.Paire) => probabilitePaireDans2(l1)
    case (6,Hand.DoublePaire) => probabiliteDoublePaireDans6(l1)
    case (5,Hand.DoublePaire) => probabiliteDoublePaireDans5(l1)
    case (2,Hand.DoublePaire) => probabiliteDoublePaireDans2(l1)
    case (6,Hand.Brelan) => probabiliteBrelanDans6(l1)
    case (5,Hand.Brelan) => probabiliteBrelanDans5(l1)
    case (2,Hand.Brelan) => probabiliteBrelanDans2(l1)
    case (6,Hand.Quinte) => probabiliteQuinteDans6(l1)
    case (5,Hand.Quinte) => probabiliteQuinteDans5(l1)
    case (2,Hand.Quinte) => probabiliteQuinteDans2(l1)
    case (6,Hand.Couleur) => probabiliteCouleurDans6(l1)
    case (5,Hand.Couleur) => probabiliteCouleurDans5(l1)
    case (2,Hand.Couleur) => probabiliteCouleurDans2(l1)
    case (6,Hand.Full) => probabiliteFullDans6(l1)
    case (5,Hand.Full) => probabiliteFullDans5(l1)
    case (2,Hand.Full) => probabiliteFullDans2(l1)
    case (6,Hand.Carre) => probabiliteCarreDans6(l1)
    case (5,Hand.Carre) => probabiliteCarreDans5(l1)
    case (2,Hand.Carre) => probabiliteCarreDans2(l1)
    case (6,Hand.QuinteFlush) => probabiliteQuinteFlushDans6(l1)
    case (5,Hand.QuinteFlush) => probabiliteQuinteFlushDans5(l1)
    case (2,Hand.QuinteFlush) => probabiliteQuinteFlushDans2(l1)
    case (6,Hand.QuinteFlushRoyale) => probabiliteQuinteFlushRoyaleDans6(l1)
    case (5,Hand.QuinteFlushRoyale) => probabiliteQuinteFlushRoyaleDans5(l1)
    case (2,Hand.QuinteFlushRoyale) => probabiliteQuinteFlushRoyaleDans2(l1)

  //entrée : une liste de listes comportant toutes les possibilités du joueur et une liste de listes comportant tous les possibilités
  //des autres joueurs et l3 qui est la copie de l2 du début
  //sortie : une liste de listes montrant toutes les combinaisons possibles (produit cartésien) suivant le schéma suivant :
  //(valeur1Joueur,...,valeur7Joueur,valeur1Adversaire,...valeur7Adversaire)
  def combinaisons(l1:List[List[Cartes]],l2:List[List[Cartes]],l3:List[List[Cartes]]) : List[List[Cartes]] = (l1,l2) match
    case (a::Nil,b::Nil) => List(a:::b)
    case (a::b,c::Nil) => List(a:::c):::combinaisons(b,l3,l3)
    case (a::b,c::d) => List(a:::c):::combinaisons(l1,d,l3)

  //entrée : la liste des cartes des autres joueurs (main) et la liste des cartes sur le jeu et la liste de la main du joueur
  //et l4 qui est une copie de la valeur de l1 de départ
  //sortie : toutes les possibilités de sortie des autres joueurs
  def toutesPossibilitesAutresJoueursPour5(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes],l4:List[Cartes]) : List[List[Cartes]] = l1 match
    case x::y::Nil => toutesPossibilitesPour5V3(x::y::Nil:::l2,retirerUneCarte(retirerUneCarte(l4:::l3,y),x))
    case x::y::z => toutesPossibilitesPour5V3(x::y::Nil:::l2,retirerUneCarte(retirerUneCarte(l4:::l3,y),x)) ::: toutesPossibilitesAutresJoueursPour5(z,l2,l3,l4)

  //entrée : la liste des cartes des autres joueurs (main) et la liste des cartes sur le jeu et la liste de la main du joueur
  //et l4 qui est une copie de la valeur de l1 de départ
  //sortie : toutes les possibilités de sortie des autres joueurs
  def toutesPossibilitesAutresJoueursPour6(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes],l4:List[Cartes]) : List[List[Cartes]] = l1 match
    case x::y::Nil => toutesPossibilitesPour6V3(x::y::Nil:::l2,retirerUneCarte(retirerUneCarte(l4:::l3,y),x))
    case x::y::z => toutesPossibilitesPour6V3(x::y::Nil:::l2,retirerUneCarte(retirerUneCarte(l4:::l3,y),x)) ::: toutesPossibilitesAutresJoueursPour6(z,l2,l3,l4)
  
  //entrée : la liste des cartes du joueur (main) et une liste contenant toutes les cartes des autres joueurs (mains) et une liste du jeu
  //sortie : toutes les combinaisons sous la forme de la fonction juste au dessus
  def combinaisonsV2Pour5(l1:List[Cartes],l2:List[Cartes], l3:List[Cartes]) : List[List[Cartes]] = combinaisons(toutesPossibilitesPour5V3(l1:::l3,l2),toutesPossibilitesAutresJoueursPour5(l2,l3,l1,l2),toutesPossibilitesAutresJoueursPour5(l2,l3,l1,l2))

  //entrée : la liste des cartes du joueur (main) et une liste contenant toutes les cartes des autres joueurs (mains) et une liste du jeu
  //sortie : toutes les combinaisons sous la forme de la fonction juste au dessus
  def combinaisonsV2Pour6(l1:List[Cartes],l2:List[Cartes], l3:List[Cartes]) : List[List[Cartes]] = combinaisons(toutesPossibilitesPour6V3(l1:::l3,l2),toutesPossibilitesAutresJoueursPour6(l2,l3,l1,l2),toutesPossibilitesAutresJoueursPour6(l2,l3,l1,l2))

  def nbrCombinaisonGagnante(l1:List[List[Cartes]]) : Double = l1 match
    case Nil => 0.0
    case x::y => if bestHandBetween(x.take(7),x.takeRight(7)) == 1 then nbrCombinaisonGagnante(y)+1.0 else nbrCombinaisonGagnante(y)

  /** fonction généraliste numéro 4 */
  //entrée : l1 : liste des cartes du joueur, l2 : liste des cartes en jeu, l3 : liste des cartes des autres
  def fonction4(l1:List[Cartes],l2:List[Cartes],l3:List[Cartes]): Double = if l2.size == 3 then
    nbrCombinaisonGagnante(combinaisonsV2Pour5(l1,l3,l2)) / combinaisonsV2Pour5(l1,l3,l2).size
    else nbrCombinaisonGagnante(combinaisonsV2Pour6(l1,l3,l2)) / combinaisonsV2Pour5(l1,l3,l2).size

  /** Fonction utilisé pour calculer ce que le joueur a dans sa main */

  //entrée : une liste de cartes trié (la main du joueur), ainsi que la valeur d'une carte
  //sortie : Nombre de fois que la valeur apparaît dans la main du joueur
  def nbOccurrence(l: List[Cartes], v : Valeur) : Int = l match
    case Nil => 0
    case Cartes.P(va,c)::Nil => if va == v then 1 else 0
    case Cartes.P(va,c)::xs => if va == v then 1+nbOccurrence(xs,v) else nbOccurrence(xs,v)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que la valeur d'une carte
  //sortie : une liste de carte qui ne contient aucune occurrence de cartes ayant la valeur en paramètre
  def deleteOccurrence(l: List[Cartes], v : Valeur) : List[Cartes] =
    if nbOccurrence(l,v) == 0 then l else l match
      case Cartes.P(va,c)::Nil => if va == v then Nil else Cartes.P(va,c)::Nil
      case Cartes.P(va,c)::xs => if va == v then deleteOccurrence(xs,v) else Cartes.P(va,c)::deleteOccurrence(xs,v)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que la valeur d'une carte
  //sortie : une liste de carte qui contient seulement les occurrences de cartes ayant la valeur en paramètre
  def deleteAllExcept(l:List[Cartes], v:Valeur) : List[Cartes] =
    if nbOccurrence(l,v) == 0 then Nil else l match
      case x::Nil => x::Nil
      case Cartes.P(va,c)::xs => if va == v then Cartes.P(va,c)::deleteAllExcept(xs,v) else deleteAllExcept(xs,v)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède une paire sinon false
  @tailrec
  def isPair(l: List[Cartes]): Boolean = l match
    case Nil => false
    case x::Nil => false
    case Cartes.P(v,c)::xs => if nbOccurrence(xs,v)==1 then true else isPair(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : la pair du joueur
  @tailrec
  def rePair(l: List[Cartes]): List[Cartes] = l match
      case Cartes.P(v,c)::xs => if nbOccurrence(xs,v)==1 then deleteAllExcept(l,v) else rePair(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède deux paire sinon false
  @tailrec
  def isTwoPair(l: List[Cartes], nbPair: Int) : Boolean =
    if nbPair == 2 then true else l match
      case Nil => false
      case x::Nil => false
      case Cartes.P(v,c)::xs => if nbOccurrence(xs,v) ==1 then isTwoPair(xs,nbPair+1) else isTwoPair(xs,nbPair)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de cartes contenant seulement les deux meilleurs paires (cas où le joueur a 6 paires en main)
  def calculPair(l:List[Cartes]) : List[Cartes] =
    if l.length > 4 then l match
      case Cartes.P(v,c)::xs => deleteOccurrence(l,v)
    else l

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de cartes contenant toutes les paires que le joueur possède en main
  def re2TwoPair(l:List[Cartes]) : List[Cartes] = l match
    case Nil => Nil
    case Cartes.P(va,cb)::Cartes.P(vb,c)::Nil => if va.ordinal == vb.ordinal then Cartes.P(va,cb)::Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,cb)::xs => if nbOccurrence(xs,va) == 1 then deleteAllExcept(l,va):::re2TwoPair(deleteOccurrence(xs,va)) else re2TwoPair(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de cartes contenant la double paire du joueur
  def reTwoPair(l:List[Cartes]) : List[Cartes] = calculPair(re2TwoPair(l))

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède un brelan sinon false
  @tailrec
  def isBrelan(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurrence(xs,va) == 2 then true else isBrelan(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de cartes contenant la brelan du joueur
  @tailrec
  def reBrelan(l: List[Cartes]) : List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurrence(xs,v)==2 then deleteAllExcept(l,v) else reBrelan(xs)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que le nombre de carte à la suite qu'il possède, initialement à 0
  //sortie : true si le joueur possède une suite sinon false
  @tailrec
  def isQuinte(l: List[Cartes], nbSui : Int) : Boolean = l match
    case Nil => nbSui >=4
    case Cartes.P(va,c)::Cartes.P(vb,ca)::Nil => if vb.ordinal - va.ordinal == 1 then isQuinte(Nil,nbSui+1) else isQuinte(Nil,nbSui)
    case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then isQuinte(Cartes.P(vb,ca)::xs,nbSui+1) else isQuinte(Cartes.P(vb,ca)::xs,nbSui)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que la valeur de la carte d'avant, initialement à 0
  //sortie : une liste de carte contenant la quinte du joueur
  def reQuinteBis(l : List[Cartes],va :Int ): List[Cartes] = l match
    case Cartes.P(vb,c)::Nil => if vb.ordinal - va == 1 then Cartes.P(vb,c)::Nil else Nil
    case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinteBis(Cartes.P(vb,ca)::xs,va.ordinal) else reQuinteBis(Cartes.P(vb,ca)::xs,va.ordinal)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de carte contenant la quinte du joueur
  def reQuinte(l: List[Cartes]) : List[Cartes] = reQuinteBis(l,0)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que le nombre de coeur, trefle, carreau, pique, que le joueur possède, ces valeurs sont initialisées à 0
  //sortie : true si le joueur possède une couleur
  @tailrec
  def isFlush(l: List[Cartes], nbCoeur: Int, nbTrefle: Int, nbCarreau: Int, nbPique: Int) : Boolean =
    if nbCoeur >= 5 || nbTrefle >= 5 || nbCarreau >= 5 || nbPique >= 5 then true else l match
      case Nil => false
      case Cartes.P(va,c)::Nil => if c == Couleur.COEUR then isFlush(Nil,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
        if c == Couleur.TREFLE then isFlush(Nil,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
          if c == Couleur.CARREAU then isFlush(Nil,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(Nil,nbCoeur,nbTrefle,nbCarreau,nbPique+1)
      case Cartes.P(va,c)::xs => if c == Couleur.COEUR then isFlush(xs,nbCoeur+1,nbTrefle,nbCarreau,nbPique) else
        if c == Couleur.TREFLE then isFlush(xs,nbCoeur,nbTrefle+1,nbCarreau,nbPique) else
          if c == Couleur.CARREAU then isFlush(xs,nbCoeur,nbTrefle,nbCarreau+1,nbPique) else isFlush(xs,nbCoeur,nbTrefle,nbCarreau,nbPique+1)

  //entrée : une liste de cartes trié (la main du joueur), ainsi que le nombre de coeur, trefle, carreau, pique, que le joueur possède, ces valeurs sont initialisées à 0
  //sortie : la valeur de la couleur que le joueur possède
  @tailrec
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

  //entrée : une liste de cartes trié (la main du joueur), la valeur de la couleur qu'il possède
  //sortie : la couleur que le joueur possède en main
  def reFlushBis(l:List[Cartes],coul: Couleur) : List[Cartes] =  l match
    case Cartes.P(v,c)::Nil => if c == coul then Cartes.P(v,c)::Nil else Nil
    case Cartes.P(v,c)::xs => if c == coul then Cartes.P(v,c)::reFlushBis(xs,coul) else reFlushBis(xs,coul)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : la couleur que le joueur possède en main, on utilise triQuinte pour retourner seulement les meilleurs cartes de la couleur
  def reFlush(l:List[Cartes]): List[Cartes] = triQuinte(reFlushBis(l,reCoulFlush(l,0,0,0,0)))

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède un full sinon false
  @tailrec
  def isFull(l: List[Cartes]): Boolean = l match
      case Nil => false
      case x::Nil => false
      case Cartes.P(v,c)::xs => if nbOccurrence(xs,v) == 2  then isPair(deleteOccurrence(l,v)) else
        if nbOccurrence(xs,v) == 1 then isBrelan(deleteOccurrence(l,v)) else isFull(xs)

  //entrée : une liste de cartes trié (la main du joueur), le brelan du joueur, la paire du joueur, un boolean pour savoir si le brelan est avant la paire ou non
  //sortie : une liste de cartes contenant le full du joueur
  @tailrec
  def reFull(l:List[Cartes], bre:List[Cartes], pair:List[Cartes], bretrouver: Int): List[Cartes] = l match
    case Nil => bre:::pair
    case Cartes.P(v,c)::xs => if nbOccurrence(xs,v) == 2 && bretrouver != 1 then reFull(deleteOccurrence(l,v),reBrelan(l),pair,1) else
      if nbOccurrence(xs,v) >= 1 then reFull(deleteOccurrence(l,v),bre,rePair(l),bretrouver) else reFull(xs,bre,pair,bretrouver)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède un carré sinon false
  @tailrec
  def isCarre(l: List[Cartes]) : Boolean = l match
    case Nil => false
    case Cartes.P(va,c)::Nil => false
    case Cartes.P(va,c)::xs => if nbOccurrence(xs,va) == 3 then true else isCarre(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : la carré du joueur
  @tailrec
  def reCarre(l: List[Cartes]): List[Cartes] = l match
    case Cartes.P(v,c)::xs => if nbOccurrence(xs,v)==3 then deleteAllExcept(l,v) else reCarre(xs)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : true si le joueur possède une quinteFlush sinon false
  def isQuinteFlush(l: List[Cartes]) : Boolean =
    if isFlush(l,0,0,0,0) then isQuinte(reFlush(l),0)
    else false

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de cartes contenant les meilleurs cartes nécessaires (cas où le joueur a une quinte/couleur de plus de 5 cartes)
  def triQuinte(l: List[Cartes]) : List[Cartes] = l.drop(l.length-5)

  //entrée : une liste de cartes trié (la main du joueur), un entier utile pour faire un premier tour à vide, initialisé à -1,
  //  un entier correspondant à la valeur de la carte d'avant, initialisé à 0
  //  Nous faisons le tour à vide pour permettre de rentrer la main du joueur directement dans la fonction et non la couleur de la main du joueur
  //  Ce qui facilite l'utilisation de la fonction et permet de ne pas avoir d'oubli d'utilisation
  //sortie : la quinteFlush du joueur (peut y avoir plus de 5 cartes)
  def reQuinteFlushBis(l:List[Cartes], n:Int, va:Int): List[Cartes] =
    if n < 0 then reQuinteFlushBis(reFlush(l),1,0) else l match
      case Cartes.P(vb,c)::Nil => if vb.ordinal - va == 1 then Cartes.P(vb,c)::Nil else Nil
      case Cartes.P(va,c)::Cartes.P(vb,ca)::xs => if vb.ordinal - va.ordinal == 1 then Cartes.P(va,c)::reQuinteFlushBis(Cartes.P(vb,ca)::xs,1,va.ordinal) else reQuinteFlushBis(Cartes.P(vb,ca)::xs,1,va.ordinal)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : la quinteFlush du joueur (5 cartes)
  def reQuinteFlush(l:List[Cartes]) : List[Cartes] = triQuinte(reQuinteFlushBis(l,-1,0))

  //entrée : une liste de cartes trié (la main du joueur), la couleur de la flush, un entier utilisé pour faire un tour à vide, initialisé à -1,
  //  un entier correspondant au nombre de suite que le joueur possède, initialisé à 0
  //sortie : true si le joueur possède une quinteflushroyale sinon false
  @tailrec
  def isQuinteFlushRoyale(l: List[Cartes], col : Couleur, start : Int, nbSuit : Int) : Boolean =
    if start <0 then isQuinteFlushRoyale(l,reCoulFlush(l,0,0,0,0),1,0) else l match
      case Cartes.P(va,c)::Nil =>  va == Valeur.AS && c == col && nbSuit>=4
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then isQuinteFlushRoyale(xs,col,1,nbSuit+1) else isQuinteFlushRoyale(xs,col,1,nbSuit)

  //entrée : une liste de cartes trié (la main du joueur), un entier utilisé pour faire un tour à vide, initialisé à -1,
  //  un entier correspondant au nombre de suite que le joueur possède, initialisé à 0
  //sortie : la quinteFlushRoyale du joueur
  def reQuinteFlushRoyale(l: List[Cartes], n:Int, nbSuit: Int) : List[Cartes] =
    if n<0 then reQuinteFlushRoyale(reFlush(l),1,0) else l match
      case Cartes.P(va,c)::Nil => if va == Valeur.AS then Cartes.P(va,c)::Nil else Nil
      case Cartes.P(va,c)::xs => if va.ordinal == (Valeur.AS.ordinal-4 + nbSuit) then Cartes.P(va,c)::reQuinteFlushRoyale(xs,1,nbSuit+1) else reQuinteFlushRoyale(xs,1,nbSuit)

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de carte contenant seulement la meilleure carte (celle avec la plus grosse valeur)
  def bestCard(l:List[Cartes]) : List[Cartes] = l.takeRight(1)

  /** grosse fonction numéro 1 du projet ------------------------------------------------------------------------------------------------*/

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : une liste de carte contenant la meilleur main du joueur
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

  //entrée : une liste de cartes trié (la main du joueur)
  //sortie : un entier correspond au "rank" de sa meilleure main
  def bestHandBis(l:List[Cartes]) : Int =
    if isQuinteFlush(l) then
      if isQuinteFlushRoyale(l,Couleur.CARREAU,-1,0) then 10 else 9
    else
      if isCarre(l) then 8
      else
        if isFull(l) then 7
        else
          if isFlush(l,0,0,0,0) then 6
          else
            if isQuinte(l,0) then 5
            else
              if isBrelan(l) then 4
              else
                if isTwoPair(l,0) then 3
                else
                  if isPair(l) then 2
                  else
                    1

  //entrée : une liste de cartes trié (la main du joueur), une liste de cartes trié (la main d'un autre joueur)
  //sortie : un entier pour savoir qui a la meilleur main entre les deux joueurs, 1 si l>l1, 0 si égal et -1 si l1>l
  def bestHandBetween(l:List[Cartes],l1:List[Cartes]): Int = if bestHandBis(l) > bestHandBis(l1) then 1 else
    if bestHandBis(l) < bestHandBis(l1) then -1 else (bestCard(l),bestCard(l1)) match
      case (Cartes.P(x:Valeur,_)::_,Cartes.P(y:Valeur,_)::_) => if x.ordinal > y.ordinal then 1 else if x.ordinal < y.ordinal then -1 else 0

  def probabilityHighCard(l:List[Cartes],m:Int) : Int = 1;
