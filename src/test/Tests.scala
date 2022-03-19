import Cartes.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import scala.util.Sorting

class Tests extends AnyFlatSpec {

  "BestHand Carre" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.SIX,Couleur.CARREAU),Cartes.P(Valeur.SIX,Couleur.COEUR),Cartes.P(Valeur.SIX,Couleur.PIQUE))

    val corre = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE))

    isCarre(l) shouldBe true
    bestHand(l).length shouldBe 4
    bestHand(l) shouldBe corre
  }

  "BestHand RoyalFlush" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.DIX,Couleur.CARREAU),Cartes.P(Valeur.VALET,Couleur.CARREAU),Cartes.P(Valeur.DAME,Couleur.CARREAU),Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU),Cartes.P(Valeur.ROI,Couleur.PIQUE),Cartes.P(Valeur.AS,Couleur.PIQUE))

    val lRES = List[Cartes](Cartes.P(Valeur.DIX,Couleur.CARREAU),Cartes.P(Valeur.VALET,Couleur.CARREAU),Cartes.P(Valeur.DAME,Couleur.CARREAU),Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU))

    bestHand(l).length shouldBe 5
    bestHand(l) shouldBe lRES
  }

  "BestHand QuinteFlush" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.HUIT, Couleur.CARREAU), Cartes.P(Valeur.NEUF, Couleur.CARREAU), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.CARREAU),Cartes.P(Valeur.VALET, Couleur.PIQUE),Cartes.P(Valeur.DAME, Couleur.PIQUE),Cartes.P(Valeur.DAME, Couleur.CARREAU))

    val lRES = List[Cartes](Cartes.P(Valeur.HUIT, Couleur.CARREAU), Cartes.P(Valeur.NEUF, Couleur.CARREAU), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.CARREAU),Cartes.P(Valeur.DAME, Couleur.CARREAU))

    isFlush(l,0,0,0,0) shouldBe true
    isQuinte(l,0) shouldBe true
    reQuinteFlush(l).length shouldBe 5
    bestHand(l) shouldBe lRES
  }

  "BestHand Full" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.DIX,Couleur.PIQUE),Cartes.P(Valeur.DIX,Couleur.CARREAU),Cartes.P(Valeur.DIX,Couleur.COEUR))

    val lRES = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.DIX,Couleur.CARREAU),Cartes.P(Valeur.DIX,Couleur.COEUR))

    isFull(l) shouldBe true
    bestHand(l) shouldBe lRES
  }

  "BestHand Flush" should "be defined" in {
    val l = List[Cartes]( Cartes.P(Valeur.DEUX, Couleur.CARREAU), Cartes.P(Valeur.QUATRE, Couleur.CARREAU),Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.CARREAU),Cartes.P(Valeur.DAME, Couleur.CARREAU),Cartes.P(Valeur.ROI, Couleur.CARREAU))

    isFlush(l,0,0,0,0) shouldBe true
    reCoulFlush(l,0,0,0,0) shouldBe Couleur.CARREAU
    bestHand(l).length shouldBe 5
    bestHand(l) shouldBe l.drop(2)
  }

  "BestHand Quinte" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.HUIT, Couleur.COEUR), Cartes.P(Valeur.NEUF, Couleur.TREFLE), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.PIQUE),Cartes.P(Valeur.DAME, Couleur.PIQUE),Cartes.P(Valeur.ROI, Couleur.COEUR))

    isQuinte(l,0) shouldBe true
    bestHand(l) shouldBe l
  }

  "BestHand Brelan" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.SIX,Couleur.PIQUE),Cartes.P(Valeur.ROI,Couleur.CARREAU))

    val corre = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE))

    isFull(l) shouldBe false
    isBrelan(l) shouldBe true
    bestHand(l) shouldBe corre
  }

  "BestHand TwoPair" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.SIX,Couleur.CARREAU),Cartes.P(Valeur.SIX,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.TREFLE))

    isTwoPair(l,0) shouldBe true
    bestHand(l).length shouldBe 4
    bestHand(l) shouldBe l.drop(2)
  }

  "BestHand Pair" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.DAME,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.SIX,Couleur.CARREAU))

    val corre = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.PIQUE))

    isPair(l) shouldBe true
    bestHand(l) shouldBe corre
  }

  "BestHand High" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.DEUX,Couleur.PIQUE),Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.SIX,Couleur.CARREAU),Cartes.P(Valeur.DAME,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.TREFLE))

    val corre = List[Cartes](Cartes.P(Valeur.ROI,Couleur.TREFLE))

    bestHand(l) shouldBe corre
  }

  "retirer une carte" should "be defined" in {
    val l1 = List(Cartes.P(Valeur.SIX,Couleur.PIQUE),Cartes.P(Valeur.SEPT,Couleur.PIQUE),Cartes.P(Valeur.HUIT,Couleur.PIQUE),Cartes.P(Valeur.VALET,Couleur.CARREAU))
    val test = Cartes.P(Valeur.HUIT,Couleur.PIQUE)
    val l2 = List(Cartes.P(Valeur.SIX,Couleur.PIQUE),Cartes.P(Valeur.SEPT,Couleur.PIQUE),Cartes.P(Valeur.VALET,Couleur.CARREAU))

    retirerUneCarte(l1,test) shouldBe l2
  }

  "retirer des cartes" should "be defined" in {
    val l1 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR), Cartes.P(Valeur.CINQ,Couleur.COEUR), Cartes.P(Valeur.SIX,Couleur.COEUR), Cartes.P(Valeur.SEPT,Couleur.COEUR), Cartes.P(Valeur.HUIT,Couleur.COEUR), Cartes.P(Valeur.NEUF,Couleur.COEUR), Cartes.P(Valeur.DIX,Couleur.COEUR), Cartes.P(Valeur.VALET,Couleur.COEUR), Cartes.P(Valeur.DAME,Couleur.COEUR), Cartes.P(Valeur.ROI,Couleur.COEUR), Cartes.P(Valeur.AS,Couleur.TREFLE), Cartes.P(Valeur.DEUX,Couleur.TREFLE), Cartes.P(Valeur.TROIS,Couleur.TREFLE), Cartes.P(Valeur.QUATRE,Couleur.TREFLE), Cartes.P(Valeur.CINQ,Couleur.TREFLE), Cartes.P(Valeur.SIX,Couleur.TREFLE), Cartes.P(Valeur.SEPT,Couleur.TREFLE), Cartes.P(Valeur.HUIT,Couleur.TREFLE), Cartes.P(Valeur.NEUF,Couleur.TREFLE), Cartes.P(Valeur.DIX,Couleur.TREFLE), Cartes.P(Valeur.VALET,Couleur.TREFLE), Cartes.P(Valeur.DAME,Couleur.TREFLE), Cartes.P(Valeur.ROI,Couleur.TREFLE), Cartes.P(Valeur.AS,Couleur.PIQUE), Cartes.P(Valeur.DEUX,Couleur.PIQUE), Cartes.P(Valeur.TROIS,Couleur.PIQUE), Cartes.P(Valeur.QUATRE,Couleur.PIQUE), Cartes.P(Valeur.CINQ,Couleur.PIQUE), Cartes.P(Valeur.SIX,Couleur.PIQUE), Cartes.P(Valeur.SEPT,Couleur.PIQUE), Cartes.P(Valeur.HUIT,Couleur.PIQUE), Cartes.P(Valeur.NEUF,Couleur.PIQUE)
    )

    val l2 = List (Cartes.P(Valeur.DIX,Couleur.PIQUE), Cartes.P(Valeur.VALET,Couleur.PIQUE), Cartes.P(Valeur.DAME,Couleur.PIQUE), Cartes.P(Valeur.ROI,Couleur.PIQUE), Cartes.P(Valeur.AS,Couleur.CARREAU), Cartes.P(Valeur.DEUX,Couleur.CARREAU), Cartes.P(Valeur.TROIS,Couleur.CARREAU), Cartes.P(Valeur.QUATRE,Couleur.CARREAU), Cartes.P(Valeur.CINQ,Couleur.CARREAU), Cartes.P(Valeur.SIX,Couleur.CARREAU), Cartes.P(Valeur.SEPT,Couleur.CARREAU), Cartes.P(Valeur.HUIT,Couleur.CARREAU), Cartes.P(Valeur.NEUF,Couleur.CARREAU), Cartes.P(Valeur.DIX,Couleur.CARREAU), Cartes.P(Valeur.VALET,Couleur.CARREAU), Cartes.P(Valeur.DAME,Couleur.CARREAU), Cartes.P(Valeur.ROI,Couleur.CARREAU)
    )

    retirerDesCartes(l1) shouldBe l2
  }

  "toutes possibilitees pour 6V2" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR), Cartes.P(Valeur.CINQ,Couleur.COEUR), Cartes.P(Valeur.SIX,Couleur.COEUR))
    val l1 = l0:::Cartes.P(Valeur.SEPT,Couleur.COEUR)::Nil
    val l2 = l0:::Cartes.P(Valeur.HUIT,Couleur.COEUR)::Nil
    val l3 = l0:::Cartes.P(Valeur.NEUF,Couleur.COEUR)::Nil
    val l4 = l0:::Cartes.P(Valeur.DIX,Couleur.COEUR)::Nil
    val l5 = l0:::Cartes.P(Valeur.VALET,Couleur.COEUR)::Nil
    val l6 = l0:::Cartes.P(Valeur.DAME,Couleur.COEUR)::Nil
    val l7 = l0:::Cartes.P(Valeur.ROI,Couleur.COEUR)::Nil
    val l8 = l0:::Cartes.P(Valeur.AS,Couleur.TREFLE)::Nil
    val l9 = l0:::Cartes.P(Valeur.DEUX,Couleur.TREFLE)::Nil
    val l10 =l0:::Cartes.P(Valeur.TROIS,Couleur.TREFLE)::Nil
    val l11= l0:::Cartes.P(Valeur.QUATRE,Couleur.TREFLE)::Nil
    val l12 =l0:::Cartes.P(Valeur.CINQ,Couleur.TREFLE)::Nil
    val l13 =l0:::Cartes.P(Valeur.SIX,Couleur.TREFLE)::Nil
    val l14= l0:::Cartes.P(Valeur.SEPT,Couleur.TREFLE)::Nil
    val l15= l0:::Cartes.P(Valeur.HUIT,Couleur.TREFLE)::Nil
    val l16= l0:::Cartes.P(Valeur.NEUF,Couleur.TREFLE)::Nil
    val l17= l0:::Cartes.P(Valeur.DIX,Couleur.TREFLE)::Nil
    val l18= l0:::Cartes.P(Valeur.VALET,Couleur.TREFLE)::Nil
    val l19= l0:::Cartes.P(Valeur.DAME,Couleur.TREFLE)::Nil
    val l20= l0:::Cartes.P(Valeur.ROI,Couleur.TREFLE)::Nil
    val l21= l0:::Cartes.P(Valeur.AS,Couleur.PIQUE)::Nil
    val l22= l0:::Cartes.P(Valeur.DEUX,Couleur.PIQUE)::Nil
    val l23= l0:::Cartes.P(Valeur.TROIS,Couleur.PIQUE)::Nil
    val l24= l0:::Cartes.P(Valeur.QUATRE,Couleur.PIQUE)::Nil
    val l25= l0:::Cartes.P(Valeur.CINQ,Couleur.PIQUE)::Nil
    val l26= l0:::Cartes.P(Valeur.SIX,Couleur.PIQUE)::Nil
    val l27= l0:::Cartes.P(Valeur.SEPT,Couleur.PIQUE)::Nil
    val l28= l0:::Cartes.P(Valeur.HUIT,Couleur.PIQUE)::Nil
    val l29 = l0:::Cartes.P(Valeur.NEUF,Couleur.PIQUE)::Nil
    val l30 = l0:::Cartes.P(Valeur.DIX,Couleur.PIQUE)::Nil
    val l31 = l0:::Cartes.P(Valeur.VALET,Couleur.PIQUE)::Nil
    val l32 = l0:::Cartes.P(Valeur.DAME,Couleur.PIQUE)::Nil
    val l33 = l0:::Cartes.P(Valeur.ROI,Couleur.PIQUE)::Nil
    val l34 = l0:::Cartes.P(Valeur.AS,Couleur.CARREAU)::Nil
    val l35 = l0:::Cartes.P(Valeur.DEUX,Couleur.CARREAU)::Nil
    val l36 = l0:::Cartes.P(Valeur.TROIS,Couleur.CARREAU)::Nil
    val l37 = l0:::Cartes.P(Valeur.QUATRE,Couleur.CARREAU)::Nil
    val l38 = l0:::Cartes.P(Valeur.CINQ,Couleur.CARREAU)::Nil
    val l39 = l0:::Cartes.P(Valeur.SIX,Couleur.CARREAU)::Nil
    val l40 = l0:::Cartes.P(Valeur.SEPT,Couleur.CARREAU)::Nil
    val l41 = l0:::Cartes.P(Valeur.HUIT,Couleur.CARREAU)::Nil
    val l42= l0:::Cartes.P(Valeur.NEUF,Couleur.CARREAU)::Nil
    val l43= l0:::Cartes.P(Valeur.DIX,Couleur.CARREAU)::Nil
    val l44= l0:::Cartes.P(Valeur.VALET,Couleur.CARREAU)::Nil
    val l45= l0:::Cartes.P(Valeur.DAME,Couleur.CARREAU)::Nil
    val l46= l0:::Cartes.P(Valeur.ROI,Couleur.CARREAU)::Nil
    val finale = List(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24,l25,l26,l27,l28,l29,l30,l31,l32,l33,l34,l35,l36,l37,l38,l39,l40,l41,l42,l43,l44,l45,l46)

    toutesPossibilitesPour6V2(l0) shouldBe finale
  }

  "toutes possibilitees pour 5v2" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR))
    toutesPossibilitesPour5V2(l0).length shouldBe 1081
  }

  "toutes possibilitees pour 2v2" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR))
    toutesPossibilitesPour2V2(l0).length shouldBe 2118760
  }

  "proba a partir de 6 cartes d'avoir une paire" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR), Cartes.P(Valeur.CINQ,Couleur.COEUR), Cartes.P(Valeur.SIX,Couleur.COEUR))
    probabilitePaireDans6(l0) shouldBe 0.391304347826087
  }

  "test probabilite" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.COEUR), Cartes.P(Valeur.SIX,Couleur.COEUR))
    probabilite(l0,Hand.Paire) shouldBe 0.391304347826087
  }

  "test probabilite2" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.AS,Couleur.COEUR), Cartes.P(Valeur.DEUX,Couleur.COEUR), Cartes.P(Valeur.TROIS,Couleur.COEUR), Cartes.P(Valeur.QUATRE,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.COEUR), Cartes.P(Valeur.SIX,Couleur.COEUR))
    probabilite(l0,Hand.QuinteFlushRoyale) shouldBe 0
  }

  "test probabilite3" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.DIX,Couleur.CARREAU), Cartes.P(Valeur.VALET,Couleur.CARREAU), Cartes.P(Valeur.DAME,Couleur.CARREAU), Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU))
    probabilite(l0,Hand.Brelan) shouldBe 0
  }

  "test probabilite4" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.DIX,Couleur.CARREAU), Cartes.P(Valeur.VALET,Couleur.CARREAU), Cartes.P(Valeur.DAME,Couleur.CARREAU), Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU),Cartes.P(Valeur.DEUX,Couleur.CARREAU))
    probabilite(l0,Hand.Brelan) shouldBe 0
  }

  "test probabilite5" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.DIX,Couleur.PIQUE), Cartes.P(Valeur.DIX,Couleur.TREFLE), Cartes.P(Valeur.DIX,Couleur.CARREAU), Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU),Cartes.P(Valeur.DEUX,Couleur.CARREAU))
    probabilite(l0,Hand.Carre) shouldBe 0.021739130434782608
  }

  "test probabilite6" should "be defined" in {
    val l0 = List(Cartes.P(Valeur.DIX,Couleur.PIQUE), Cartes.P(Valeur.DIX,Couleur.TREFLE), Cartes.P(Valeur.ROI,Couleur.CARREAU), Cartes.P(Valeur.DAME,Couleur.COEUR),Cartes.P(Valeur.AS,Couleur.CARREAU))
    probabilite(l0,Hand.DoublePaire) shouldBe 0.35892691951896394
  }
}
