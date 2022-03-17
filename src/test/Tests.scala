import Cartes.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import scala.util.Sorting

class Tests extends AnyFlatSpec {

  "BestHand Carre" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.SIX,Couleur.CARREAU))

    val corre = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE))

    isCarre(l) shouldBe true
    bestHand(l).length shouldBe 4
    bestHand(l) shouldBe corre
  }

  "BestHand RoyalFlush" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.DIX,Couleur.CARREAU),Cartes.P(Valeur.VALET,Couleur.CARREAU),Cartes.P(Valeur.DAME,Couleur.CARREAU),Cartes.P(Valeur.ROI,Couleur.CARREAU),Cartes.P(Valeur.AS,Couleur.CARREAU))

    isQuinteFlushRoyale(l,Couleur.CARREAU,-1,0) shouldBe true
    bestHand(l).length shouldBe 5
    bestHand(l) shouldBe l
  }

  "BestHand QuinteFlush" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.HUIT, Couleur.CARREAU), Cartes.P(Valeur.NEUF, Couleur.CARREAU), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.CARREAU))

    isQuinteFlush(l) shouldBe true
    bestHand(l).length shouldBe 5
    bestHand(l) shouldBe l
  }

  "BestHand Full" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.TREFLE),Cartes.P(Valeur.DIX,Couleur.PIQUE),Cartes.P(Valeur.DIX,Couleur.CARREAU))

    isFull(l) shouldBe true
    bestHand(l) shouldBe l
  }

  "BestHand Flush" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.DEUX, Couleur.CARREAU), Cartes.P(Valeur.QUATRE, Couleur.CARREAU), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.CARREAU))

    isFlush(l,0,0,0,0) shouldBe true
    reCoulFlush(l,0,0,0,0) shouldBe Couleur.CARREAU
    bestHand(l).length shouldBe 5
    bestHand(l) shouldBe l
  }

  "BestHand Quinte" should "be defined" in {
    val l = List[Cartes](Cartes.P(Valeur.SEPT, Couleur.CARREAU), Cartes.P(Valeur.HUIT, Couleur.COEUR), Cartes.P(Valeur.NEUF, Couleur.TREFLE), Cartes.P(Valeur.DIX, Couleur.CARREAU), Cartes.P(Valeur.VALET, Couleur.PIQUE))

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
    val l = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.ROI,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.TREFLE),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.SIX,Couleur.CARREAU))

    val corre = List[Cartes](Cartes.P(Valeur.CINQ,Couleur.CARREAU),Cartes.P(Valeur.CINQ,Couleur.PIQUE),Cartes.P(Valeur.ROI,Couleur.COEUR),Cartes.P(Valeur.ROI,Couleur.TREFLE))

    isTwoPair(l,0) shouldBe true
    bestHand(l).length shouldBe 4
    bestHand(l) shouldBe corre
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
}
