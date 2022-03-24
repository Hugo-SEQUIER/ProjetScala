import Cartes.*

object Main {
  def main(args:Array[String]) : Unit = {

    val mesCartes = List(Cartes.P(Valeur.QUATRE,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.COEUR))
    val tableau = List(Cartes.P(Valeur.AS,Couleur.COEUR),Cartes.P(Valeur.DEUX,Couleur.COEUR),Cartes.P(Valeur.TROIS,Couleur.COEUR))
    val cartes3Adversaires = List(Cartes.P(Valeur.SIX,Couleur.COEUR),Cartes.P(Valeur.SEPT,Couleur.COEUR),Cartes.P(Valeur.HUIT,Couleur.COEUR),Cartes.P(Valeur.NEUF,Couleur.COEUR),Cartes.P(Valeur.DIX,Couleur.COEUR),Cartes.P(Valeur.VALET,Couleur.COEUR))

    printf("\n___________________________________________________________________\n")

    printf("\n FONCTION NUMERO 1 \n")

    val l = mesCartes:::tableau
    val besthandPlayer = bestHand(l.sorted(new PlayingCardValue))
    printf("Voici les cartes du joueur : " + afficheValeur(mesCartes) + "\n")
    printf("Voici les cartes à la rivière : " + afficheValeur(tableau) + "\n")
    printf("Voici la meilleur main que le joueur possède : " + afficheValeur(besthandPlayer) + "\n")

    printf("\n___________________________________________________________________\n")
    
    printf("\n FONCTION NUMERO 2 \n")
    val probabilitePaire = probabilite(mesCartes:::tableau,Hand.Paire)
    printf("La probabilité que j'ai une paire à la fin de la partie est de : " + probabilitePaire + "\n")
    val probabiliteDoublePaire = probabilite(mesCartes:::tableau,Hand.DoublePaire)
    printf("La probabilité que j'ai une double paire à la fin de la partie est de : " + probabiliteDoublePaire+"\n")
    val probabiliteBrelan = probabilite(mesCartes:::tableau,Hand.Brelan)
    printf("La probabilité que j'ai un brelan à la fin de la partie est de : " + probabiliteBrelan+"\n")
    
    printf("\n___________________________________________________________________\n")

    printf("\n FONCTION NUMERO 3 \n")

    printf("\n___________________________________________________________________\n")

    printf("\n FONCTION NUMERO 4 \n")

    //ne fonctionne pas mais la logique est là

    val res = fonction4(mesCartes,tableau,cartes3Adversaires)
    printf("La probabilité de gagner à la fin du tirage est de : " + res)

    printf("\n___________________________________________________________________\n")
  }
}
