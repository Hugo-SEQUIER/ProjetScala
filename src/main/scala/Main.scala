import Cartes.*

object Main {
  def main(args:Array[String]) : Unit = {

    val mesCartes = List(Cartes.P(Valeur.QUATRE,Couleur.COEUR),Cartes.P(Valeur.CINQ,Couleur.COEUR))
    val tableau = List(Cartes.P(Valeur.AS,Couleur.COEUR),Cartes.P(Valeur.DEUX,Couleur.COEUR),Cartes.P(Valeur.TROIS,Couleur.COEUR))

    printf("\n___________________________________________________________________\n")

    printf("\n FONCTION NUMERO 1 \n")

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

    printf("\n___________________________________________________________________\n")
  }
}
