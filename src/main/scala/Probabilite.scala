import Cartes.*

//Probabilités d'obtenir une certaine main en fonction du nombre de joueurs et des outs lorsqu'aucune carte n'est sortie
def probaFromNothing(outs: Int, nbJoueurs: Int): Int = (outs/52)+(outs/(52-nbJoueurs))+(outs/(52-(2*nbJoueurs)))+(outs/(52-(1+2*nbJoueurs)))+(outs/(52-(2+2*nbJoueurs)))+(outs/(52-(3+2*nbJoueurs)))+(outs/(52-(4+2*nbJoueurs)))-((outs/52)*(outs/(52-nbJoueurs))*(outs/(52-(2*nbJoueurs)))*(outs/(52-(1+2*nbJoueurs)))*(outs/(52-(2+2*nbJoueurs)))*(outs/(52-(3+2*nbJoueurs)))*(outs/(52-(4+2*nbJoueurs))))

//Probabilités d'obtenir une certaine main en fonction du nombre de joueurs et des outs lorsque 0 cartes ouvertes et 2 cartes fermées sont sorties
def probaFromClose(outs: Int, nbJoueurs: Int): Int = (outs/(52-(2*nbJoueurs)))+(outs/(52-(1+2*nbJoueurs)))+(outs/(52-(2+2*nbJoueurs)))+(outs/(52-(3+2*nbJoueurs)))+(outs/(52-(4+2*nbJoueurs)))-((outs/(52-(2*nbJoueurs)))*(outs/(52-(1+2*nbJoueurs)))*(outs/(52-(2+2*nbJoueurs)))*(outs/(52-(3+2*nbJoueurs)))*(outs/(52-4+2*nbJoueurs)))

//Probabilités d'obtenir une certaine main en fonction du nombre de joueurs et des outs lorsque 3 cartes ouvertes et 2 cartes fermées sont sorties
def probaFromFlop(outs: Int, nbJoueurs: Int): Int = (outs/(52-(3+2*nbJoueurs)))+(outs/(52-(4+2*nbJoueurs)))-((outs/(52-(3+2*nbJoueurs)))*(outs/(52-(4+2*nbJoueurs))))

//Probabilités d'obtenir une certaine main en fonction du nombre de joueurs et des outs lorsque 4 cartes ouvertes et 2 cartes fermées sont sorties
def probaFromTurn(outs: Int, nbJoueurs: Int): Int = outs/(52-(4+2*nbJoueurs))