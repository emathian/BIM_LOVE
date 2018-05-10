---
title: "Annalyse_modèle2"
output:
  html_document: default
  pdf_document: default
---

## Analyse du modèle 2
Le second modèle reprend le modèle 1, mais lie explicitement les sentiments en fonctions des résultats et réciproquement les résultats aux sentiments. Selon des hypothèses équivalentes nous modélisons lévolution des sentiments au cours du temps grâce aux systèmes:

\[
\left\{
\begin{array}{r c l}
X_1^\prime = X_1(X_1-1+R_1)(1-X_1) + \epsilon \\
R_1^\prime =R_1(\frac{Tw}{T_{max}} + X_1)(1-R_1)
\end{array}
\right.
\]

\[
\left\{
\begin{array}{r c l}
X_2^\prime = X_2(X_2-1+R_2)(1-X_2) + \epsilon \\
R_2^\prime =R_2(\frac{Tw}{T_{max}}+X_2)(1-R_2)
\end{array}
\right.
\]

### Paramètres et interprétation

L'équation de l'amour de 1->2 et de 2->1 est de même forme générale que celles du modèle 1. Néanmoins les équations des sentiments sont directement liées à celles des résultats. En outre les résultats sont eux aussi liée aux sentiments. Ainsi les résultats croissent au cours du temps pour tendre vers la note maximal et ceux d'autant plus vite que le rapport du temps de travail est élévé et que l'amour pour son partenaire augmente, en somme être amoureux est un moteur intelectuel ! Les sentiments quand à eux tendent vers la passion lorsque les si l'amour initial est supérieur à $1-R_{0,i}$ et s'estompent d'autant plus lentement que les résultats augmeentent. Ainsi il est nécéssaire d'avoir des résultats minimaux pour maintenir une relation amoureuse. L'intérêt de coupler les équation de X et de R est augmentre la pérénité de la relation étant donné que d'après nos hypothèse pour un travail constant les résultats augmentent.  
<!-- C'est assez idilique comme truc ...!!! -->

Comme précédemment le modèle proposé est stochastique étant donné que des disputes peuvent avoir lieu. Contrairement au modèle 1 nous avons également modélisé des efforts au sein du couple, ceux-ci sont choisis par l'utilisateur grâce au paramètre *nombre de sorties par mois*. Nous générons deux vecteurs aléatoire tirés dans une lois binomiale de paramètre $p1=\text{fréquence des disputes}$ et respectivement $p2=\text{fréquence de sorties}$. Pour chaque temps où la valeurs de ces vecteurs est différente de 0 la valeur de $\epsilon$ est tirées dans une lois normale de paramètre $\mathcal{N}~>(0,0.1)$.
Nous obtenons le modèle suivant :

\[
\left\{
\begin{array}{r c l}
Dispute \sim B ( n , p1) \quad \text{où  n= Nombre de pas de temps et p1 = frequece de disputes }\\
Effort \sim B ( n , p2) \quad \text{où  n= Nombre de pas de temps et p2 = frequece de sorties }\\
\end{array}
\right.
\]
<br>
\[
\left\{
\begin{array}{r c l}
\text{Si Dispute ou Effort = 0} \qquad \epsilon = 0 \\
\text{Sinon } valeur_perturbation \sim N (0,0.1)\\
\text{Signe de la pertubation : si pertubation = Dispute} \quad \epsilon = -|valeur perturbation|\\
\text{Signe de la pertubation : sinon} \quad \epsilon = +|valeur perturbation|\\
\end{array}
\right.
\]



Les paramètre de cet EDO sont m1 et m2, qui équivalent à ($1-\frac{ Tw_i}{Tmax}$), où $Tw_i$ est le temps de travail du partenaire$_i$ à l'instant $t=0$. $Tmax$ est une estimation du temps de travail maximal pouvant être réalisé en une semaine, ici "arbitrairement" fixé à $Tmax=35 h$. Ainsi définit $m_i\in[0,1]$.

Le modèle proposé ci-dessus est stochastique, en effet on fait ici l'hypothèse que les relations amoureuses sont perturbées par des disputes. L'utilisateur choisit ainsi la fréquence de disputes (*Nombre de dispute par mois*). À partir de cette fréquence  l'algorithme génére un échantillon alétoire tiré dans une lois binomiale, de taille équivalent au nombre de pas de temps. Pour chaque occurence de 1 une dispute est modélisée, lors d'une dispute $\epsilon \neq 0$. L'ampleur de la dispute est quant à elle générée par tirage dans une loi normale centre d'écart type $0.1$. On obtient finalement :



### Équilibres

En résolvant le système et pour $\fract{Tw}{T_{max}} \in [0,1] $ :
\[
\left\{
\begin{array}{r c l}
X_1^\prime = X_1(X_1-1+R_1)(1-X_1) + \epsilon \\
R_1^\prime =R_1(\frac{Tw}{T_{max}} + X_1)(1-R_1)
\end{array}
\right.
\]
Nous obtenons quatre équilibres : (0,0) , (1,0) , (0,1) et (1,1)
Posons les fonctiions suivantes:
$$f(X_1)= X_1(X_1-1+R_1)(1-X_1) $$
$$ g(R_1) =R_1(\frac{Tw}{T_{max}} + X_1)(1-R_1) $$
Déterminons la matrice jacobienne : 
$$
\begin{equation}
J = 
\begin{pmatrix} 
\frac{\partial f}{\partial X} & \frac{\partial f}{\partial R}  \\
\frac{\partial g}{\partial X} & \frac{\partial g}{\partial R} 
\end{pmatrix}
\end{equation}
$$
J pour notre système équivaut à :
$$
\begin{equation}
J = 
\begin{pmatrix} 
2X-1+R-3X^2+2X-2XR & X-X^2  \\
R-R^2 & \frac{Tw}{T_{max}}+X-2R\frac{Tw}{T_{max}} -2XR 
\end{pmatrix}
\end{equation}
$$
**Nature de l'équilibre au point (0,0) :**
$$
\begin{equation}
J_{1,0} = 
\begin{pmatrix} 
-1 &0  \\
0& \frac{Tw}{T_{max}}
\end{pmatrix}
\end{equation}
$$
$$Tr = -1 + \frac{Tw}{T_{max}}+1 < 0$$ 
où si $\frac{Tw}{T_{max}}$  $Tr=0$
$$ Det <0$$
Le point d'équilibre est un point selle.
**Nature de l'équilibre au point (1,0) :**
$$
\begin{equation}
J_{1,0} = 
\begin{pmatrix} 
0 &0  \\
0& \frac{Tw}{T_{max}}+1
\end{pmatrix}
\end{equation}
$$
$$Tr =  \frac{Tw}{T_{max}}+1 >0$$
$$ Det = 0$$
La linéarisation prévoit des centres.
<!-- A démontrer -->
<br>
**Nature de l'équilibre au point (0,1) :**
$$
\begin{equation}
J_{0,1} = 
\begin{pmatrix} 
0 &0  \\
0& -\frac{Tw}{T_{max}}
\end{pmatrix}
\end{equation}
$$
$$Tr =  -\frac{Tw}{T_{max}}<0$$
$$Det = 0$$
Le point d'équilibre est un point selle.
<br>
**Nature de l'équilibre au point (1,1) :**
$$
\begin{equation}
J_{1,1} = 
\begin{pmatrix} 
-1 &0  \\
0& -\frac{Tw}{T_{max}}-1
\end{pmatrix}
\end{equation}
$$
$$Tr =  -\frac{Tw}{T_{max}}<0$$
$$Det =\frac{Tw}{T_{max}}>0 $$
Le point d'équilibre est un noeud asymptotiquement stable.
<br>
**Isoclines verticales : **
Les isoclines verticales vérifient $X^\prime = 0$ donc $X(X-1+R)(1-X)=0$. On obtient ainsi les droites d'équations $X=0$ , $X=1$ et $R = 1-X$
<!-- R = 1-X et surtout égale à pas sur du tout !!-->
<br>
**Isoclines horizontale : **
Les isoclines horizontales vérifient $R^\prime = 0$ donc $R(\frac{Tw}{T_{max}} + X)(1-R)=0$. On obtient ainsi les droites d'équations $R=0$ , $R=1$ et $X=-\frac{Tw}{T_{max}}$ (non étudié étant donné que $\frac{Tw}{T_{max}} \in [0,1]$).
<br>
**Portrait de phase **
<br>
![](/Users/mathian/Desktop/INSA_L3_S2/BIM_SHINY/PORTRAIT_PHASE.png)!
