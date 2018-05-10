

library(deSolve)
library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(crosstalk)
library(rmarkdown)
library(knitLatex)
library(knitr)
library(rootSolve)
library(phaseR)

withMathJax()
# equation du modèle 1 
# Rappelons que y1 est l'amour de 1->2
# y2 y1 est l'amour de 2->1
# m et m2 vallent respectivement 1-R1 et 1-R2
# où R1 et R2 sont les résultats de chacun des partenaire calculés tq : R1 = (TW/35 ) / (TW/35 +1) (Tw : Work time)
# On veut en effet des résultats scolaiers corrélés au temps de travail celui ci pouvant aller jusqu'à 35h/sem 
EqL <- function(t, y, p) { 
  with(as.list(c(y, p)) , {
    dy1 <- y[1]*(y[1]-m)*(1-y[1]) 
    dy2 <- y[2]*(y[2]-m2)*(1-y[2])  
    list(c(Y1= dy1, Y2 = dy2))
  })
}

# EqR : équation des résultats au cours du temps simle parabole de 0 convergent vers 1 avec comme paramètre
# le rapport du de travail soit Tw/Tmax
# Cet évolution des résultats est calculée pour les deux partenaires soit yR1 résultat de 1 (Vos) et résultats 2 yR2 (Ses)
# R1'(t) = Tw / Tmax * (y_1 ) (1- y1)
EqR <- function(t, y, p) {
  with(as.list(c(y, p)) , {
    dyR1 <- pR1*y[1] * (1-(y[1]) ) 
    dyR2 <-  pR2*y[2] * (1-(y[2]) ) 
    
    list(c(YR1= dyR1, YR2 = dyR2))
  })
}

# X en fonction de R, L'amour en fonction des résultats des deux partenaires 
XR <- function (R1 , R2 , J )
{
  X_R  <- c()
  for (i in 1:length(R1))
  {
    X_R [i] = R1[i] - R2[i] *J
  }
  return(X_R)
}

# This model is the same as the first one but parameters of love equation has been replaced 
#by the result of reqult equations (cf : EqR) 



EqL2 <- function(t, y,p) { 
  with(as.list(c(y, p)) , {
    dy1 <- y[1]*(y[1]-1+y[2])*(1-y[1]) 
    dy2 <- y[2]*(y[2]-(1-y[4]))*(1-y[2])  
    dy3 <- (y[3]*(pR1 + y[1]) )* (1-(y[3]) ) 
    dy4 <- (y[4]*(pR2 +y[2]) )* (1-(y[4]) ) 
    list(c(Y1= dy1, Y2 = dy2 , Y3=dy3 , Y4=dy4) )#Y2 = dy2 ,, Y4=dy4
  })
}
# MM modele pour les portraits de phase
EqL2_stab <- function(t, y, parameters) { 
  with(as.list(c(y, parameters)) , {
    dy1 <- y[1]*(y[1]-1+y[2])*(1-y[1]) 
    dy2 <- (y[2]*(parameters[1] + y[1]) )* (1-(y[2]) ) 
    list(c(Y1= dy1,  Y2=dy2 ))
  })
}

# Modèle avec équations différentielles pour les résultats inutile 
#EqR <- function(t, y, parms) {




EqL3 <- function(t, y, p) { 
  with(as.list(c(y, p)) , {
    dy1 <- y[1]*(1-y[1])*(m1_M3-y[1])-E1_M3*y[1] 
    dy2 <- y[2]*(1-y[2])*(m2_M3-y[2])-E2_M3*y[2]  
    list(c(Y1= dy1, Y2 = dy2))
  })
}


# ui : pour interface utilisateur cet environnnement contient l'ensemble des éléments de l'interface : code HTML , variables d'entrées , menus ...
# Les variables entrées dans cette interface peuvent être utilisée dans le server  


ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage("Les mathématiques de l'amour",
             navbarMenu("Modèle 1",
                        
                        tabPanel("Equations amoureuses",
                                 sidebarLayout(
                                 
                                   sidebarPanel(
                                     width = 2 ,
                                     numericInput("TW", label = "Votre temps de travail par semaine (h/sem)", value = 1 , min = 0 , max=35 ),
                                     numericInput("L0" , label = "Votre Amour à l'instant t", value = 1 , min = 0, max=1 ,step=0.1 ),
                                     numericInput("STW", label = "Son temps de travail par semaine (h/sem)", value = 1 , min = 0 , max = 35 ),
                                     numericInput("SL0" , label = "Son Amour à l'instant t", value = 1 , min = 0, max=1 , step=0.1 ),
                                     # Resultats initiaux
                                     numericInput("R", label = "Vos résultats au temps t", value = 10  , min = 0 , max = 20 , step=0.5),
                                     numericInput("SR" , label = "Ses résultats au temps t", value = 10 , min = 0, max=20  , step=0.5 ),
                                     
                                     numericInput("PER" , label = "Nombre de dispute par mois", value = 1, min = 0 , max=20 ,step=1 ),
                                     # Temps sur le quel "tourne le modèle"
                                     sliderInput	("Tm", label="Temps",min = 0, max = 100, value = 50),
                                     
                                     ###### Test Jalousie admmiration JA
                                     sliderInput	("J1", label="votre degré de jalousie",min =0, max = 10, value = 0),
                                     sliderInput	("J2", label="Son degré de jalousie",min =0, max = 10, value = 0)
                                   ),
                                
                                   mainPanel(
                                     width = 10 ,
                                     plotlyOutput("model1" ,height = "900px")
                                    
                                   )
                                   )
                                    
                        
                        ),
                    
                        tabPanel("Annalyse",
                                 #includeHTML("Annalyse_modele1.html")
                                withMathJax(includeMarkdown("Annalyse_modele1.md"),
                                  
                                  tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))
                        )
             ),
       
             navbarMenu("Modèle 2",
                        tabPanel("Equations amoureuses",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     width = 2 ,
                                     numericInput("TW_M2", label = "Votre temps de travail par semaine (h/sem)", value = 1 , min = 0 , max=35 ),
                                     numericInput("L0_M2" , label = "Votre Amour à l'instant t", value = 1 , min = 0, max=1 ,step=0.1 ),
                                     numericInput("R_M2", label = "Vos résultats au temps t", value = 10  , min = 0 , max = 20 , step=0.5),
                                    # sliderInput	("J1_M2", label="Votre degré de jalousie",min =0, max = 10, value = 0),

                                     numericInput("STW_M2", label = "Son temps de travail par semaine (h/sem)", value = 1 , min = 0 , max = 35 ),
                                     numericInput("SL0_M2" , label = "Son Amour à l'instant t", value = 1 , min = 0, max=1 , step=0.1 ),
                                     numericInput("SR_M2" , label = "Ses résultats au temps t", value = 10 , min = 0, max=20  , step=0.5 ),
                                    # sliderInput	("J2_M2", label="Son degré de jalousie",min =0, max = 10, value = 0),

                                     
                                     numericInput("PER_M2" , label = "Nombre de dispute par mois", value = 1, min = 0 , max=20 ,step=1 ),
                                     numericInput("EFF_M2" , label = "Nombre de sortie par mois", value = 1, min = 0 , max=20 ,step=1 ), # Sortie =
                                     # Temps sur le quel "tourne le modèle"
                                     sliderInput	("Tm_M2", label="Temps",min = 0, max = 100, value = 50)
                                     
                                     ###### Test Jalousie admmiration JA
                                     
                                     
                                   ),
                                   mainPanel(
                                     
                                     width = 10 ,
                                     plotlyOutput("model2" ,height = "900px"),
                                     plotOutput("portrait_phase" , height = "150px", width = "150px"),
                                     plotOutput("portrait_phase2" , height = "150px" , width = "150px")
                                     
                                     
                                   )
                                  )
                                 ),
                                   
                                   tabPanel("Annalyse",
                                            #includeHTML("Annalyse_modele1.html")
                                            withMathJax(includeMarkdown("Analyse_modele2.md"),
                                                        
                                                        tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))
                                   )
                        ),
                        
             
             
            navbarMenu("Modèle 3",
                      tabPanel("Equations amoureuses",
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   width = 2 ,
                                   numericInput("TW_M3", label = "Votre temps de travail par semaine (h/sem)", value = 1 , min = 0 , max=35 ),
                                   numericInput("L0_M3" , label = "Votre Amour à l'instant t", value = 1 , min = 0, max=1 ,step=0.1 ),
                                   numericInput("R_M3", label = "Vos résultats au temps t", value = 10  , min = 0 , max = 20 , step=0.5),
                                   sliderInput	("J1_M3", label="Votre degré de jalousie",min =0, max = 10, value = 0),
                                   sliderInput	("E1_M3", label="Votre coefficient d'oubli",min =0, max = 10, value = 10),  # Mal dit 
                                   
                                   numericInput("STW_M3", label = "Son temps de travail par semaine (h/sem)", value = 1 , min = 0 , max = 35 ),
                                   numericInput("SL0_M3" , label = "Son Amour à l'instant t", value = 1 , min = 0, max=1 , step=0.1 ),
                                   numericInput("SR_M3" , label = "Ses résultats au temps t", value = 10 , min = 0, max=20  , step=0.5 ),
                                   sliderInput	("J2_M3", label="Son degré de jalousie",min =0, max = 10, value = 0),
                                   sliderInput	("E2_M3", label="Son coefficient d'oubli",min =0, max = 10, value = 10), 
                                   
                                   
                                   numericInput("PER_M3" , label = "Nombre de dispute par mois", value = 1, min = 0 , max=20 ,step=1 ),
                                   numericInput("EFF_M3" , label = "Nombre de sortie par mois", value = 1, min = 0 , max=20 ,step=1 ), # Sortie =
                                   # Temps sur le quel "tourne le modèle"
                                   sliderInput	("Tm_M3", label="Temps",min = 0, max = 100, value = 50)
                                   
                                   ###### Test Jalousie admmiration JA
                                 
                                   
                                 ),
                                 
                                 mainPanel(
                                
                                   plotlyOutput("model3" ,height = "900px")
                                  
                                 )
                             )
                      
                        ) 
                   )
    )
  )



# Le serveur contient l'ensemble des fonctions et algorithme , attention ON NE PEUT PAS FAIRE DE PRINT OU GARDER EN MEMOIRE LES VARIABLES
# En effet elles dépendent des input

server <- function(input , output){
# Renvoi les graphiques du modèles : soit l'évolution de l'amour de 1->2 et l'évolution de l'amour de 2->1
  output$model1 <- renderPlotly({
################ RECUPERATION DES INPUT ##########################################
    # M et SM vallent respectivement 1-R1 et 1-R2
    # où R1 et R2 sont les résultats de chacun des partenaire calculés tq : R1 = (TW/35 ) / (TW/35 +1) (Tw : Work time)
    # On veut en effet des résultats scolaiers corrélés au temps de travail celui ci pouvant aller jusqu'à 35h/sem 
    # Remarque dans ce modèle les résultats sont fixes tout au long du calcul
    M <- reactive({1-((input$TW/35)/((input$TW/35) +1)) })
    SM <- reactive({1-((input$STW/35)/((input$STW/35) +1))}) 
    # Tm est la valeur maximal de temps récupérée par TMAX, permettant de calculer la fonction sur un intervalle de temps variables
    TMAX <-  reactive({input$Tm })
    # L0 : amour de 1->2 et amour de 2->1  choisit par l'utilisateur. L0 est un vecteur de conditions initiales pour le calcul des solutions sur le 
    # premier intervalle de temps (soit avant une perturbation)
    L0  <- c(Y1 = input$L0, Y2 = input$SL0)
    # parms est le vecteur de paramètres passé à lsoda
    parms <- c(m = M() , m2= SM())
    # per correspond au nombre de disputes par mois choisi par l'utilisateur
    per <- reactive({input$PER})

# Les variables réactives varient selon l'input, elles sont écrites en majuscules (genéralement )  
# Attention pour récupérer la  valeur des variables réactives des parenthèses sont nécessaires ex: mavariable()
    
#######################################################    
  


    # Pour le bien être du mddèle : pour obtenir une fréquence de pertuturbation on divise par 100 le nombre de perturbations. Sinon on se retrourve avec des 
      # pas de temsp nul et lsoda bug
    fq_per <-  reactive({input$PER / 100 }) 
    # On génère un vecteur de perturbation, soit un vecteur de digit tirés dans une loi binomiale. La probabilité de cette loi dépand de la fréquence de perturbation (fq_per)
    vper <- rbinom(TMAX()/0.1 +1 , 1 , fq_per()) 
    # On repère l'index des perturbation, conservés dans le vecteur vintervalle pour vecteur d'intervalle des pas de temps sur lesquels sont
    # calculés les solutions. Ce vecteur sera ensuite utilisé comme une pile
    vintervalle <- c(which(vper==1) )  #,TMAX()/0.1 +1
    if (length(vintervalle) > 1){
    
    # # le premier intervalle calculé est nécessairement compris entre 0 et la valeur du premier index de perturbation*0.1 (Attention 
    # le temps égal l'index*0.1)
    t1 <- seq(0, vintervalle[1]*0.1, by=0.1) 
  
 
   
    psolutionL <- lsoda(y = L0, times=t1, func = EqL, parms)
    # Les solutions de ce premiers intervalle sont conservés dans des vecteus pa1 et  pa2  (p : pour premier et a1  amour de 1, a2 amour de 2)
    pa1<- psolutionL[,2]
    pa2 <- psolutionL[,3]
    # On initialise deux liste vides (A1 et A2) qui seront par la suite des listes de listes contenant les résultats pour chaque tour de boucle 
    # (Ce sera en réalité des listes de listes)

     A1 <- list()
     A2 <- list()
     # Temsp_r (mal nommé) est une liste contenant le temps de chaque intervalle 
    temps_r <- list()
    # A INITIALISER
    L0<- c(psolutionL[,2][length(psolutionL[,2])] , psolutionL[,3][length(psolutionL[,3])])
    # L'algorithme fonctionne sur une pile. Autrement dit à chaque tour de boucle on supprime l'élément N°1 de vecteur_int. L'algorithme s'arrête lorsque 
    # tous les intervalles ont été calculés
    i=0
    while(length(vintervalle) != 1)
    {
      i = i+1
      #  temps_r est donc les pas de temps. En fonctionnant sur une pile on conserve les index étant donné que nous supprimons 
      #à chaque tour la valeur en position 1. Une fois encore le pas de temps est calculé par index_début * 0.1 index_fin*0.1
      
      temps_r <- seq(vintervalle[1]*0.1, vintervalle[2]*0.1, by=0.1) 
      
      # À chaque tour on calcul les solution (c'est le but :)  )
      solutionL <- lsoda(y = L0, times =  temps_r , func = EqL, parms) 
      # On conserve les solutions de lsoda dans deux vecteurs intermédiaires (vA1 et vA2)
      vA1 <- solutionL[,2]
      vA2 <- solutionL[,3]
      # On soustrait une valeur de perturbation sur la dernière valeur de ces vecteurs (bizarement R n'aime pas l'index [-1] donc je prend l'index égal à la longeur du vecteur c'est tordu) . 
      #La perturbation cette valeur correspondantv à la valeur absolue d'un échantillon de taille 1 tiré  dans une loi normale de moyenne 0 et d'écart typer 1 pour limiter les perturbations plus importantes
    
      valeur_perturbation <-abs(rnorm(1,0,0.1))
      Stagnation <- 
      
      vA1[length(vA1)] <-vA1[length(vA1)] - abs(valeur_perturbation)
      vA2[length(vA2)] <-vA2[length(vA2)] - abs(valeur_perturbation)
      # On conserve finalement les deux vecteurs modifiés par la perturbations dans les listes de listes A1 et A2 précedemment initialisée
      A1[[i]] <- vA1
      A2[[i]] <- vA2
      # On prend comme valeur intiale les dernièers solutions modifiées comme conditions initiles 
      L0 <- c(vA1[length(vA1)] , vA2[length(vA2)] )
      # Enfin on élimine le première élément du vecteur vecteur_int
      vintervalle <- vintervalle[-1]
    }
   
    temps_f <- seq( vintervalle * 0.1, length(vper)*0.1,  by=0.1) 
    L0 <- c(vA1[length(vA1)] , vA2[length(vA2)] )
    solutionL_finale <- lsoda(y = L0, times =  temps_f , func = EqL, parms)
    
    a1_dernier_intervalle <- solutionL_finale[,2]
    a2_dernier_intervalle <- solutionL_finale[,3]
    
    # On ajoute les valeurs des solutions calculées sur le premier intervalle au listes de solutions concaténnées issus de la boucle while 
    a1 <-c( pa1  ,  Reduce(c,A1),  a1_dernier_intervalle)
    a2 <- c(pa2 , Reduce(c,A2), a2_dernier_intervalle )
    # le temps final est recalculé par rapport au nombre total de solutions calculées
    # atttention je me suis planté on ne se décale pas de 0.1 entre chaque start 
    tf <- seq(0, length(a1)-1)*0.1
    # ces solutions sont finalement enregistrées dans un data frame pour l'utilisation de ggplot
    df_SolL <- data.frame( Amour1 = c(a1), Amour2  =  c(a2) , Time = c(tf))
    #df_SolL$Amour1[df_SolL$Amour1<0] <- 0
   # df_SolL$Amour2[df_SolL$Amour2<0] <- 0
    
    
    # Calcul des Résultats
      R <- reactive({input$R / 20 })
     SR <- reactive({input$SR /20 }) 
     TR <- seq(0 , TMAX() , by=0.1)
     PR1 <- reactive ({input$TW / 35 })
     PR2 <- reactive ({input$STW / 35 })
  
     tempsR <- seq(0, TMAX(), by=0.1) # Intervalle de temps de 0 à 20 avec un pas de temps de 0.1
     initR <- c(R() , SR() )
     parmsR <- c(pR1 = PR1(), pR2 = PR2()) 
     solutionR <- lsoda(y = initR, times=tempsR, func = EqR, parmsR)
     
    # Calcul de l'amour en fontion des résultats
     
     jalousie1 <- reactive({(input$J1 /10 )}) 
     jalousie2 <- reactive({(input$J2 /10 )}) 
     Amour_resultats1 <- XR(solutionR[,2], solutionR[,3] ,jalousie1())
     Amour_resultats2 <- XR(solutionR[,3], solutionR[,2],jalousie2())
      
    
    p1 <- plot_ly(df_SolL, x = ~Time,  y = ~Amour1, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Amour1, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
      add_trace(y = M() , line = list(color = 'black') )%>%
       layout(xaxis = list(title = "Temps"),#, , domain = c(0,0.5)
        yaxis = list (title = "Amour 1-> 2" ,range=c(-0.5 , 1.2)), #, domain = c(0.66, 1),
        showlegend = FALSE) 
    #xaxis = list(domain = c(0.6, 0.95)),
   # yaxis = list(domain = c(0.6, 0.95))
    
    p2 <- plot_ly(df_SolL, x = ~Time,  y = ~Amour2, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Amour2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
      add_trace(y = SM() , line = list(color = 'black') )%>%
      layout(     
        xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
        yaxis = list (title = "votre amour", range=c(-0.5 , 1.2)),#,  domain = c(0.66 ,1 )
       showlegend = FALSE) 
    
   # p3 <-plot_ly(x = tempsR , z = df_SolL$Amour1[1:length(per)], y = solutionR[,2] , type = 'mesh3d' ,  opacity=0.4 )%>%
     # layout( scene = list(aspectmode='cube') )  
    
    
   # p4 <- plot_ly(x = solutionR[,1],  y = solutionR[,2] , type = 'scatter' , mode='lines') 
   p3 <-  plot_ly(x= solutionR[,1] , y=solutionR[,2], type = 'scatter', mode = 'lines') %>%
     layout( xaxis = list(title=  "Temps"),#, domain= c(0, 0.25)
              yaxis = list (title = "Vos résultats", range=c(0 , 1)),#, domain= c(0.50, 0.33)
              showlegend = FALSE)
   
   p4 <-  plot_ly(x= solutionR[,1] , y=solutionR[,3], type = 'scatter', mode = 'lines') %>%
     layout( xaxis = list(title=  "Temps"),#, domain= c(0.27, 0.5)
             yaxis = list (title = "Sesrésultats",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
             showlegend = FALSE)
    
   
   p5 <- plot_ly(x= solutionR[,2] , y=Amour_resultats1, type = 'scatter', mode = 'lines') %>%
     layout( xaxis = list(title=  "Vos résualtats", range=c(0 , 1) ),#,domain= c(0.5, 0.75)
             yaxis = list (title = "Voter amour ",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
             showlegend = FALSE)
   
   p6 <- plot_ly(x= solutionR[,3] , y=Amour_resultats2, type = 'scatter', mode = 'lines') %>%
     layout( xaxis = list(title=  "Ses résualtats",range=c(0 , 1)),#,  domain= c(0.78, 1)
             yaxis = list (title = "Son amour ",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
             showlegend = FALSE)
    
    p7 <- plot_ly(x = tempsR , z = df_SolL$Amour1[1:length(vper)], y = Amour_resultats1  , type = 'mesh3d' ,  opacity=0.4 , scene='scene') %>%
      layout( scene = list (
            xaxis = list(title = "Temps"),
             zaxis = list (title = "son amour"),
            yaxis = list (title = "Amour en fct des résultats")
             ))
    
    
    p8 <- plot_ly(x = tempsR , z = df_SolL$Amour2[1:length(vper)], y = Amour_resultats2  , type = 'mesh3d' ,  opacity=0.4, scene='scene2' ) %>%
      layout( scene = list (
        xaxis = list(title = "Temps"),
        zaxis = list (title = "Amour 2 -> 1"),
        yaxis = list (title = "Amour en fct des résultats")
      ))
    # Représentation des solutions 
    
  #  g1 = ggplot(df_SolL, aes(x=Time, y=Amour1, colour=Amour1) ) + geom_path(aes(Time, Amour1), lwd=3)+ geom_hline(aes(yintercept = M()) , color = "blue")+ geom_hline(aes(yintercept = 1) , color = "blue")+
   #   scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Temps ", limits=c(0, TMAX())) +scale_y_continuous(name="Amour", limits=c(-1, 1)) 
    
#   g2 =ggplot(df_SolL, aes(x=Time, y=Amour2, colour=Amour2) ) + geom_path(aes(Time, Amour2), lwd=3)+ geom_hline(aes(yintercept = SM()) , color = "blue")+geom_hline(aes(yintercept = 1) , color = "blue")+
 #     scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Temps ", limits=c(0, TMAX())) +scale_y_continuous(name="Amour", limits=c(-1, 1) )   
    
  #  grid.arrange(g1,g2, ncol=2)
    
    }
    else
    {
      t1 <- seq(0, TMAX(), by=0.1) 
      Unique_solutionL <- lsoda(y = L0, times=t1, func = EqL, parms)
      df_SolL <- data.frame( Amour1 = Unique_solutionL[,2], Amour2  = Unique_solutionL[,3] , Time = Unique_solutionL[,1])
      p1 <- plot_ly(df_SolL, x = ~Time,  y = ~Amour1, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Amour1, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        add_trace(y = M() , line = list(color = 'black') )%>%
        layout(xaxis = list(title = "Temps"),#, , domain = c(0,0.5)
               yaxis = list (title = "Amour 1-> 2" ,range=c(-0.5 , 1.2)), #, domain = c(0.66, 1),
               showlegend = FALSE) 
      #xaxis = list(domain = c(0.6, 0.95)),
      # yaxis = list(domain = c(0.6, 0.95))
      
      p2 <- plot_ly(df_SolL, x = ~Time,  y = ~Amour2, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Amour2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        add_trace(y = SM() , line = list(color = 'black') )%>%
        layout(     
          xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
          yaxis = list (title = "votre amour", range=c(-0.5 , 1.2)),#,  domain = c(0.66 ,1 )
          showlegend = FALSE) 
      
      
      p3 <-  plot_ly(x= solutionR[,1] , y=solutionR[,2], type = 'scatter', mode = 'lines') %>%
        layout( xaxis = list(title=  "Temps"),#, domain= c(0, 0.25)
                yaxis = list (title = "Vos résultats", range=c(0 , 1)),#, domain= c(0.50, 0.33)
                showlegend = FALSE)
      
      p4 <-  plot_ly(x= solutionR[,1] , y=solutionR[,3], type = 'scatter', mode = 'lines') %>%
        layout( xaxis = list(title=  "Temps"),#, domain= c(0.27, 0.5)
                yaxis = list (title = "Sesrésultats",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
                showlegend = FALSE)
      
      # l'amour en fonction des résultats 
      p5 <- plot_ly(x= solutionR[,2] , y=Amour_resultats1, type = 'scatter', mode = 'lines') %>%
        layout( xaxis = list(title=  "Vos résualtats", range=c(0 , 1) ),#,domain= c(0.5, 0.75)
                yaxis = list (title = "Voter amour ",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
                showlegend = FALSE)
      
      p6 <- plot_ly(x= solutionR[,3] , y=Amour_resultats2, type = 'scatter', mode = 'lines') %>%
        layout( xaxis = list(title=  "Ses résualtats",range=c(0 , 1)),#,  domain= c(0.78, 1)
                yaxis = list (title = "Son amour ",  range=c(0 , 1)),#, domain= c(0.50, 0.33)
                showlegend = FALSE)
      
      p7 <- plot_ly(x = tempsR , z = df_SolL$Amour1[1:length(vper)], y = Amour_resultats1  , type = 'mesh3d' ,  opacity=0.4 , scene='scene') %>%
        layout( scene = list (
          xaxis = list(title = "Temps"),
          zaxis = list (title = "son amour"),
          yaxis = list (title = "Amour en fct des résultats")
        ))
      
      
      p8 <- plot_ly(x = tempsR , z = df_SolL$Amour2[1:length(vper)], y = Amour_resultats2  , type = 'mesh3d' ,  opacity=0.4, scene='scene2' ) %>%
        layout( scene = list (
          xaxis = list(title = "Temps"),
          zaxis = list (title = "Amour 2 -> 1"),
          yaxis = list (title = "Amour en fct des résultats")
        ))
      # Représentation des solutions 
      
      #  g1 = ggplot(df_SolL, aes(x=Time, y=Amour1, colour=Amour1) ) + geom_path(aes(Time, Amour1), lwd=3)+ geom_hline(aes(yintercept = M()) , color = "blue")+ geom_hline(aes(yintercept = 1) , color = "blue")+
      #   scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Temps ", limits=c(0, TMAX())) +scale_y_continuous(name="Amour", limits=c(-1, 1)) 
      
      #   g2 =ggplot(df_SolL, aes(x=Time, y=Amour2, colour=Amour2) ) + geom_path(aes(Time, Amour2), lwd=3)+ geom_hline(aes(yintercept = SM()) , color = "blue")+geom_hline(aes(yintercept = 1) , color = "blue")+
      #     scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Temps ", limits=c(0, TMAX())) +scale_y_continuous(name="Amour", limits=c(-1, 1) )   
      
      #  grid.arrange(g1,g2, ncol=2)
      
   
      
    }
    subplot(p1, p2 ,p3, p4 , p5 ,p6 ,p7, p8,  nrows = 4,  titleX = TRUE, titleY = TRUE)%>%  #  nrows = 4,
      layout(
        p1 = list(domain=list(x=c(0,0.5),y=c(0.69,1))),
        p2 = list(domain=list(x=c(0.55,1),y=c(0.69,1))),
        p3 = list(domain=list(x=c(0,0.5),y=c(0.48,0.66))),
        p4 = list(domain=list(x=c(0.55,1),y=c(0.48, 0.66))),
        p5 = list(domain=list(x=c(0.5,1),y=c(0.36,0.48))),
        p6 = list(domain=list(x=c(0.55,1),y=c(0.36 ,0.48))),
        scene = list(domain=list(x=c(0,0.55),y=c(0,0.22))),
        scene2 = list(domain=list(x=c(0.55,1),y=c(0,0.22))),
        showlegend=FALSE,showlegend2=FALSE)
    
      
  })
  
 
 # output$Result <- renderPlot({
  #  temps <- seq(0,20, by=0.1) 
   # parmsR <- reactive({input$TWR/35 })
    #parmsSR <-  reactive({input$STWR/35 })
    #parms <- c(parmsR() , parmsSR())
    #R0 <- reactive({input$R0R })
    #SR0 <- reactive({input$SR0R })
    #L0 <- reactive({input$L0R })
    #SL0 <- reactive({input$SL0R })
    
  #  init  <- c(R0(),SL0(),L0(),SL0())
  #  solutionR <- lsoda(y = init, times=temps, func = EqR, parms)
  #  df_SolR <- data.frame(Time = c(temps), solL1 = solutionR[,4] , solL2 = solutionR[,5], solR1 = solutionR[,2]/20 , solR2 = solutionR[,3]/20)  
    #gR1 = ggplot(df_SolR, aes(x=Time, y=solL1, colour=solL1) ) + geom_path(aes(Time, solL1), lwd=3)+#+ geom_hline(aes(yintercept = M()) , color = "blue")+
    #  scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Vos résultats ", limits=c(0, 20)) +scale_y_continuous(name="Amour", limits=c(-1, 2)) 
    #gR2 = ggplot(df_SolR, aes(x=solR2, y=solL2, colour=solL2) ) + geom_path(aes(solR2, solL2), lwd=3)+#+ geom_hline(aes(yintercept = M()) , color = "blue")+
    # scale_colour_gradientn(colours=c(low="blue", high="red")) +   scale_x_continuous(name="Ces résultats ", limits=c(0, 20)) +scale_y_continuous(name="Amour", limits=c(-1, 2)) 
    #grid.arrange(gR1,gR2, ncol=2)   
  #})
  
  
  output$model2 <- renderPlotly({
    
    # Calcul de m1 et m2 (cf : EqL3)
    # m1 équivaut au résultats de 1 et réciproquement pour m2 ou SM 
    M_M2 <- reactive({(input$TW_M2/35)})
    SM_M2 <- reactive({(input$STW_M2/35)}) 
    R_M2 <- reactive({(input$R_M2/20)})
    SR_M2 <- reactive({(input$SR_M2/20)}) 
    
    # Tm est la valeur maximal de temps récupérée par TMAX, permettant de calculer la fonction sur un intervalle de temps variables
    TMAX_M2 <-  reactive({input$Tm_M2 })
    # L0 : amour de 1->2 et amour de 2->1  choisit par l'utilisateur. L0 est un vecteur de conditions initiales pour le calcul des solutions sur le 
    # premier intervalle de temps (soit avant une perturbation)
    L0_M2  <- c(Y1 = input$L0_M2, Y2 = input$SL0_M2, Y3=R_M2() ,Y4=SR_M2())
    # parms est le vecteur de paramètres passé à lsoda
    parms_M2 <- c(pR1 =M_M2() , pR2=SM_M2())
    # per correspond au nombre de disputes par mois choisi par l'utilisateur
    per_M2 <- reactive({input$PER_M2})
    eff_M2 <-reactive({input$EFF_M2})
    
    
    
    # Les variables réactives varient selon l'input, elles sont écrites en majuscules (genéralement )  
    # Attention pour récupérer la  valeur des variables réactives des parenthèses sont nécessaires ex: mavariable()
    
    #######################################################   
    
    # Pour le bien être du mddèle : pour obtenir une fréquence de pertuturbation on divise par 100 le nombre de perturbations. Sinon on se retrourve avec des 
    # pas de temsp nul et lsoda bug
    fq_per_M2 <-  reactive({input$PER_M2 / 100 }) 
    fq_eff_M2 <-  reactive({input$EFF_M2/ 100 }) 
    # On génère un vecteur de perturbation, soit un vecteur de digit tirés dans une loi binomiale. La probabilité de cette loi dépand de la fréquence de perturbation (fq_per)
    vper_M2 <- rbinom(TMAX_M2()/0.1 +1 , 1 , fq_per_M2()) 
    veff_M2 <- rbinom(TMAX_M2()/0.1 +1 , 1 , fq_eff_M2()) 
    # On repère l'index des perturbation, conservés dans le vecteur vintervalle pour vecteur d'intervalle des pas de temps sur lesquels sont
    # calculés les solutions. Ce vecteur sera ensuite utilisé comme une pile 
    vector_int_P_M2 <-c(which(vper_M2==1))  
    vector_int_E_M2 <-c(which(veff_M2==1)) 
    vintervalle_M2 <- sort (c(vector_int_P_M2 , vector_int_E_M2))
    if (length(vintervalle_M2) >1){
    
    t1_M2 <- seq(0, vintervalle_M2[1]*0.1, by=0.1) 
    
    psolutionL_M2 <- lsoda(y = L0_M2, times=t1_M2, func = EqL2, parms_M2)
    # Les solutions de ce premiers intervalle sont conservés dans des vecteus pa1 et  pa2  (p : pour premier et a1  amour de 1, a2 amour de 2)
    pa1_M2<- psolutionL_M2[,2]
    pa2_M2 <- psolutionL_M2[,3]
    pr1_M2<- psolutionL_M2[,4]
    pr2_M2 <- psolutionL_M2[,5]
    # On initialise deux liste vides (A1 et A2) qui seront par la suite des listes de listes contenant les résultats pour chaque tour de boucle 
    # (Ce sera en réalité des listes de listes)
    
    A1_M2 <- list()
    A2_M2 <- list()
    R1_M2 <- list()
    R2_M2 <- list()
    # Temsp_r (mal nommé) est une liste contenant le temps de chaque intervalle 
    temps_r_M2 <- list()
    
    L0_M2<- c(psolutionL_M2[,2][length(psolutionL_M2[,2])] , psolutionL_M2[,3][length(psolutionL_M2[,3])],psolutionL_M2[,4][length(psolutionL_M2[,4])], psolutionL_M2[,5][length(psolutionL_M2[,5])])
    
    # L'algorithme fonctionne sur une pile. Autrement dit à chaque tour de boucle on supprime l'élément N°1 de vecteur_int. L'algorithme s'arrête lorsque 
    # tous les intervalles ont été calculés
    i=0
    while(length(vintervalle_M2) != 1)
    {
      i = i+1
      #  temps_r est donc les pas de temps. En fonctionnant sur une pile on conserve les index étant donné que nous supprimons 
      #à chaque tour la valeur en position 1. Une fois encore le pas de temps est calculé par index_début * 0.1 index_fin*0.1
      
      temps_r_M2 <- seq(vintervalle_M2[1]*0.1, vintervalle_M2[2]*0.1, by=0.1) 
      
      # À chaque tour on calcul les solution (c'est le but :)  )
      solutionL_M2 <- lsoda(y = L0_M2, times =  temps_r_M2 , func = EqL2, parms_M2) 
      # On conserve les solutions de lsoda dans deux vecteurs intermédiaires (vA1 et vA2)
      vA1_M2 <- solutionL_M2[,2]
      vA2_M2 <- solutionL_M2[,3]
      vR1_M2 <- solutionL_M2[,4]
      vR2_M2 <- solutionL_M2[,5]
      # On soustrait une valeur de perturbation sur la dernière valeur de ces vecteurs (bizarement R n'aime pas l'index [-1] donc je prend l'index égal à la longeur du vecteur c'est tordu) . 
      #La perturbation cette valeur correspondantv à la valeur absolue d'un échantillon de taille 1 tiré  dans une loi normale de moyenne 0 et d'écart typer 1 pour limiter les perturbations plus importantes
      
      valeur_perturbation_M2 <-abs(rnorm(1,0,0.1))
      #  Stagnation <-  A voir
      # Condition sur le signe de la pertrbation si le pas de temps correspond à un effort ou une pertubation
      if (is.element(vintervalle_M2[2], vector_int_P_M2 ) == TRUE)
      {
        vA1_M2[length(vA1_M2)] <-vA1_M2[length(vA1_M2)] - valeur_perturbation_M2
        vA2_M2[length(vA2_M2)] <-vA2_M2[length(vA2_M2)] -valeur_perturbation_M2
      }
      else
      {
        vA1_M2[length(vA1_M2)] <-vA1_M2[length(vA1_M2)] + valeur_perturbation_M2
        vA2_M2[length(vA2_M2)] <-vA2_M2[length(vA2_M2)] + valeur_perturbation_M2
      }
      # On conserve finalement les deux vecteurs modifiés par la perturbations dans les listes de listes A1 et A2 précedemment initialisée
      A1_M2[[i]] <- vA1_M2
      A2_M2[[i]] <- vA2_M2
      R1_M2[[i]] <- vR1_M2
      R2_M2[[i]] <- vR2_M2
      # On prend comme valeur intiale les dernièers solutions modifiées comme conditions initiles 
      L0_M2 <- c(vA1_M2[length(vA1_M2)] , vA2_M2[length(vA2_M2)],  vR1_M2[length(vR1_M2)],   vR2_M2[length(vR2_M2)] )
      # Enfin on élimine le première élément du vecteur vecteur_int
      vintervalle_M2 <- vintervalle_M2[-1]
    }
    tf_M2 <- seq (vintervalle_M2[1]*0.1 , length(vper_M2)*0.1 , 0.1)
    L0_M2 <- c(vA1_M2[length(vA1_M2)] , vA2_M2[length(vA2_M2)],  vR1_M2[length(vR1_M2)],   vR2_M2[length(vR2_M2)] )
    solutionL_f_M2 <- lsoda(y = L0_M2, times =  tf_M2 , func = EqL2, parms_M2) 
    
    a1_M2 <-c( pa1_M2  ,  Reduce(c,A1_M2),  solutionL_f_M2[,2]) #
    a2_M2 <- c(pa2_M2 , Reduce(c,A2_M2), solutionL_f_M2[,3]) #
    r1_M2 <-c( pr1_M2  ,  Reduce(c,R1_M2),  solutionL_f_M2[,4]) #
    r2_M2 <- c(pr2_M2 , Reduce(c,R2_M2), solutionL_f_M2[,5])
    temps_M2 <- seq(0, (length(a1_M2)-1)*0.1, 0.1)
    
    df_SolL_M2 <- data.frame( Amour1_M2 = c(a1_M2), Resultat1_M2 = c(r1_M2), Amour2_M2  =  c(a2_M2), Resultat2_M2 = c(r2_M2),  Time_M2 = c(temps_M2))
    
    p1_M2 <- plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Amour1_M2, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Amour1_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
     # add_trace(y = M_M2() , line = list(color = 'black') )%>%
      layout(xaxis = list(title = "Temps"),#, , domain = c(0,0.5)
             yaxis = list (title = "Amour 1-> 2" ,range=c(-0.5 , 1.2)), #, domain = c(0.66, 1),
             showlegend = FALSE) 
  
    
    p2_M2<- plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Amour2_M2, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Amour2_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
     # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
      layout(     
        xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
        yaxis = list (title = "votre amour", range=c(-0.5 , 1.2)),#,  domain = c(0.66 ,1 )
        showlegend = TRUE)
    
    
    p3_M2 <- plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Resultat1_M2, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Resultat1_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
      # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
      layout(     
        xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
        yaxis = list (title = "Vos résultats",  range=c(0, 1)),#,  domain = c(0.66 ,1 )
        showlegend = FALSE) 
    
    p4_M2 <-  plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Resultat2_M2, type = 'scatter', mode = 'lines' )%>%
      add_trace(   y = ~Resultat2_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
      # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
      layout(     
        xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
        yaxis = list (title = "Ses résultats", range=c(0, 1)),#,  domain = c(0.66 ,1 )
        showlegend = FALSE) 
   # x<-seq(1,100,1)
  #y <- rnorm(100 , 0, 1)
   # y2 <- rnorm(100 , 1, 1)
    #ds <- data.frame(x=c(x), y=c(y) , y2<-c(y2))
    #p1_M2 <- plot_ly(ds, x = ~x,  y = ~y, type = 'scatter', mode = 'lines' )%>%
    # add_trace( y = ~y, type = "scatter")
    #p2_M2 <- plot_ly(ds, x = ~x,  y = ~y2, type = 'scatter', mode = 'lines' )%>%
    #  add_trace( y = ~y2, type = "scatter")
    }
    else{
      t1_M2 <- seq(0, TMAX_M2()*0.1, by=0.1) 
      
      psolutionL_M2 <- lsoda(y = L0_M2, times=t1_M2, func = EqL2, parms_M2)
      df_SolL_M2 <- data.frame( Amour1_M2 = psolutionL_M2[,2], Resultat1_M2 = psolutionL_M2[,4], Amour2_M2  =  psolutionL_M2[,3], Resultat2_M2 = psolutionL_M2[,4],  Time_M2 = psolutionL_M2[,1])
      
      p1_M2 <- plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Amour1_M2, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Amour1_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        # add_trace(y = M_M2() , line = list(color = 'black') )%>%
        layout(xaxis = list(title = "Temps"),#, , domain = c(0,0.5)
               yaxis = list (title = "Amour 1-> 2" ,range=c(-0.5 , 1.2)), #, domain = c(0.66, 1),
               showlegend = FALSE) 
      #xaxis = list(domain = c(0.6, 0.95)),
      # yaxis = list(domain = c(0.6, 0.95))
      
      p2_M2<- plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Amour2_M2, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Amour2_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
        layout(     
          xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
          yaxis = list (title = "votre amour", range=c(-0.5 , 1.2)),#,  domain = c(0.66 ,1 )
          showlegend = FALSE) 
      
      p3_M2 <-  plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Resultat1_M2, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Resultat1_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
        layout(     
          xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
          yaxis = list (title = "Vos résultats", range=c(0, 1)),#,  domain = c(0.66 ,1 )
          showlegend = FALSE) 
      
      
      p4_M2 <-   plot_ly(df_SolL_M2, x = ~Time_M2,  y = ~Resultat2_M2, type = 'scatter', mode = 'lines' )%>%
        add_trace(   y = ~Resultat2_M2, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
        # add_trace(y = SM_M2() , line = list(color = 'black') )%>%
        layout(     
          xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
          yaxis = list (title = "Vos résultats", range=c(0, 1)),#,  domain = c(0.66 ,1 )
          showlegend = FALSE) 
      
    }
    subplot(p1_M2, p2_M2 , p3_M2, p4_M2, nrows = 2,  titleX = TRUE, titleY = TRUE )
    
    
    
  })
  
  output$portrait_phase <- renderPlot({
    # Paramètre des équations de R
    M_M22 <- reactive({(input$TW_M2/35)})
    R_M22<- reactive({(input$R_M2/20)})
    # Conditions initioales de l'équation des sentiments
    L0_M22  <- c(Y1 = input$L0_M2,  Y2=R_M22() )
    # # parms est le vecteur de paramètres passé à lsoda
    pR1 <- M_M22()
    
   
    
    flowField(deriv = EqL2_stab , x.lim = c(0,1.3), y.lim = c(0,1),  parameters= c(pr1) , points = 13 , add = FALSE, xlab="Amour" , ylab="Résultats")
    nullclines(deriv = EqL2_stab , x.lim = c(-1,1.5), y.lim = c(-1,1.5),  parameters= c(pr1) , points = 501 , system = "two.dim" ,  colour=c("darkblue", "aquamarine") , add=TRUE)
    trajectory(deriv = EqL2_stab,  y0 = L0_M22, t.end = 50,   parameters=c(pr1) , add=TRUE)
  })  
  
  
  output$portrait_phase2 <- renderPlot({
    # Paramètre des équations de R
    SM_M22 <- reactive({(input$STW_M2/35)})
    SR_M22<- reactive({(input$SR_M2/20)})
    # Conditions initioales de l'équation des sentiments
    SL0_M22  <- c(Y1 = input$SL0_M2,  Y2=SR_M22() )
    # # parms est le vecteur de paramètres passé à lsoda
    pR2 <- SM_M22()
    
    
    
    flowField(deriv = EqL2_stab , x.lim = c(0,1.3), y.lim = c(0,1),  parameters= c(pR2) , points = 13 , add = FALSE, xlab="Amour" , ylab="Résultats")
    nullclines(deriv = EqL2_stab , x.lim = c(-1,1.5), y.lim = c(-1,1.5),  parameters= c(pR2) , points = 501 , system = "two.dim" ,  colour=c("darkblue", "aquamarine") , add=TRUE)
    trajectory(deriv = EqL2_stab,  y0 = SL0_M22, t.end = 50,   parameters=c(pR2) , add=TRUE)
  })  
  
  output$model3 <- renderPlotly({
    
    # Calcul de m1 et m2 (cf : EqL3)
    # m1 équivaut au résultats de 1 et réciproquement pour m2 ou SM 
    M_M3 <- reactive({(input$TW_M3/35)})
    SM_M3 <- reactive({(input$STW_M3/35)}) 
    # Coefficient d'oubli E1 et E2 
    E1_M3 <- reactive({(input$E1_M3/10)})
    E2_M3 <- reactive({(input$E2_M3/10)})
    # Calcul du minimun de E pour avoir des sollutions réelles 
    if (E1_M3() > (-0.25*(-M_M3()-1)^2+M_M3()) )
    {
      # Autrement dit E1 = E1
      e1 <-reactive({E1_M3()})
    }
    else 
    {
      e1 <- reactive( {(-0.25*(-M_M3()-1)^2+M_M3())})
    }
    # Idem pour 2
    
   if (E2_M3() > (-0.25*(-M_M3()-1)^2+M_M3()))
    {
      # Autrement dit E1 = E1
      e2 <-reactive({E2_M3()})
    }
    else 
    {
      e2 <- reactive( {-0.25*(-M_M3()-1)^2+M_M3() })
    }
    
    
    
    # Tm est la valeur maximal de temps récupérée par TMAX, permettant de calculer la fonction sur un intervalle de temps variables
    TMAX_M3 <-  reactive({input$Tm_M3 })
    # L0 : amour de 1->2 et amour de 2->1  choisit par l'utilisateur. L0 est un vecteur de conditions initiales pour le calcul des solutions sur le 
    # premier intervalle de temps (soit avant une perturbation)
    L0_M3  <- c(Y1 = input$L0_M3, Y2 = input$SL0_M3)
    # parms est le vecteur de paramètres passé à lsoda
    parms_M3 <- c(m1_M3 = M_M3() , m2_M3= SM_M3() , E1_M3= e1() , E2_M3= e2())
    # per correspond au nombre de disputes par mois choisi par l'utilisateur
    per_M3 <- reactive({input$PER_M3})
    eff_M3 <-reactive({input$EFF_M3})
    
    
    
    # Les variables réactives varient selon l'input, elles sont écrites en majuscules (genéralement )  
    # Attention pour récupérer la  valeur des variables réactives des parenthèses sont nécessaires ex: mavariable()
    
    #######################################################   
    
    # Pour le bien être du mddèle : pour obtenir une fréquence de pertuturbation on divise par 100 le nombre de perturbations. Sinon on se retrourve avec des 
    # pas de temsp nul et lsoda bug
    fq_per_M3 <-  reactive({input$PER_M3 / 100 }) 
    fq_eff_M3 <-  reactive({input$EFF_M3/ 100 }) 
    # On génère un vecteur de perturbation, soit un vecteur de digit tirés dans une loi binomiale. La probabilité de cette loi dépand de la fréquence de perturbation (fq_per)
    vper_M3 <- rbinom(TMAX_M3()/0.1 +1 , 1 , fq_per_M3()) 
    veff_M3 <- rbinom(TMAX_M3()/0.1 +1 , 1 , fq_eff_M3()) 
    # On repère l'index des perturbation, conservés dans le vecteur vintervalle pour vecteur d'intervalle des pas de temps sur lesquels sont
    # calculés les solutions. Ce vecteur sera ensuite utilisé comme une pile 
    vector_int_P_M3 <-c(which(vper_M3==1))  
    vector_int_E_M3 <-c(which(veff_M3==1)) 
    vintervalle_M3 <- sort (c(vector_int_P_M3 , vector_int_E_M3))
  
    t1_M3 <- seq(0, vintervalle_M3[1]*0.1, by=0.1) 
    
    psolutionL_M3 <- lsoda(y = L0_M3, times=t1_M3, func = EqL3, parms_M3)
    # Les solutions de ce premiers intervalle sont conservés dans des vecteus pa1 et  pa2  (p : pour premier et a1  amour de 1, a2 amour de 2)
    pa1_M3<- psolutionL_M3[,2]
    pa2_M3 <- psolutionL_M3[,3]
    # On initialise deux liste vides (A1 et A2) qui seront par la suite des listes de listes contenant les résultats pour chaque tour de boucle 
    # (Ce sera en réalité des listes de listes)
    
    A1_M3 <- list()
    A2_M3 <- list()
    # Temsp_r (mal nommé) est une liste contenant le temps de chaque intervalle 
    temps_r_M3 <- list()
    
    L0_M3<- c(psolutionL_M3[,2][length(psolutionL_M3[,2])] , psolutionL_M3[,3][length(psolutionL_M3[,3])])
    
    # L'algorithme fonctionne sur une pile. Autrement dit à chaque tour de boucle on supprime l'élément N°1 de vecteur_int. L'algorithme s'arrête lorsque 
    # tous les intervalles ont été calculés
    i=0
    while(length(vintervalle_M3) != 1)
    {
      i = i+1
      #  temps_r est donc les pas de temps. En fonctionnant sur une pile on conserve les index étant donné que nous supprimons 
      #à chaque tour la valeur en position 1. Une fois encore le pas de temps est calculé par index_début * 0.1 index_fin*0.1
      
      temps_r_M3 <- seq(vintervalle_M3[1]*0.1, vintervalle_M3[2]*0.1, by=0.1) 
      
      # À chaque tour on calcul les solution (c'est le but :)  )
      solutionL_M3 <- lsoda(y = L0_M3, times =  temps_r_M3 , func = EqL3, parms_M3) 
      # On conserve les solutions de lsoda dans deux vecteurs intermédiaires (vA1 et vA2)
      vA1_M3 <- solutionL_M3[,2]
      vA2_M3 <- solutionL_M3[,3]
      # On soustrait une valeur de perturbation sur la dernière valeur de ces vecteurs (bizarement R n'aime pas l'index [-1] donc je prend l'index égal à la longeur du vecteur c'est tordu) . 
      #La perturbation cette valeur correspondantv à la valeur absolue d'un échantillon de taille 1 tiré  dans une loi normale de moyenne 0 et d'écart typer 1 pour limiter les perturbations plus importantes
      
      valeur_perturbation_M3 <-abs(rnorm(1,0,0.1))
    #  Stagnation <-  A voir
     # Condition sur le signe de la pertrbation si le pas de temps correspond à un effort ou une pertubation
      if (is.element(vintervalle_M3[2], vector_int_P_M3 ) == TRUE)
      {
        vA1_M3[length(vA1_M3)] <-vA1_M3[length(vA1_M3)] - valeur_perturbation_M3
        vA2_M3[length(vA2_M3)] <-vA2_M3[length(vA2_M3)] -valeur_perturbation_M3
      }
      else
      {
       vA1_M3[length(vA1_M3)] <-vA1_M3[length(vA1_M3)] + valeur_perturbation_M3
       vA2_M3[length(vA2_M3)] <-vA2_M3[length(vA2_M3)] + valeur_perturbation_M3
      }
      # On conserve finalement les deux vecteurs modifiés par la perturbations dans les listes de listes A1 et A2 précedemment initialisée
      A1_M3[[i]] <- vA1_M3
      A2_M3[[i]] <- vA2_M3
      # On prend comme valeur intiale les dernièers solutions modifiées comme conditions initiles 
      L0_M3 <- c(vA1_M3[length(vA1_M3)] , vA2_M3[length(vA2_M3)] )
      # Enfin on élimine le première élément du vecteur vecteur_int
      vintervalle_M3 <- vintervalle_M3[-1]
    }
    tf_M3 <- seq (vintervalle_M3[1]*0.1 , length(vper_M3)*0.1 , 0.1)
    L0_M3 <- c(vA1_M3[length(vA1_M3)] , vA2_M3[length(vA2_M3)] )
    solutionL_f_M3 <- lsoda(y = L0_M3, times =  tf_M3 , func = EqL3, parms_M3) 
    
   a1_M3 <-c( pa1_M3  ,  Reduce(c,A1_M3),  solutionL_f_M3[,2]) #
   a2_M3 <- c(pa2_M3 , Reduce(c,A2_M3), solutionL_f_M3[,3]) #
   temps_M3 <- seq(0, (length(a1_M3)-1)*0.1, 0.1)
    
   df_SolL_M3 <- data.frame( Amour1_M3 = c(a1_M3), Amour2_M3  =  c(a2_M3) , Time_M3 = c(temps_M3))
    
   p1_M3 <- plot_ly(df_SolL_M3, x = ~Time_M3,  y = ~Amour1_M3, type = 'scatter', mode = 'lines' )%>%
     add_trace(   y = ~Amour1_M3, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
     add_trace(y = M_M3() , line = list(color = 'black') )%>%
     layout(xaxis = list(title = "Temps"),#, , domain = c(0,0.5)
            yaxis = list (title = "Amour 1-> 2" ,range=c(-0.5 , 1.2)), #, domain = c(0.66, 1),
            showlegend = FALSE) 
   #xaxis = list(domain = c(0.6, 0.95)),
   # yaxis = list(domain = c(0.6, 0.95))
   
   p2_M3 <- plot_ly(df_SolL_M3, x = ~Time_M3,  y = ~Amour2_M3, type = 'scatter', mode = 'lines' )%>%
     add_trace(   y = ~Amour2_M3, type = "scatter"  , marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(200)) , mode = 'markers', showlegend = FALSE  )%>%
     add_trace(y = SM_M3() , line = list(color = 'black') )%>%
     layout(     
       xaxis = list(title = "Temps"),# , domain = c(0.5, 1)
       yaxis = list (title = "votre amour", range=c(-0.5 , 1.2)),#,  domain = c(0.66 ,1 )
       showlegend = FALSE) 
   
  
   subplot(p1_M3, p2_M3 )
   
   
   
  })
  
  
  
    
}



shinyApp(ui = ui , server = server)

