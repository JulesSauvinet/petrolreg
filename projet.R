install.packages("gdata")
library(gdata) 

#TODO en relatif le chemin du répertoire du projet
setwd("R/projet")
getwd()

#récupération des données du fichier EXCEL
#il faut avoir perl d'installé
datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"))
datapuits

#on transpose les données pour avoir les puits en colonne et les mois en ligne
tdata = setNames(data.frame(t(datapuits[,-1])), datapuits[,1])
tdata

#1) On obtient pas les meme courbes que le prof (bizarre, ça doit venir du traitement des données, j'ai obtenu 
# un truc similaire a un moment donnée mais je sais plus comment - et en meme temps c'est WAHL donc 
# on a peut etre raison nous)

par(mfrow=c(2,3))
#on va tracer 5 graphiques
seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

for (j in seq1){
  for (i in seq2){
    
    #l'abscisse
    mois <- 1:35
    
    #on récupère les données du puits i
    v <- as.vector(tdata[,i])
    
    #la couleur en fonction de la classification de qualité
    col="black"
    if (v[36] == "Good"){
      col = "red"
    }else if (v[36] == "medium"){
      col = "green"
    }else 
      col = "blue"
    
    v <- as.numeric(v[1:35])
    
    #on plot pas les 0 qui sont des ND (d'apres moi)
    nd <- which(v %in% 0)
    #print('V AVANT')
    #print(v)
    v <- v[v != 0]
    #print('V APRES')
    #print(v)

    #print('MOIS AVANT')    
    #print(mois)
    mois <- mois[!mois %in%  nd]
    #print('MOIS APRES')    
    #print(mois)
    
    #tracé de la figure 1 : les données de production
    if (j==0){
      if (i==1){
        plot(mois,v,type="l",col=col, ylab="gas prod", main="Les courbes non fittées",ylim=c(0,max(v)+10))
        #axis(side=2, at=seq(0, 700, by=100))
        #box()
      }else {
        lines(mois,v,type="l",col=col) 
      } 
    }
    #tracé de la figure 2 : les courbes de production obtenues avec des polynômes de degré 2 (et 0, 1, 3, 4)
    else{
      fit2 <- lm(v ~ poly(mois, j, raw=TRUE))
      #summary(fit2)
      if (i==1){
        plot(mois, predict(fit2), type="l",col=col, ylab="gas prod", lwd=1, main=paste("Régression polynomiale de degré ",j), ylim=c(0,max(predict(fit2))+10))
      }else {
        lines(mois, predict(fit2), col=col, ylab="gas prod", lwd=1)
      } 
    }
  }
}


#2) Régression parametrique (exponentielle) 
#valeur pas trop mal avec k0=0.5 et k1=0.1
#k0 fait fluctuer la valeur de départ notamment (plus k0 est grand, plus les courbes sont hautes au départ)
#k1 aplatit les courbes 0.1 (TODO expliquer tout ça)
par(mfrow=c(1,1))
for (i in seq2){
  
  mois <- 1:35
  v <- as.vector(tdata[,i])
  
  col="black"
  if (v[36] == "Good"){
    col = "red"
  }else if (v[36] == "medium"){
    col = "green"
  }else 
    col = "blue"
  
  v <- as.numeric(v[1:35])
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  
  k0=0.5
  k1=0.1
  y=k0*exp(-k1*mois)
  expfit <- lm(v ~ y)
  
  #tracé de la figure 1 : les données de production
  if (i==1){
    plot(mois,predict(expfit),type="l",col=col, ylab="gas prod", main=paste("Régression exponentielle avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(predict(expfit))+10))
    #axis(side=2, at=seq(0, 700, by=100))
    #box()
  }else {
    lines(mois,predict(expfit),type="l",col=col) 
  } 
 
}


#3) Courbe haute + courbe basse à 95%



#4) Suggestions sur 5 courbes mal classées

#5) Gestion des spikes (smoothing curves)


