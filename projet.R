install.packages("gdata")
install.packages("nls2")
install.packages("boot")
install.packages("kernlab")
library(nnet)
library(nls2)
library(gdata) 
library(boot)
library(kernlab)


setwd("documents\\M2\\regression\\projet\\petrolreg")
getwd()

#récupération des données du fichier EXCEL
perl <- 'C:\\Strawberry\\perl\\bin\\perl.exe'
datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"), perl=perl)

#on transpose les données pour avoir les puits en colonne et les mois en ligne
puits = setNames(data.frame(t(datapuits[,-1])), datapuits[,1])
puits

##################################################
##   QUESTION   1   -   Régressions polynomiales##
##################################################

par(mfrow=c(2,3))
seq1 <- -1:4 #on va tracer 6 graphiques
seq2 <- 1:75 #les 75 puits
r2 <- numeric(5) #la moyenne du rsquared
for (j in seq1){
  r2j = 0.0 # le rsquared d'une regression
  for (i in seq2){
    mois <- 1:35 #l'abscisse
    v <- as.vector(puits[,i]) #on récupère les données du puits i
    
    col="black" #la couleur en fonction de la classification de qualité
    if (v[36] == "Good"){col = "red"}else if (v[36] == "medium"){col = "green"}else {col = "blue"}
    
    v <- as.numeric(v[1:35])
    
    #on plot pas les 0 qui sont des ND
    nd <- which(v %in% 0)
    v <- v[v != 0]
    mois <- mois[!mois %in%  nd]
    
    #tracé de la figure 1 : les données de production
    if (j==-1){
      if (i==1){
        plot(mois,v,type="l",col=col, ylab="production simulée", main="Les courbes non ajustées",ylim=c(0,max(v)+10))
      }
      else 
        lines(mois,v,type="l",col=col) 
    }
    #tracé de la figure 2 : les courbes de production obtenues avec des polynômes de degré 0, 1, 2, 3, et 4
    else{
      if (j==0){
        fit2 <- lm(v ~ 1)
      }else {
        fit2 <- lm(v ~ poly(mois, j, raw=TRUE))
      }
      if (i==1){
        plot(mois, predict(fit2), type="l",col=col, ylab="production simulée", lwd=1, main=paste("Ajustement par un polynôme de degré ",j), ylim=c(0,max(predict(fit2))+10))
      }
      lines(mois, predict(fit2), col=col, lwd=1)
    }
    if (j >= 0) {r2j=r2j+summary(fit2)$adj.r.squared}
  }
  #on calcule la moyenne des R-Squared pour chaque type de régression
  if (j >= 0){
    r2[j+1]=r2j/i
  }
}

par(mfrow=c(1,1))
#####################################################
##   QUESTION   2    -    Régression exponentielle ##
#####################################################

#Avec lm
rse2=0
r2e1 <- numeric(1)
for (i in seq2){
  mois <- 1:35
  v <- as.vector(puits[,i])
  
  col="black"
  if (v[36] == "Good"){
    col = "red"
  }else if (v[36] == "medium"){
    col = "green"
  }else 
    col = "blue"
  
  v <- as.numeric(v[1:35])
  
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  
  expfit <- lm(log(v) ~ mois)
  
  if (i==3){
    #residus=v-exp(expfit$fitted.values)
    #plot(v,residus, main=paste("Graphique des résidus de la régression exponentielle"), xlab="production simulée", ylab="résidus")
    
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="production simulée",
         main=paste("Régression exponentielle"),ylim=c(0,max(exp(predict(expfit)))+10))
    
  }
  else {
    lines(mois,exp(predict(expfit)),type="l",col=col)
  }
  rse2 = rse2 + exp(sigma(expfit))
}
rse2 = rse2 / 75
rse2 
summary(expfit)


par(mfrow=c(1,1))
#----------------------------------------------------------

#Avec nls
rse = 0
seq2 <- 1:75
r2e2 <- numeric(1)
for (i in seq2){
  mois <- 1:35
  v <- as.vector(puits[,i])
  
  col="black"
  if (v[36] == "Good"){
    col = "red"
  }else if (v[36] == "medium"){
    col = "green"
  }else {
    col = "blue"
  }
  
  v <- as.numeric(v[1:35])
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  df <- data.frame(mois, v)
  df$lv <- df$v
  
  k0start=-300
  k1start=0.1
  
  m <- nls(lv ~ k0*exp(-k1*mois), start=c(k0=k0start, k1=k1start), df)
  summary(m)

  if (i==1){
    plot(df$mois,predict(m),type="l",col=col, ylab="production simulée", xlab="mois", main=paste("Régression exponentielle"),ylim=c(0,max(predict(m))+10))
  }
  lines(df$mois,predict(m),type="l",col=col)
  
  rse = rse + sigma(m)
}
rse = rse / 75
rse 
summary(m)


par(mfrow=c(2,3))
########################################################
##   QUESTION   3   Courbe haute + courbe basse à 95% ##
########################################################

#avec nls + predict_NLS
plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE
for (i in seq2){
  mois <- 1:35
  v <- as.vector(puits[,i])
  classif = v[36]
  
  v <- as.numeric(v[1:35])
  
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  df <- data.frame(mois, v)
  df$lv <- log(df$v)
  
  col="black"
  
  k0start=400
  k1start=0.01
  
  m <- nls(lv ~ k0*exp(-k1*mois), start=c(k0=k0start, k1=k1start), df)
  summary(m)
  
  k0 = signif(exp(coef(m)[1][["k0"]]), digits = 4)
  k1 = signif(exp(coef(m)[2][["k1"]]), digits = 4)
  
  
  if (classif == "Good" && plotGood == TRUE){
    predict1 = predictNLS(m, df)
    plotGood = FALSE
    col = "red"
    plot(df$mois,exp(predict(m)),type="p",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    lines(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    
    lines(mois,exp(predict1[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict1[,7]),type="l",col="black",lwd = 1,lty=2)  
    
  }else if (classif == "medium" && plotMed == TRUE){
    predict2 = predictNLS(m, df)
    plotMed = FALSE
    col = "green"
    plot(df$mois,exp(predict(m)),type="p",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité medium avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    lines(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité medium avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    
    lines(mois,exp(predict2[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict2[,7]),type="l",col="black",lwd = 1,lty=2)  
    
  }else if (classif== "bad" && plotBad == TRUE) {
    predict3 = predictNLS(m, df)
    plotBad = FALSE
    col = "blue"
    plot(df$mois,exp(predict(m)),type="p",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    lines(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(exp(predict(m)))+10))
    
    lines(mois,exp(predict3[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict3[,7]),type="l",col="black",lwd = 1,lty=2)   
  }
}

#avec predict
seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE

par(mfrow=c(2,3))
for (i in seq2){
  mois <- 1:35
  v <- as.vector(puits[,i])
  classif = v[36]
  
  col="black"
  
  v <- as.numeric(v[1:35])
  
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  
  expfit <- lm(log(v) ~ mois)
  
  k0start = signif(exp(expfit$coefficients[1]), digit=4)
  k1start = signif(-expfit$coefficients[2], digit=4)
  
  newdat <- data.frame(mois)  
  
  if (classif == "Good" && plotGood == TRUE){
    plotGood = FALSE
    col = "red"
    
    predG = predict(expfit, newdat, interval="confidence", level=0.95)
    epredG = exp(predG)
    
    plot(mois,epredG[,1],type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(epredG[,1]))+10)
    
    lines(mois,epredG[,2],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,epredG[,3],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
  }else if (classif == "medium" && plotMed == TRUE){
    plotMed = FALSE
    col = "green"
    
    predG = predict(expfit, newdat, interval="confidence", level=0.95)
    epredG = exp(predG)
    
    plot(mois,epredG[,1],type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité médium avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(epredG[,1]))+10)
    
    lines(mois,epredG[,2],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,epredG[,3],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
  }else if (classif== "bad" && plotBad == TRUE) {
    plotBad = FALSE
    col = "blue"
    
    predG = predict(expfit, newdat, interval="confidence", level=0.95)
    epredG = exp(predG)
    
    plot(mois,epredG[,1],type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(epredG[,1]))+10)
    
    lines(mois,epredG[,2],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,epredG[,3],type="l",col="black",ylim=c(0,max(exp(predict(expfit)))+10))
  }
}


############################################################
##   QUESTION   4  Suggestions sur 5 courbes mal classées ##
############################################################

improvePredict <- function(colorsPred = c(), badClass = c()){
  #On stocke les valeurs de k0, k1 et la classe (couleur)
  k0s <- numeric(75)
  k1s <- numeric(75)
  colors <- numeric(75)
  par(mfrow=c(1,1))
  r2e1 <- numeric(1)
  for(i in seq2){
    mois <- 1:35
    v <- as.vector(puits[,i])
    
    col="black"
    if (v[36] == "Good"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "red"
      }
    }else if (v[36] == "medium"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "green"
      }
    }else {
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "blue"
      }
    }
    
    v <- as.numeric(v[1:35])
    
    nd <- which(v %in% 0)
    v <- v[v != 0]
    mois <- mois[!mois %in%  nd]
    
    expfit <- lm(log(v) ~ mois)
    summary(expfit)
    
    
    k0i = exp(expfit$coefficients[1])
    k1i = -expfit$coefficients[2]
    k0s[i] = k0i
    k1s[i] = k1i
    colors[i] = col
    
    rsquared = summary(expfit)$adj.r.squared
    r2e1=r2e1+summary(expfit)$adj.r.squared
  }
  
  r2e1=r2e1/i
  r2e1
  kcoefs <- c()
  kcoefs$k0 <- k0s
  kcoefs$k1 <- k1s
  kcoefs$col <- colors
  
  clustering <- multinom(col ~ k0 + k1, data = kcoefs)
  summary(clustering)
  
  names = character(75)
  names <- lapply(datapuits$V1, as.character)
  
  kcoefs$names <- names
  kcoefs$model <- clustering
  
  kcoefs$colPred <- clustering$lev[predict(clustering)]
  
  # Compter les courbes mal classées
  count=0
  if(length(kcoefs$colPred) > 0){
    for(i in 1:75){
      if (kcoefs$col[i] != kcoefs$colPred[i]){
        count=count + 1
      }
    }
  }
  kcoefs$badPredictCount <- count
  kcoefs$correctedPoints <- length(badClass)
  
  plot(kcoefs$k0,kcoefs$k1,type="p",pch=1,cex=2, lwd = 2,col=kcoefs$col, main="k1 en fonction de k0")
  lines(kcoefs$k0,kcoefs$k1,type="p",pch=19,cex=1,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")
  text(kcoefs$k0, kcoefs$k1, labels=names, cex= 0.7, pos=3)
  
  
  print(kcoefs$badPredictCount)
  print(kcoefs$correctedPoints)
  
  return(kcoefs)
}

removePoint <- function(badClass, pointNumber){
  badClass=c(badClass, which(kcoefs$names == paste("Well", pointNumber, sep="-")))
  kcoefs = improvePredict(colorsPred = kcoefs$colPred,badClass = badClass)
  return (badClass)
}

badClass=c()
kcoefs = improvePredict()

# On a 16 courbes mal prédites. On en sélectionne 5.

# Courbe n°75
badClass = removePoint(badClass, 288)
# Courbe n°47
badClass = removePoint(badClass, 333)
# Courbe n°39
badClass = removePoint(badClass, 246)
# Courbe n°53
badClass = removePoint(badClass, 257)
# Courbe n°11
badClass = removePoint(badClass, 258)



#5) Gestion des spikes (smoothing curves) 

#fait avec loess
seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75
#polynomial de degré 3
r2p3 <- numeric(1)
for (i in seq2){
  i=1
  #l'abscisse
  mois <- 1:35
  
  #on récupère les données du puits i
  v <- as.vector(puits[,i])
  
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
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  
  smooth <- loess(v~mois)
  fit3 <- lm(smooth$fitted ~ poly(mois, 3, raw=TRUE))
  
  if (i==1){
    plot(mois, predict(fit3), type="l",col=col, ylab="gas prod", lwd=1, main="Régression polynomiale de degré 3 avec smooth ", ylim=c(0,max(predict(fit3))+10))
  } else {
    lines(mois, predict(fit3), col=col, ylab="gas prod", lwd=1)
  }
  lines(mois, predict(fit3), col=col, ylab="gas prod", lwd=1)
  
  rsquared = summary(fit3)$adj.r.squared
  r2p3=r2p3+summary(fit3)$adj.r.squared
}

#moyenne du rsquared
r2p3=r2p3/i
r2p3


#exponentiel
r2e3 <- numeric(1)
par(mfrow=c(1,1))
for (i in seq2){
  
  mois <- 1:35
  v <- as.vector(puits[,i])
  
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
  
  smooth <- loess(v~mois)
  
  nd2 <- which(smooth$fitted <= 0)
  smooth$fitted <- smooth$fitted[smooth$fitted > 0]
  mois <- mois[!mois %in%  nd2]
  
  expfit <- lm(log(smooth$fitted) ~ mois)
  
  #tracé de la figure 1 : les données de production
  if (i==1){
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", main="Régression exponentielle avec smooth",ylim=c(0,max(exp(predict(expfit)))+10))
  } else {
    lines(mois,exp(predict(expfit)),type="l",col=col)
  }
  
  rsquared = summary(expfit)$adj.r.squared
  print (rsquared)
  r2e3=r2e3+summary(expfit)$adj.r.squared
}

#moyenne du rsquared
r2e3=r2e3/i
r2e3

# -------------------------------------------------
# On relance le clustering avec les données lissées
# -------------------------------------------------

# Exonentielle
improvePredict2 <- function(colorsPred = c(), badClass = c()){
  #On stocke les valeurs de k0, k1 et la classe (couleur)
  k0s <- numeric(75)
  k1s <- numeric(75)
  colors <- numeric(75)
  par(mfrow=c(1,1))
  r2e1 <- numeric(1)
  for(i in seq2){
    mois <- 1:35
    v <- as.vector(puits[,i])
    
    col="black"
    if (v[36] == "Good"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "red"
      }
    }else if (v[36] == "medium"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "green"
      }
    }else {
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "blue"
      }
    }
    
    v <- as.numeric(v[1:35])
    
    smooth <- loess(v~mois)
    
    nd2 <- which(smooth$fitted <= 0)
    smooth$fitted <- smooth$fitted[smooth$fitted > 0]
    mois <- mois[!mois %in%  nd2]
    
    expfit <- lm(log(smooth$fitted) ~ mois)
    summary(expfit)
    
    
    k0i = exp(expfit$coefficients[1])
    k1i = -expfit$coefficients[2]
    k0s[i] = k0i
    k1s[i] = k1i
    colors[i] = col
    
    rsquared = summary(expfit)$adj.r.squared
    r2e1=r2e1+summary(expfit)$adj.r.squared
  }
  
  r2e1=r2e1/i
  r2e1
  kcoefs <- c()
  kcoefs$k0 <- k0s
  kcoefs$k1 <- k1s
  kcoefs$col <- colors
  
  clustering <- multinom(col ~ k0 + k1, data = kcoefs)
  print(summary(clustering))
  
  names = character(75)
  names <- lapply(datapuits$V1, as.character)
  
  kcoefs$names <- names
  kcoefs$model <- clustering
  
  kcoefs$colPred <- clustering$lev[predict(clustering)]
  
  # Compter les courbes mal classées
  count=0
  if(length(kcoefs$colPred) > 0){
    for(i in 1:75){
      if (kcoefs$col[i] != kcoefs$colPred[i]){
        count=count + 1
      }
    }
  }
  kcoefs$badPredictCount <- count
  kcoefs$correctedPoints <- length(badClass)
  
  plot(kcoefs$k0,kcoefs$k1,type="p",pch=1,cex=2, lwd = 2,col=kcoefs$col, main="k1 en fonction de k0")
  lines(kcoefs$k0,kcoefs$k1,type="p",pch=19,cex=1,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")
  text(kcoefs$k0, kcoefs$k1, labels=names, cex= 0.7, pos=3)
  
  
  print(kcoefs$badPredictCount)
  print(kcoefs$correctedPoints)
  
  return(kcoefs)
}

removePoint2 <- function(badClass, pointNumber){
  badClass=c(badClass, which(kcoefs$names == paste("Well", pointNumber, sep="-")))
  kcoefs = improvePredict2(colorsPred = kcoefs$colPred,badClass = badClass)
  return (badClass)
}

badClass=c()
kcoefs = improvePredict2()

summary(clustering)
# On a 16 courbes mal prédites. On en sélectionne 5.
badClass = removePoint2(badClass, 333)
badClass = removePoint2(badClass, 301)
badClass = removePoint2(badClass, 328)
badClass = removePoint2(badClass, 258)
badClass = removePoint2(badClass, 312)
badClass = removePoint2(badClass, 288)
badClass = removePoint2(badClass, 290)
badClass = removePoint2(badClass, 268)
badClass = removePoint2(badClass, 246)
badClass = removePoint2(badClass, 289)
badClass = removePoint2(badClass, 312)



#Polynôme de degré 3
improvePredict3 <- function(colorsPred = c(), badClass = c()){
  seq1 <- -1:4
  #les 75 puits
  seq2 <- 1:75
  #On stocke les valeurs de k0, k1 et la classe (couleur)
  k0s <- numeric(75)
  k1s <- numeric(75)
  k2s <- numeric(75)
  k3s <- numeric(75)
  colors <- numeric(75)
  r2e1=0
  par(mfrow=c(1,1))
  for(i in seq2){
    mois <- 1:35
    v <- as.vector(puits[,i])

    col="black"
    if (v[36] == "Good"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "red"
      }
    }else if (v[36] == "medium"){
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "green"
      }
    }else {
      if(i %in% badClass){
        col = kcoefs$colPred[i]
      } else {
        col = "blue"
      }
    }

    v <- as.numeric(v[1:35])

    nd <- which(v %in% 0)
    v <- v[v != 0]
    mois <- mois[!mois %in%  nd]

    smooth <- loess(v~mois)
    fit3 <- lm(smooth$fitted ~ poly(mois, 3, raw=TRUE))


    k0i = fit3$coefficients[1]
    k1i = fit3$coefficients[2]
    k2i = fit3$coefficients[3]
    k3i = fit3$coefficients[4]
    k0s[i] = k0i
    k1s[i] = k1i
    k2s[i] = k2i
    k3s[i] = k3i
    colors[i] = col

    rsquared = summary(fit3)$adj.r.squared
    r2e1=r2e1+summary(fit3)$adj.r.squared
  }

  r2e1=r2e1/i
  r2e1
  kcoefs <- c()
  kcoefs$k0 <- k0s
  kcoefs$k1 <- k1s
  kcoefs$k2 <- k2s
  kcoefs$k3 <- k3s
  kcoefs$col <- colors

  clustering <- multinom(col ~ k0 + k1 + k2 + k3, data = kcoefs)
  print(summary(clustering))

  names = character(75)
  names <- lapply(datapuits$V1, as.character)

  kcoefs$names <- names
  kcoefs$model <- clustering

  kcoefs$colPred <- clustering$lev[predict(clustering)]

  # Compter les courbes mal classées
  count=0
  if(length(kcoefs$colPred) > 0){
    for(i in 1:75){
      if (kcoefs$col[i] != kcoefs$colPred[i]){
        count=count + 1
      }
    }
  }
  kcoefs$badPredictCount <- count
  kcoefs$correctedPoints <- length(badClass)

  plot(kcoefs$k0,-kcoefs$k3+kcoefs$k2+kcoefs$k1,type="p",pch=1,cex=2, lwd = 2,col=kcoefs$col, main="Classes des puits")
  lines(kcoefs$k0,-kcoefs$k3+kcoefs$k2+kcoefs$k1,type="p",pch=19,cex=1,col=clustering$lev[predict(clustering)],main="k3 en fonction de k0")
  text(kcoefs$k0, -kcoefs$k3+kcoefs$k2+kcoefs$k1, labels=names, cex= 0.7, pos=3)


  print(kcoefs$badPredictCount)
  print(kcoefs$correctedPoints)

  return(kcoefs)
}

removePoint3 <- function(badClass, pointNumber){
  badClass=c(badClass, which(kcoefs$names == paste("Well", pointNumber, sep="-")))
  kcoefs = improvePredict3(colorsPred = kcoefs$colPred,badClass = badClass)
  return (badClass)
}

badClass=c()
kcoefs = improvePredict3()
badClass = removePoint3(badClass, 257)
badClass = removePoint3(badClass, 250)
badClass = removePoint3(badClass, 333)
badClass = removePoint3(badClass, 266)


###################################
##      FONCTION LIBRAIRIES      ##
###################################
#Pour la question 3

predictNLS <- function(
  object, 
  newdata,
  level = 0.95, 
  nsim = 10000,
  ...
)
{
  require(MASS, quietly = TRUE)
  
  ## get right-hand side of formula
  RHS <- as.list(object$call$formula)[[3]]
  EXPR <- as.expression(RHS)
  
  ## all variables in model
  VARS <- all.vars(EXPR)
  
  ## coefficients
  COEF <- coef(object)
  
  ## extract predictor variable    
  predNAME <- setdiff(VARS, names(COEF))  
  
  ## take fitted values, if 'newdata' is missing
  if (missing(newdata)) {
    newdata <- eval(object$data)[predNAME]
    colnames(newdata) <- predNAME
  }
  
  ## check that 'newdata' has same name as predVAR
  if (names(newdata)[1] != predNAME) stop("newdata should have name '", predNAME, "'!")
  
  ## get parameter coefficients
  COEF <- coef(object)
  
  ## get variance-covariance matrix
  VCOV <- vcov(object)
  
  ## augment variance-covariance matrix for 'mvrnorm' 
  ## by adding a column/row for 'error in x'
  NCOL <- ncol(VCOV)
  ADD1 <- c(rep(0, NCOL))
  ADD1 <- matrix(ADD1, ncol = 1)
  colnames(ADD1) <- predNAME
  VCOV <- cbind(VCOV, ADD1)
  ADD2 <- c(rep(0, NCOL + 1))
  ADD2 <- matrix(ADD2, nrow = 1)
  rownames(ADD2) <- predNAME
  VCOV <- rbind(VCOV, ADD2) 
  
  ## iterate over all entries in 'newdata' as in usual 'predict.' functions
  NR <- nrow(newdata)
  respVEC <- numeric(NR)
  seVEC <- numeric(NR)
  varPLACE <- ncol(VCOV)   
  
  ## define counter function
  counter <- function (i) 
  {
    #if (i%%10 == 0) 
    #  cat(i)
    #else cat(".")
    #if (i%%50 == 0) 
    #  cat("\n")
    #flush.console()
  }
  
  outMAT <- NULL 
  
  for (i in 1:NR) {
    counter(i)
    
    ## get predictor values and optional errors
    predVAL <- newdata[i, 1]
    if (ncol(newdata) == 2) predERROR <- newdata[i, 2] else predERROR <- 0
    names(predVAL) <- predNAME  
    names(predERROR) <- predNAME  
    
    ## create mean vector for 'mvrnorm'
    MU <- c(COEF, predVAL)
    
    ## create variance-covariance matrix for 'mvrnorm'
    ## by putting error^2 in lower-right position of VCOV
    newVCOV <- VCOV
    newVCOV[varPLACE, varPLACE] <- predERROR^2
    
    ## create MC simulation matrix
    simMAT <- mvrnorm(n = nsim, mu = MU, Sigma = newVCOV, empirical = TRUE)
    
    ## evaluate expression on rows of simMAT
    EVAL <- try(eval(EXPR, envir = as.data.frame(simMAT)), silent = TRUE)
    if (inherits(EVAL, "try-error")) stop("There was an error evaluating the simulations!")
    
    ## collect statistics
    PRED <- data.frame(predVAL)
    colnames(PRED) <- predNAME   
    FITTED <- predict(object, newdata = data.frame(PRED))
    MEAN.sim <- mean(EVAL, na.rm = TRUE)
    SD.sim <- sd(EVAL, na.rm = TRUE)
    MEDIAN.sim <- median(EVAL, na.rm = TRUE)
    MAD.sim <- mad(EVAL, na.rm = TRUE)
    QUANT <- quantile(EVAL, c((1 - level)/2, level + (1 - level)/2))
    RES <- c(FITTED, MEAN.sim, SD.sim, MEDIAN.sim, MAD.sim, QUANT[1], QUANT[2])
    outMAT <- rbind(outMAT, RES)
  }
  
  colnames(outMAT) <- c("fit", "mean", "sd", "median", "mad", names(QUANT[1]), names(QUANT[2]))
  rownames(outMAT) <- NULL
  
  #cat("\n")
  
  return(outMAT)  
}
