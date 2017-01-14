install.packages("gdata")
install.packages("nls2")
install.packages("boot")
install.packages("kernlab")
library(nnet)
library(nls2)
library(gdata) 
library(boot)
library(kernlab)


#setwd("..\\..\\Projects\\petrolreg")
setwd("documents\\M2\\regression\\projet\\petrolreg")
getwd()

#récupération des données du fichier EXCEL
perl <- 'C:\\Strawberry\\perl\\bin\\perl.exe'
datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"))
#datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"), perl=perl)

#on transpose les données pour avoir les puits en colonne et les mois en ligne
puits = setNames(data.frame(t(datapuits[,-1])), datapuits[,1])
puits

###################
##   QUESTION   1##
###################
#Régressions polynomiales

par(mfrow=c(2,3))

#on va tracer 5 graphiques
seq1 <- -1:4
#les 75 puits
seq2 <- 1:75
r2 <- numeric(5)
for (j in seq1){
  r2j = 0.0
  for (i in seq2){
    
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
    
    #on plot pas les 0 qui sont des ND
    nd <- which(v %in% 0)
    v <- v[v != 0]
    mois <- mois[!mois %in%  nd]
    
    #tracé de la figure 1 : les données de production
    if (j==-1){
      if (i==1){
        plot(mois,v,type="l",col=col, ylab="gas prod", main="Les courbes non fittées",ylim=c(0,max(v)+10))
      }
      lines(mois,v,type="l",col=col) 

    }
    #tracé de la figure 2 : les courbes de production obtenues avec des polynômes de degré 0, 1, 2, 3, et 4
    else{
      if (j==0){
        fit2 <- lm(v ~ 1)
      }
      else {
        fit2 <- lm(v ~ poly(mois, j, raw=TRUE))
      }
      if (i==1){
        plot(mois, predict(fit2), type="l",col=col, ylab="gas prod", lwd=1, main=paste("Régression polynomiale de degré ",j), ylim=c(0,max(predict(fit2))+10))
      }
      lines(mois, predict(fit2), col=col, ylab="gas prod", lwd=1)
    }
    if (j >= 0) {
      r2j=r2j+summary(fit2)$adj.r.squared
    }
    #print(r2j)
  }
  #on calcule la moyenne des R-Squared pour chaque type de régression
  if (j >= 0){
    r2j=r2j/i
    r2[j+1]=r2j
  }
}
r2

###################
##   QUESTION   2##
###################
#Régression exponentielle

#Avec lm
rse2=0
par(mfrow=c(1,1))
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
  
  # tracé de la figure 1 : les données de production 
  if (i==1){
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod",
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


#----------------------------------------------------------

#Avec nls
rse = 0

k0s <- numeric(75)
k1s <- numeric(75)
colors <- numeric(75)
seq2 <- 1:75
# Tentative avec nls
par(mfrow=c(1,1))

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
  
  k0i = coef(m)[1][["k0"]]
  k1i = coef(m)[2][["k1"]]
  k0s[i] = k0i
  k1s[i] = k1i
  colors[i] = col
  
  if (i==1){
    plot(df$mois,predict(m),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle"),ylim=c(0,max(predict(m))+10))
  }
  lines(df$mois,predict(m),type="l",col=col)
  
  rse = rse + sigma(m)
}

rse = rse / 75
rse 
summary(m)
# 
# kcoefs <- c()
# kcoefs$k0 <- k0s
# kcoefs$k1 <- k1s
# kcoefs$col <- colors
# 
# clustering <- multinom(col ~ k0 + k1, data = kcoefs)
# summary(clustering)
# 
# plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
# lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")


###################
##   QUESTION   3##
###################
#Courbe haute + courbe basse à 95%
#Quelles sont les incertitudes??

#NLS + predict_NLS
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
  
  v <- as.numeric(v[1:35])
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
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


#4) Suggestions sur 5 courbes mal classées
#certaines apparaissent clairement (graphiquement) comme pouvant être classées différemment

#On stocke les valeurs de k0 et k1
k0s <- numeric(75)
k1s <- numeric(75)
colors <- numeric(75)
par(mfrow=c(1,1))
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
  summary(expfit)
  
  k0i = exp(expfit$coefficients[1])
  k1i = -expfit$coefficients[2]
  k0s[i] = k0i
  k1s[i] = k1i
  colors[i] = col
  
  # tracé de la figure 1 : les données de production 
  if (i==1){
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod",
         main=paste("Régression exponentielle"),ylim=c(0,max(exp(predict(expfit)))+10)) 
  }
  else {
    lines(mois,exp(predict(expfit)),type="l",col=col)
  }
}

kcoefs <- c()
#kcoefs$name <- names
kcoefs$k0 <- k0s
kcoefs$k1 <- k1s
kcoefs$col <- colors

clustering <- multinom(col ~ k0 + k1, data = kcoefs)
summary(clustering)

names = character(75)
names <- lapply(datapuits$V1, as.character)

plot(kcoefs$k0,kcoefs$k1,type="p",pch=1,cex=2, lwd = 2,col=kcoefs$col, main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=19,cex=1,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")
text(kcoefs$k0, kcoefs$k1, labels=names, cex= 0.7, pos=3)

kcoefs$colPred <- clustering$lev[predict(clustering)]
kcoefs$names <- names


# On a 16 courbes mal prédites. On en sélectionne 5.
badClass=c()
# Courbe n°75
badClass=c(badClass, which(kcoefs$names == "Well-288"))#OK
# Courbe n°53
badClass=c(badClass, which(kcoefs$names == "Well-257"))#OK
# Courbe n°47
badClass=c(badClass, which(kcoefs$names == "Well-333"))#OK
# Courbe n°11
badClass=c(badClass, which(kcoefs$names == "Well-258"))#OK
# Courbe n°20
badClass=c(badClass, which(kcoefs$names == "Well-308"))#OK


# Courbe n°30
badClass=c(badClass, which(kcoefs$names == "Well-280"))
# Courbe n°45
badClass=c(badClass, which(kcoefs$names == "Well-287"))
# Courbe n°38
badClass=c(badClass, which(kcoefs$names == "Well-312"))
# Courbe n°61
badClass=c(badClass, which(kcoefs$names == "Well-290"))
# Courbe n°21
badClass=c(badClass, which(kcoefs$names == "Well-248"))
# Courbe n°48
badClass=c(badClass, which(kcoefs$names == "Well-305"))
# Courbe n°39
badClass=c(badClass, which(kcoefs$names == "Well-246"))
# Courbe n°59
badClass=c(badClass, which(kcoefs$names == "Well-319"))
# Courbe n°50
badClass=c(badClass, which(kcoefs$names == "Well-328"))
# Courbe n°57
badClass=c(badClass, which(kcoefs$names == "Well-250"))
# Courbe n°71
badClass=c(badClass, which(kcoefs$names == "Well-301"))




# On change les valeurs données par les experts pour les données mal prédites
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

kcoefs$names <- names

plot(kcoefs$k0,kcoefs$k1,type="p",pch=1,cex=2, lwd = 2,col=kcoefs$col, main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=19,cex=1,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")
text(kcoefs$k0, kcoefs$k1, labels=names, cex= 0.7, pos=3)



#5) Gestion des spikes (smoothing curves) 

#fait avec loess
seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75
#polynomial de degré 3
r2p3 <- numeric(1)
for (i in seq2){
  
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

