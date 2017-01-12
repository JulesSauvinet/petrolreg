#TODO OPTI les redondances de code
#TODO Q4
#TODO trouver k0 et k1 optis
#TODO trouver des explications formelles aux résultats pour TODO le rapport
#TODO meilleur smooth que loess?


install.packages("gdata")
install.packages("nls2")
install.packages("boot")

library(nnet)

library(nls2)
library(gdata) 
library(boot)

setwd("..\\..\\Projects\\petrolreg")
#setwd("D:\\Projets\\petrolreg")
#setwd("documents\\M2\\regression\\projet\\petrolreg")
getwd()

#récupération des données du fichier EXCEL, il faut avoir perl d'installé
perl <- 'C:\\Strawberry\\perl\\bin\\perl.exe'
datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"))
#datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"), perl=perl)
datapuits

#on transpose les données pour avoir les puits en colonne et les mois en ligne
tdata = setNames(data.frame(t(datapuits[,-1])), datapuits[,1])
tdata

#1) On obtient pas les meme courbes que le prof (bizarre, ça doit venir du traitement des données - 
#et en meme temps c'est WAHL donc on a peut etre raison nous)

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
    v <- v[v != 0]
    mois <- mois[!mois %in%  nd]
    
    #tracé de la figure 1 : les données de production
    if (j==0){
      if (i==1){
        plot(mois,v,type="l",col=col, ylab="gas prod", main="Les courbes non fittées",ylim=c(0,max(v)+10))
      }
      lines(mois,v,type="l",col=col) 

    }
    #tracé de la figure 2 : les courbes de production obtenues avec des polynômes de degré 2 (et 0, 1, 3, 4)
    else{
      fit2 <- lm(v ~ poly(mois, j, raw=TRUE))
      if (i==1){
        plot(mois, predict(fit2), type="l",col=col, ylab="gas prod", lwd=1, main=paste("Régression polynomiale de degré ",j), ylim=c(0,max(predict(fit2))+10))
      }
      lines(mois, predict(fit2), col=col, ylab="gas prod", lwd=1)
    }
  }
}


#2) Régression parametrique (exponentielle) 
#valeur pas trop mal avec k0=0.5 et k1=0.1
#k0 fait fluctuer la valeur de départ notamment (plus k0 est grand, plus les courbes sont hautes au départ)
#k1 aplatit les courbes 0.1 (TODO expliquer tout ça)

#----------------------------------------------------------

# Tentative avec lm
k0s <- numeric(75)
k1s <- numeric(75)
colors <- numeric(75)

par(mfrow=c(1,2))
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
  
  #y=k0*exp(-k1*mois)
  #v=log(v)#y=log(y)
  expfit <- lm(log(v) ~ mois)
  summary(expfit)
  
  
  k0i = exp(expfit$coefficients[1])
  k1i = -expfit$coefficients[2]
  k0s[i] = k0i
  k1s[i] = k1i
  colors[i] = col
  
  
  #tracé de la figure 1 : les données de production
  #if (i==1){
  #  plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", main=paste("Régression exponentielle"),ylim=c(0,max(exp(predict(expfit)))+10))
  #} else {
  #  lines(mois,exp(predict(expfit)),type="l",col=col)
  #}
}


kcoefs <- c()
kcoefs$k0 <- k0s
kcoefs$k1 <- k1s
kcoefs$col <- colors

clustering <- multinom(col ~ k0 + k1, data = kcoefs)
summary(clustering)

plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")

kcoefs$colPred <- clustering$lev[predict(clustering)]
# On a 16 courbes mal prédites. On en sélectionne 5.
badClass=c()

# Courbe n°75
badClass=c(badClass, which(kcoefs$k0 > 145 & kcoefs$k0 < 150))

# Courbe n°61
badClass=c(badClass, which(kcoefs$k0 > 175 & kcoefs$k0 < 190 & kcoefs$k1 > 0.06 & kcoefs$k1 < 0.07))

# Courbe n°47
badClass=c(badClass, which(kcoefs$k0 > 109 & kcoefs$k0 < 113 & kcoefs$k1 > 0.02 & kcoefs$k1 < 0.025))

# Courbe n°53
badClass=c(badClass, which(kcoefs$k0 > 150 & kcoefs$k0 < 175 & kcoefs$k1 < 0.02))

# Courbe n°35
badClass=c(badClass, which(kcoefs$k0 > 99 & kcoefs$k0 < 105 & kcoefs$k1 > 0.026 & kcoefs$k1 > 0.03))

# On change les valeurs données par les experts pour les données mal prédites
for(i in seq2){
  mois <- 1:35
  v <- as.vector(tdata[,i])
  
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
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  
  #y=k0*exp(-k1*mois)
  #v=log(v)#y=log(y)
  expfit <- lm(log(v) ~ mois)
  summary(expfit)
  
  
  k0i = exp(expfit$coefficients[1])
  k1i = -expfit$coefficients[2]
  k0s[i] = k0i
  k1s[i] = k1i
  colors[i] = col
}


kcoefs <- c()
kcoefs$k0 <- k0s
kcoefs$k1 <- k1s
kcoefs$col <- colors

clustering <- multinom(col ~ k0 + k1, data = kcoefs)
summary(clustering)

plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")


#----------------------------------------------------------

#----------------------------------------------------------

#les 75 premiers puits
k0s <- numeric(75)
k1s <- numeric(75)
colors <- numeric(75)

seq2 <- 1:75
# Tentative avec nls
par(mfrow=c(1,1))
for (i in seq2){
  mois <- 1:35
  v <- as.vector(tdata[,i])
  
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
    plot(df$mois,predict(m),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(predict(m))+10))
  }
  lines(df$mois,predict(m),type="l",col=col)
}

kcoefs <- c()
kcoefs$k0 <- k0s
kcoefs$k1 <- k1s
kcoefs$col <- colors

clustering <- multinom(col ~ k0 + k1, data = kcoefs)
summary(clustering)

plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")



# La régression est moins bonne qu'avec lm
#----------------------------------------------------------


#Est-ce que cela marche ? avez-vous d'autres idées ?
#pas trop mal je dirai
#pas d'autres idées -> demander aux maths

#3) Courbe haute + courbe basse à 95%

seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE

par(mfrow=c(2,3))
for (i in seq2){
  mois <- 1:35
  v <- as.vector(tdata[,i])
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
  
  
  if (classif == "Good" && plotGood == TRUE){
    predict1 = predictNLS(m, df)
    plotGood = FALSE
    col = "red"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict1[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict1[,7]),type="l",col="black",lwd = 1,lty=2)  

  }else if (classif == "medium" && plotMed == TRUE){
    predict2 = predictNLS(m, df)
    plotMed = FALSE
    col = "green"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité medium avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict2[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict2[,7]),type="l",col="black",lwd = 1,lty=2)  
  
  }else if (classif== "bad" && plotBad == TRUE) {
    predict3 = predictNLS(m, df)
    plotBad = FALSE
    col = "blue"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict3[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict3[,7]),type="l",col="black",lwd = 1,lty=2)   
  }
}

#quelles sont les incertitudes??


#3) Avec Bootstrap

# Fonction appelée par chaque sélection de la méthode boot
bs <- function(formula, data, indices) {
  k0start=400
  k1start=0.01
  
  d <- data[indices,]
  fit <- nls(formula, start=c(k0=k0start, k1=k1start),d)
  return(predict(fit)) 
} 

seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE

par(mfrow=c(2,3))
for (i in seq2){
  mois <- 1:35
  v <- as.vector(tdata[,i])
  classif = v[36]
  
  v <- as.numeric(v[1:35])
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
  nd <- which(v %in% 0)
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]
  df <- data.frame(mois, v)
  df$lv <- log(df$v)
  
  #bdf <- boot(data=df, statistic=bs, R=2000, formula=lv ~ k0*exp(-k1*mois))
  #bdf$t # Contient les données générées par boot
  
  # Comment intégrer ces données au calcul ci-après ?
  
  
  col="black"
  
  k0start=400
  k1start=0.01
  
  m <- nls(lv ~ k0*exp(-k1*mois), start=c(k0=k0start, k1=k1start), df)
  summary(m)
  
  
  if (classif == "Good" && plotGood == TRUE){
    predict1 = predictNLS(m, df)
    plotGood = FALSE
    col = "red"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict1[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict1[,7]),type="l",col="black",lwd = 1,lty=2)  
    
  }else if (classif == "medium" && plotMed == TRUE){
    predict2 = predictNLS(m, df)
    plotMed = FALSE
    col = "green"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité medium avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict2[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict2[,7]),type="l",col="black",lwd = 1,lty=2)  
    
  }else if (classif== "bad" && plotBad == TRUE) {
    predict3 = predictNLS(m, df)
    plotBad = FALSE
    col = "blue"
    plot(df$mois,exp(predict(m)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(m)))+10))
    lines(mois,exp(predict3[,6]),type="l",col="black",lwd = 1,lty=2)    
    lines(mois,exp(predict3[,7]),type="l",col="black",lwd = 1,lty=2)   
  }
}
#4) Suggestions sur 5 courbes mal classées
#certaines apparaissent clairement (graphiquement) comme pouvant être classées différemment






#5) Gestion des spikes (smoothing curves) 

#fait avec loess

#polynomial de degré 3
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
  v <- v[v != 0]
  mois <- mois[!mois %in%  nd]

  smooth <- loess(v~mois)
  fit3 <- lm(smooth$fitted ~ poly(mois, 3, raw=TRUE))
  
  if (i==1){
    plot(mois, predict(fit3), type="l",col=col, ylab="gas prod", lwd=1, main="Régression polynomiale de degré 3 avec smooth ", ylim=c(0,max(predict(fit3))+10))
  } else {
    lines(mois, predict(fit3), col=col, ylab="gas prod", lwd=1)
  }
}

#exponentiel
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
  
  smooth <- loess(v~mois)
  
  #k0=0.5
  #k1=0.1
  #y=k0*exp(-k1*mois)
  expfit <- lm(log(smooth$fitted) ~ mois)
  
  #tracé de la figure 1 : les données de production
  if (i==1){
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", main=paste("Régression exponentielle avec smooth et avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(predict(expfit))+10))
  } else {
    lines(mois,exp(predict(expfit)),type="l",col=col)
  }
}

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

