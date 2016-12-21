#TODO OPTI les redondances de code
#TODO Q4
#TODO trouver k0 et k1 optis
#TODO trouver des explications formelles aux r�sultats pour TODO le rapport
#TODO meilleur smooth que loess?


install.packages("gdata")
install.packages("nls2")
library(nls2)
library(gdata) 

#TODO en relatif le chemin du r�pertoire du projet
setwd("R/projet")
getwd()

#r�cup�ration des donn�es du fichier EXCEL
#il faut avoir perl d'install�
datapuits = read.xls(file.path("data/FW_Donnees_Puits.xlsx"))
datapuits

#on transpose les donn�es pour avoir les puits en colonne et les mois en ligne
tdata = setNames(data.frame(t(datapuits[,-1])), datapuits[,1])
tdata

#1) On obtient pas les meme courbes que le prof (bizarre, �a doit venir du traitement des donn�es, j'ai obtenu 
# un truc similaire a un moment donn�e mais je sais plus comment - et en meme temps c'est WAHL donc 
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
    
    #on r�cup�re les donn�es du puits i
    v <- as.vector(tdata[,i])
    
    #la couleur en fonction de la classification de qualit�
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
    
    #trac� de la figure 1 : les donn�es de production
    if (j==0){
      if (i==1){
        plot(mois,v,type="l",col=col, ylab="gas prod", main="Les courbes non fitt�es",ylim=c(0,max(v)+10))
        #axis(side=2, at=seq(0, 700, by=100))
        #box()
      }else {
        lines(mois,v,type="l",col=col) 
      } 
    }
    #trac� de la figure 2 : les courbes de production obtenues avec des polyn�mes de degr� 2 (et 0, 1, 3, 4)
    else{
      fit2 <- lm(v ~ poly(mois, j, raw=TRUE))
      #summary(fit2)
      if (i==1){
        plot(mois, predict(fit2), type="l",col=col, ylab="gas prod", lwd=1, main=paste("R�gression polynomiale de degr� ",j), ylim=c(0,max(predict(fit2))+10))
      }else {
        lines(mois, predict(fit2), col=col, ylab="gas prod", lwd=1)
      } 
    }
  }
}


#2) R�gression parametrique (exponentielle) 
#valeur pas trop mal avec k0=0.5 et k1=0.1
#k0 fait fluctuer la valeur de d�part notamment (plus k0 est grand, plus les courbes sont hautes au d�part)
#k1 aplatit les courbes 0.1 (TODO expliquer tout �a)
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
  
  #trac� de la figure 1 : les donn�es de production
  if (i==1){
    plot(mois,predict(expfit),type="l",col=col, ylab="gas prod", main=paste("R�gression exponentielle avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(predict(expfit))+10))
    #axis(side=2, at=seq(0, 700, by=100))
    #box()
  }else {
    lines(mois,predict(expfit),type="l",col=col) 
  } 
 
}

#Est-ce que cela marche ? avez-vous d'autres id�es ?
#pas trop mal je dirai
#pas d'autres id�es -> demander aux maths

#3) Courbe haute + courbe basse � 95%

#j'y arrive pas...
# il faut utiliser nls probablement pour les regression non lineaires 
# revoir les parametres k0 et k1 : les changer!!
#https://www.r-bloggers.com/first-steps-with-non-linear-regression-in-r/

seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE

par(mfrow=c(2,2))
for (i in seq2){
  
  mois <- 1:35
  v <- as.vector(tdata[,i])
  
  v2 <- as.numeric(v[1:35])
  
  #on plot pas les 0 qui sont des ND (d'apres moi)
  #nd <- which(v2 %in% 0)
  #v2 <- v2[v2 != 0]
  x <- mois#[!mois %in%  nd]
  
  k0=0.5
  k1=0.1
  
  y=k0*exp(-k1*x)
  y2 <- log(y)
  
  expfit <- lm(v2 ~ x2)
  #pred = predict(expfit)
  #pred2<-plot_fit(expfit)#,focal_var = "C",inter_var = "N")
  

  
  pred <- predict(expfit)
  
  #ci <- confint(expfit)
  
  #y2 <- log(y)
  #model <- lm(v2 ~ y2)##
  
  
  #param <- summary(model)$coefficients[, 1] 
  #se <- summary(model)$coefficients[, 2]
  
  #ddl <- 35 -2
  #uc <- param + qt(0.975, ddl) * se
  #lc <- param - qt(0.975, ddl) * se

  
  col="black"
  if (v[36] == "Good" && plotGood == TRUE){
    
    a_start<-400
    b_start<-2*log(2)/a_start
    
    m <- nls(v2~a*exp(-b*x), start = c(a=a_start,b=b_start))
    
    preds <- predict(m, interval = "prediction")
    
    
    col = "red"
    plotGood = FALSE
    plot(mois,pred,type="l",col=col, 
         ylab="gas prod", main="IC courbe de qualit� good",
         ylim=c(0,max(pred)+10))#, data = df)
    
    
    lines(x,preds,lty=2,lwd=3)
    print(cor(v2,preds))
    
    
    #lines(newx, pred[ ,3], lty = 'dashed', col = 'red')
    #lines(newx, pred[ ,2], lty = 'dashed', col = 'red')
    
  }else if (v[36] == "medium" && plotMed == TRUE){
    
    
    a_start<-120
    b_start<-2*log(2)/a_start
    
    m <- nls(v2~a*exp(-b*x), start = c(a=a_start,b=b_start))
    
    preds <- predict(m, interval = "prediction")
    
    
    col = "green"
    plotMed = FALSE
    plot(mois,pred,type="l",col=col, ylab="gas prod", 
         main="IC courbe de qualit� medium",
         ylim=c(0,max(pred)+10))#,data = df)
    
    
    lines(x,preds,lty=2,lwd=3)
    print(cor(v2,preds))
    
    
    #matlines(y, pred[, c("lwr")], col = "orange")
    #matlines(y, pred[, c("upr")], col = "purple")
  }else if (v[36] == "bad" && plotBad == TRUE){
    
    
    a_start<-40
    b_start<-10*log(10)/a_start
    
    m <- nls(v2~a*exp(-b*x), start = c(a=a_start,b=b_start))
    
    preds <- predict(m, interval = "prediction")
    
    
    col = "blue"
    plotBad = FALSE
    plot(mois,pred,type="l",col=col, ylab="gas prod", 
         main="IC courbe de qualit� bad",
         ylim=c(0,max(predict(expfit))+10))#, data = df)
    
    lines(x,preds,lty=2,lwd=3)
    
    print(cor(v2,preds))
    
    
    #matlines(y, pred[, c("lwr")], col = "orange")
    #matlines(y, pred[, c("upr")], col = "purple")
  }
}




#4) Suggestions sur 5 courbes mal class�es
#certaines apparaissent clairement (graphiquement) comme pouvant �tre class�es diff�remment



#5) Gestion des spikes (smoothing curves)

#polynomial de degr� 3
for (i in seq2){
  
  #l'abscisse
  mois <- 1:35
  
  #on r�cup�re les donn�es du puits i
  v <- as.vector(tdata[,i])
  
  #la couleur en fonction de la classification de qualit�
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
    plot(mois, predict(fit3), type="l",col=col, ylab="gas prod", lwd=1, main="R�gression polynomiale de degr� 3 avec smooth ", ylim=c(0,max(predict(fit3))+10))
  }else {
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
  
  k0=0.5
  k1=0.1
  y=k0*exp(-k1*mois)
  expfit <- lm(smooth$fitted ~ y)
  
  #trac� de la figure 1 : les donn�es de production
  if (i==1){
    plot(mois,predict(expfit),type="l",col=col, ylab="gas prod", main=paste("R�gression exponentielle avec smooth et avec k0 =",k0,"et k1 =",k1),ylim=c(0,max(predict(expfit))+10))
    #axis(side=2, at=seq(0, 700, by=100))
    #box()
  }else {
    lines(mois,predict(expfit),type="l",col=col) 
  } 
  
}


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
    if (i%%10 == 0) 
      cat(i)
    else cat(".")
    if (i%%50 == 0) 
      cat("\n")
    flush.console()
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
  
  cat("\n")
  
  return(outMAT)  
}
