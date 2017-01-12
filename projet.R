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



kcoefs <- c()
kcoefs$k0 <- k0s
kcoefs$k1 <- k1s
kcoefs$col <- colors

clustering <- multinom(col ~ k0 + k1, data = kcoefs)
summary(clustering)

plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")


###################
##   QUESTION   3##
###################
#Courbe haute + courbe basse à 95%

seq1 <- 0:4
#les 75 premiers puits
seq2 <- 1:75

plotGood <- TRUE
plotMed <- TRUE
plotBad <- TRUE

par(mfrow=c(2,2))
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

#quelles sont les incertitudes??


#3) Avec Bootstrap

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
  
  
  k0start = signif(exp(expfit$coefficients[1]), digit=6)
  k1start = signif(-expfit$coefficients[2], digit=6)
  
  if (classif == "Good" && plotGood == TRUE){
    plotGood = FALSE
    col = "red"
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité good avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
    v1=log(v)
    boot_1000=replicate(1000, sort(sample(v1,replace=T))) # générer 1000 échantillons Bootstrap
    tab3=cbind(v1,boot_1000) #correspond à tab1 tout simplement la même commande
    
    tab3_bis=cbind(Moyenne=apply(tab3,2,mean),Médiane=apply(tab3,2,median),Variance=apply(tab3,2,var)) # équivalent à tab2 avec les même commande appliquée à tab3
    
    rownames(tab3_bis)[1]="theta"
    rownames(tab3_bis)[-1]=paste("theta_star",1:1000,sep="")  
    
    tab_4=rbind(tab3_bis,theta_star=apply(tab3_bis[-1,],2,mean),sigma_star=apply(tab3_bis[-1,],2,sd))
    tab_4
    
    ci = rbind(borne_inf=tab_4[1,]-qnorm(0.975)*tab_4[nrow(tab_4)],borne_sup=tab_4[1,]+qnorm(0.975)*tab_4[nrow(tab_4)])
    
    l = ci[1,3]
    u = ci[2,3]
    
    lines(mois,exp(predict(expfit))-exp(l),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,exp(predict(expfit))+exp(u),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
    
  }else if (classif == "medium" && plotMed == TRUE){
    plotMed = FALSE
    col = "green"
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité medium avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
    v1=log(v)
    boot_1000=replicate(1000, sort(sample(v1,replace=T))) # générer 1000 échantillons Bootstrap
    tab3=cbind(v1,boot_1000) #correspond à tab1 tout simplement la même commande
    
    tab3_bis=cbind(Moyenne=apply(tab3,2,mean),Médiane=apply(tab3,2,median),Variance=apply(tab3,2,var)) # équivalent à tab2 avec les même commande appliquée à tab3
    
    rownames(tab3_bis)[1]="theta"
    rownames(tab3_bis)[-1]=paste("theta_star",1:1000,sep="")  
    
    tab_4=rbind(tab3_bis,theta_star=apply(tab3_bis[-1,],2,mean),sigma_star=apply(tab3_bis[-1,],2,sd))
    tab_4
    
    ci = rbind(borne_inf=tab_4[1,]-qnorm(0.975)*tab_4[nrow(tab_4)],borne_sup=tab_4[1,]+qnorm(0.975)*tab_4[nrow(tab_4)])
    
    l = ci[1,3]
    u = ci[2,3]
    
    lines(mois,exp(predict(expfit))-exp(l),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,exp(predict(expfit))+exp(u),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
    
  }else if (classif== "bad" && plotBad == TRUE) {
    plotBad = FALSE
    col = "blue"
    
    plot(mois,exp(predict(expfit)),type="l",col=col, ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
    #v_trie=sort(v) # on trie le premier éch avant de générer les 3 ech bootstrap
    #x1=sample(v_trie,replace=T) # replace =T pour préciser avec remise
    #x2=sample(v_trie,replace=T)
    #x3=sample(v_trie,replace=T)
    
    #tab1=cbind(y_trie,xstar1=sort(x1),xstar2=sort(x2),xstar3=sort(x3))
    #tab2=cbind(Moyenne=apply(tab1,2,mean),Médiane=apply(tab1,2,median),Variance=apply(tab1,2,var))
    #rownames(tab2)=c("theta","theta_star1","theta_star2","theta_star3")
    
    v1=log(v)
    boot_1000=replicate(1000, sort(sample(v1,replace=T))) # générer 1000 échantillons Bootstrap
    tab3=cbind(v1,boot_1000) #correspond à tab1 tout simplement la même commande
    
    tab3_bis=cbind(Moyenne=apply(tab3,2,mean),Médiane=apply(tab3,2,median),Variance=apply(tab3,2,var)) # équivalent à tab2 avec les même commande appliquée à tab3
    
    rownames(tab3_bis)[1]="theta"
    rownames(tab3_bis)[-1]=paste("theta_star",1:1000,sep="")  
    
    tab_4=rbind(tab3_bis,theta_star=apply(tab3_bis[-1,],2,mean),sigma_star=apply(tab3_bis[-1,],2,sd))
    tab_4
    
    ci = rbind(borne_inf=tab_4[1,]-qnorm(0.975)*tab_4[nrow(tab_4)],borne_sup=tab_4[1,]+qnorm(0.975)*tab_4[nrow(tab_4)])
    
    l = ci[1,3]
    u = ci[2,3]
    
    lines(mois,exp(predict(expfit))-exp(l),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    lines(mois,exp(predict(expfit))+exp(u),type="l",col="black", ylab="gas prod", xlab="mois", main=paste("Régression exponentielle d'une courbe \n de qualité bad avec k0 =",k0start,"et k1 =",k1start),ylim=c(0,max(exp(predict(expfit)))+10))
    
  }
}

ci


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
  print (rsquared)
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

plot(kcoefs$k0,kcoefs$k1,type="p",pch=15,col=kcoefs$col,main="k1 en fonction de k0")
lines(kcoefs$k0,kcoefs$k1,type="p",pch=7,col=clustering$lev[predict(clustering)],main="k1 en fonction de k0")



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

