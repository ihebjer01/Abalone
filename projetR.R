#Tache 2 : Pre-traitement des données
#-------------------------------------------------------------------------------------------------------------------------------

# inderstanding the dataset
summary(Abalone)
str(Abalone)
#-------------------------------------------------------------------------------------------------------------------------------
## 1. Valeurs aberantes :
#Après avoir vérifié notre ensemble de données, nous avons remarqué qu'il contient des valeurs NA que nous allons traiter plus tard
#Alons verifier les valeurs abbérentes ,Tout d'abord, nous allons écrire une fonction qui affiche les valeurs aberrantes et lon va les imputer par des valeurs NA

Outliers_replacement <- function(variable_to_check, Q1, Q3) {
  Vmax = Q3+1.5*(Q3-Q1)
  Vmin = Q1-1.5*(Q3-Q1)
  outliers_inf<-which(Abalone$variable_to_check<Vmin)
  outliers_sup<-which(Abalone$variable_to_check>Vmax)
  for (variable in outliers_inf) { 
    Abalone$variable_to_check[variable] <- NA 
  }
  for (variable in outliers_sup) { 
    Abalone$variable_to_check[variable] <- NA 
  }
}

  #Diameter
names(Abalone)
boxplot(Abalone$Diameter,main = "Diameter")
summary(Abalone$Diameter)
Q1 = 0.3500
Q3 = 0.4800
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Diameter<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Diameter[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Diameter>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Diameter[variable] = NA 
}
boxplot(Abalone$Diameter,main = "Diameter")


  #Height
names(Abalone)
boxplot(Abalone$Height,main = "Height")
summary(Abalone$Height)
Q1 = 0.1150
Q3 = 0.1650
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Height<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Height[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Height>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Height[variable] = NA 
}
boxplot(Abalone$Height,main = "Height")


  #Whole_Weight
names(Abalone)
boxplot(Abalone$Whole_Weight,main = "Whole_Weight")
summary(Abalone$Whole_Weight)
Q1 = 0.4415
Q3 = 1.1530
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Whole_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Whole_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Whole_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Whole_Weight[variable] = NA 
}
boxplot(Abalone$Whole_Weight,main = "Whole_Weight")


  #Shucked_Weight
names(Abalone)
boxplot(Abalone$Shucked_Weight,main = "Shucked_Weight")
summary(Abalone$Shucked_Weight)
Q1 = 0.1860
Q3 = 0.5020
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Shucked_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Shucked_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Shucked_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Shucked_Weight[variable] = NA 
}
boxplot(Abalone$Shucked_Weight,main = "Shucked_Weight")


  #Viscera_Weight
names(Abalone)
boxplot(Abalone$Viscera_Weight,main = "Viscera_Weight")
summary(Abalone$Viscera_Weight)
Q1 = 0.0935
Q3 = 0.2530
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Viscera_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Viscera_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Viscera_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Viscera_Weight[variable] = NA 
}
boxplot(Abalone$Viscera_Weight,main = "Viscera_Weight")


  #SHell_Weight
names(Abalone)
boxplot(Abalone$SHell_Weight,main = "SHell_Weight")
summary(Abalone$SHell_Weight)
Q1 = 0.1300
Q3 = 0.3290
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$SHell_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$SHell_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$SHell_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$SHell_Weight[variable] = NA 
}
boxplot(Abalone$SHell_Weight,main = "SHell_Weight")


  #Rings
names(Abalone)
boxplot(Abalone$Rings,main="Rings")
summary(Abalone$Rings)
Q1 = 8.0000
Q3 = 11.000
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Rings<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Rings[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Rings>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Rings[variable] = NA 
}
boxplot(Abalone$Rings,main="Rings")
  #Sex
Abalone["Sex"][Abalone["Sex"] == ''] <- NA

##2. Valeurs manquantes :
  # Maintenant, nous allons vérifier le taux des valeurs NA
   sum(is.na(Abalone)) 
   T = (sum(is.na(Abalone))/prod(dim(Abalone)))
   T

  #On a 1.6% < 5% des données manquantes donc on les supprime
  dataset=na.omit(Abalone)
  View(dataset)
  sum(is.na(dataset))
  
#-------------------------------------------------------------------------------------------------------------------------------
# Tache 3 : Analyse univariée : 
#-------------------------------------------------------------------------------------------------------------------------------
  
attach(dataset)

# tester la normalité
shapiro.test(Height)
shapiro.test(Length)
shapiro.test(Diameter)
shapiro.test(Whole_Weight)
shapiro.test(Viscera_Weight)
shapiro.test(SHell_Weight)
shapiro.test(Shucked_Weight)
shapiro.test(Rings)

#Modalité de la variable qualitative :
table(Sex)
install.packages("ggplot2")
library(ggplot2)
ggplot(data=dataset,aes(x=Sex,fill=Sex))+geom_bar()
#-------------------------------------------------------------------------------------------------------------------------------
# Tache 4 : Analyse bivarié : 
#-------------------------------------------------------------------------------------------------------------------------------

# on va tester la correlation entre la variable quantative cible 'Diameter' avec les autres variable quantitative
# on va utiliser la coefficient de Spearman  puisque l’inférence statistique ne repose plus sur la normalité bivariée du couple de variables (X, Y ).
#------------------------------------------------
# Variable quantitative - Variable quantitative :
#------------------------------------------------
#Diameter - Height
#1- representation graphique
plot(Diameter,Height,main="Correlation Diameter-Height")
#2 test de correlation
cor.test(Diameter,Height,method="spearman")  
#R2=0.89 => il y'a une forte liaison entre Diameter et Height

# Diameter - Length
#1- representation graphique
plot(Diameter,Length,main="correlation Diameter-Length")
#2 test de correlation
cor.test(Length,Diameter,method="spearman")  
#R2=0.98 => il y'a une forte liaison entre Diameter et Length

# Diameter - Whole_Weight
#1- representation graphique
plot(Diameter,Whole_Weight,main="correlation Height-Whole_Weight")
#2 test de correlation
cor.test(Diameter,Whole_Weight,method="spearman")  
#R2=0.97 => il y'a une forte liaison entre Diameter et Whole_Weight

#Diameter-Shucked_Weight
#1- representation graphique
plot(Diameter,Shucked_Weight,main="correlation Diameter-Shucked_Weight")
#2 test de correlation
cor.test(Diameter,Shucked_Weight,method="spearman")  
#R2=0.94 => il y'a une forte liaison entre Diameter et Shucked_Weight

#Diameter-Viscera_Weight
#1- representation graphique
plot(Diameter,Viscera_Weight,main="correlation Diameter-Viscera_Weight")
#2 test de correlation
cor.test(Diameter,Viscera_Weight,method="spearman")  
#R2=0.94 => il y'a une forte liaison entre Diameter et Viscera_Weight

#Diameter-SHell_Weight
#1- representation graphique
plot(Diameter,SHell_Weight,main="correlation Diameter-SHell_Weight")
#2 test de correlation
cor.test(Diameter,SHell_Weight,method="spearman")  
#R2=0.95 => il y'a une forte liaison entre Diameter et SHell_Weight

#Diameter-Rings
#1- representation graphique
plot(Diameter,Rings,main="correlation Diameter-Rings")
#2 test de correlation
cor.test(Diameter,Rings,method="spearman")  
#R2=0.60 => il y'a une forte liaison entre Diameter et Rings

#------------------------------------------------
# Variable quantitative - Variable qualitaive :
#On va utiliser le Test de Kruskall-Wallis puisque on a une variable quantitative a plus de deux modalité (M,F,I)
#------------------------------------------------

boxplot(Length~Sex)
kruskal.test(Length~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Length et Sex

boxplot(Height~Sex)
kruskal.test(Height~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Height et Sex

boxplot(Diameter~Sex)
kruskal.test(Diameter~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Diameter et Sex

boxplot(Whole_Weight~Sex)
kruskal.test(Whole_Weight~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Whole_Weight et Sex


boxplot(Shucked_Weight~Sex)
kruskal.test(Shucked_Weight~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Shucked_Weight et Sex

boxplot(Viscera_Weight~Sex)
kruskal.test(Viscera_Weight~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Viscera_Weight et Sex

boxplot(SHell_Weight~Sex)
kruskal.test(SHell_Weight~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre SHell_Weight et Sex

boxplot(Rings~Sex)
kruskal.test(Rings~Sex)
# p-value <0.05 alors on accepte H1 : il existe une liaision entre Rings et Sex

#-------------------------------------------------------------------------------------------------------------------------------
# Tache 5 : Régression linéaire
#-------------------------------------------------------------------------------------------------------------------------------.
#1
#--------------------------------------------------------

RM= lm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
        , data=dataset)
RM
summary(RM)
#R2=0.974 , alors on conclut que 97.4% de la variabilité de Diameter est expliqué par les autres variable
#ce modéle est un bon modéle
#--------------------------------------------------------
#2
#--------------------------------------------------------

## On va eliminé les variables le moins significative qui ont le p_value le plus elevé 
## Alors on elimine la variable whole_weight puisque elle a la valeur p*=0.81 qui est le plus important
## On deduit le nouveau modéle :

RM1= lm(Diameter ~ Length + Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
       , data=dataset)
RM1
summary(RM1)

## Le modele ne perd pas de qualit́e, on a toujours R2=0.974, alors le meme taux d’information expliqéee de la variabiíe de Diameter
## On elimine la variable Length puisque elle a la valeur p*=0.24 qui est le plus important
## On deduit le nouveau modéle :
RM2=lm(Diameter ~  Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
       , data=dataset)
RM2
summary(RM2)

# R2=0.9075 
# On va utiliser AIC pour choisir entre les deux. Le critere AIC mesure la qualite du modele en tenant compte du nombre de parametre a estimer et les residus du modele. Le meilleur modele est le modele avec le critere AIC le plus faibel.
AIC(RM) #-2060.737
AIC(RM1) #-20608.68
# RM1 a un AIC= -20608.737 et RM2 a un AIC= -20608.68 alors le modèle RM1 offre un meilleur ajustement

R= residuals(RM1)
plot(R)
#On remarque un comportement unimodale autour de 0, on peut conclure que les residus sont non corrolees et de moyenne nulle.

#--------------------------------------------------------
#3 PCA
#--------------------------------------------------------

New_Data <- subset(dataset, select = -c(Whole_Weight) )
summary(New_Data)
AbalonePC <- New_Data[,2:8]
head(AbalonePC)

# package  princomp
pca <- princomp(AbalonePC, cor=TRUE )
pca
# information output 
names(pca)
summary(pca)

# eigenvalues/eigenvectors
eigenvectors <- pca$loadings # these values are scaled
eigenvalues<- pca$sdev *pca$sdev
eigenvalues

#scree plots
#Elbow method
screeplot(pca,type="l",main="Screeplot for the Abalone data")
abline(1,0 ,col='red',lty = 2)

components <- cbind(Diameter = AbalonePC[, "Diameter"], pca$x[, 1]) 
comp <- as.data.frame(components)


RM3 <- lm(Diameter ~ Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings,  data = comp)
RM3  
summary(RM3)

RM4 <- lm(Diameter ~ Height + Shucked_Weight  + SHell_Weight + Rings,  data = comp)
RM4
summary(RM4)
AIC(RM3)
AIC(RM4)
#RM3
#-------------------------------------------------------------------------------------------------------------------------------
# Tache 6 : Régression linéaire géneralisée
#-------------------------------------------------------------------------------------------------------------------------------.
# Notre variable cile 'Diameter' prend des valeurs continues et la distibution est asymetrique donc on va utiliser la distribution gamma
Model1 <- glm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings, family = Gamma(link = "inverse"))
summary(Model1)
# Nous voyons d'après les estimations des coefficients que le Shucked_weight,Whole_wieght,Viscera_weight,Shell_weight influence positivement, tandis que le déplacement a un effet légèrement négatif
# Rings a un effet légerment negatif
# Length,Height influence négativement
# Puisque le p_value de toutes les variables est < 0.05 alors toutes les variables sont significative
Model2 <- glm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings, family = Gamma(link = "log"))
summary(Model2)
AIC(Model1)
AIC(Model2)

# AIC(Model2) < AIC(Model1) alors le deuxieme modele (link=log) est le meilleur modéle generalisé 