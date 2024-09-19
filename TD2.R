# importations de csv
# Afficher le repertoire
getwd()

# Changer de rpertoire
setwd(dir = "L:/BUT/SD/Promo 2023/mnkoghe/R studio/dataset")
getwd()

# les datasets
anime <-read.csv("anime.csv",header=TRUE,dec =".",sep=",")
bodies_karts <- read.csv("bodies_karts.csv",header = TRUE,dec= ",",sep=";")
tires <- read.csv("tires.csv",header = TRUE,dec=",",sep= "\t" )
gliders <- read.csv("gliders.csv",header = TRUE, sep="|",dec = ".")
drivers <- read.csv("drivers.csv",header =TRUE,dec=",",sep=";" )

#Afficher les dimensions

dim_anime<- dim(anime)
dim_bodies<- dim(bodies_karts)
dim_tires<- dim(tires)
dim_gliders<- dim(gliders)
dim_deivers<- dim(drivers)

# résumé de données

summary(anime)
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

# Faire un nuage de point
nuage_point <- plot(x= drivers$Weight,
                    y= drivers$Acceleration, main = "Nuage de point de Mesmin")

# Calcul du coef de corrélation
correlation <- cor(x=drivers$Weight,
                   y=drivers$Acceleration,method = "spearman")

covariance<- cov(x= drivers$Weight,
                y= drivers$Acceleration)
sX <- sd(drivers$Weight)
sY <- sd(drivers$Acceleration)

print(covariance/(sX*sY))

# coef de détermination
coef_det <- correlation^2

# Construction de matrice des correlations
matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.

# installer un package
install.packages("corrplot")

# Construire un corrélogramme
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor,method= "circle",main= "Method de Mesmin :)")

resultat <- drivers[ , c("Driver" , "Weight")]
View(resultat)

resultat <- drivers[ , c("Driver" , "Acceleration")]
View(resultat)

resultat <- drivers[,-c(5,7,9)]
View(resultat)

resultat <- drivers[,-c("Weight","Acceleration")] #cela fonctionne uniquement sur des index numériques.
resultat <- drivers[,-c(2,3)]
View(resultat)

resultat <- drivers[c("Driver","Acceleration","Weight")]
View(resultat)

