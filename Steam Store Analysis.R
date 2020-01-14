steam = read.csv('D:/FIB/MIRI/DAKD/DAKD Project/steam-clean.csv')

## PCA OF BASE COLUMNS ##
library(Rcmdr)
library(RcmdrPlugin.FactoMineR)

steam.PCA <- steam[, c("appid", "required_age", "achievements", "positive_ratings", 
                       "negative_ratings", "average_playtime", "owners", "price", "release_year", "linux", "mac")]
res <- PCA(steam.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE)
plot.PCA(res, axes = c(1, 2), choix = "ind", habillage = "none", col.ind = "black", 
         col.ind.sup = "blue", col.quali = "magenta", label = c("ind", "ind.sup", "quali"), 
         new.plot = TRUE)
plot.PCA(res, axes = c(1, 2), choix = "var", new.plot = TRUE, col.var = "black", 
         col.quanti.sup = "blue", label = c("var", "quanti.sup"), lim.cos2.var = 0)
summary(res, nb.dec = 3, nbelements = 10, nbind = 10, ncp = 3, file = "")
remove(steam.PCA)

## PCA OF GENRES ##

steam.PCA <- steam[, c("Indie", "Action", "Adventure", "Casual", "Strategy", "Simulation", 
                       "RPG", "Alpha", "F2P", "Sports", "Racing", "Violent", "MMO", "Gore", "Nudity")]
res <- PCA(steam.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE)
plot.PCA(res, axes = c(1, 2), choix = "ind", habillage = "none", col.ind = "black", 
         col.ind.sup = "blue", col.quali = "magenta", label = c("ind", "ind.sup", "quali"), 
         new.plot = TRUE)
plot.PCA(res, axes = c(1, 2), choix = "var", new.plot = TRUE, col.var = "black", 
         col.quanti.sup = "blue", label = c("var", "quanti.sup"), lim.cos2.var = 0)
summary(res, nb.dec = 3, nbelements = 10, nbind = 10, ncp = 3, file = "")
remove(steam.PCA)

#### PCA OF ALL

steam.PCA <- steam[, c("appid", "required_age", "achievements", "positive_ratings", 
                       "negative_ratings", "average_playtime", "owners", "price", "release_year", "linux", 
                       "mac", "Indie", "Action", "Adventure", "Casual", "Strategy", "Simulation", "RPG", 
                       "Alpha", "F2P", "Sports", "Racing", "Violent", "MMO", "Gore", "Nudity")]
res <- PCA(steam.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE)
plot.PCA(res, axes = c(1, 2), choix = "ind", habillage = "none", col.ind = "black", 
         col.ind.sup = "blue", col.quali = "magenta", label = c("ind", "ind.sup", "quali"), 
         new.plot = TRUE)
plot.PCA(res, axes = c(1, 2), choix = "var", new.plot = TRUE, col.var = "black", 
         col.quanti.sup = "blue", label = c("var", "quanti.sup"), lim.cos2.var = 0)
summary(res, nb.dec = 3, nbelements = 10, nbind = 10, ncp = 3, file = "")
remove(steam.PCA)

######## LINEAR MODEL OF PRICE (ALL COLUMNS)

RegModel.1 <- lm(price ~ achievements + Action + Adventure + Alpha + appid + average_playtime + 
                   Casual + F2P + Gore + Indie + linux + mac + MMO + negative_ratings + Nudity + 
                   owners + positive_ratings + Racing + release_year + required_age + RPG + Simulation + 
                   Sports + Strategy + Violent, data = steam)
summary(RegModel.1)

##### NEURAL NETWORK #####
library(caret)
library(nnet)
library(MASS)
set.seed(1234)

dt = sample(nrow(steam), nrow(steam)*.8)
steamTrain <- steam[dt,]
steamTest <- steam[-dt,]

train_control <- trainControl (method="cv", number=2)
decays <- 10^seq(-2,0,by=0.3)
sizes <- 10

start.time <- Sys.time()
model.nnet <- train(price~achievements + Action + Adventure + Alpha + appid + average_playtime + 
                      Casual + F2P + Gore + Indie + linux + mac + MMO + negative_ratings + Nudity + 
                      owners + positive_ratings + Racing + release_year + required_age + RPG + Simulation + 
                      Sports + Strategy + Violent,
                    data = steamTrain,
                    method  = "nnet",
                    trace = FALSE,
                    linout= TRUE,
                    trControl = train_control,
                    tuneGrid  = expand.grid(size=sizes,decay=decays))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#save(model.nnet, file = "model.nnet")
prediccio <- predict(model.nnet, steamTest)
rsq <- function (x, y) cor(x, y) ^ 2
rsq(steamTest$price, prediccio)  ## 0.3488118
RMSE(steamTest$price, prediccio) ## 6.479535


## ONE-HOT ENCODING OF COLUMNS IN R

lvl = levels(steam$genres)
dd <- steam

library(splitstackshape)
dd = concat.split.expanded(dd, "platforms", sep  = ";", type = "character")

dd = concat.split.expanded(dd, "genres", sep  = ";", type = "character")
dd = concat.split.expanded(dd, "categories", sep  = ";", type = "character")
dd = concat.split.expanded(dd, "steamspy_tags", sep  = ";", type = "character")

###HIST
TABLEV1= as.data.frame(with(steam,table(developer)))
TABLEV1 = TABLEV1[TABLEV1$Freq > 10,]
TABLEV1 <- TABLEV1[order(-TABLEV1$Freq),]

par(mar=c(10,5,5,5))
barplot(TABLEV1$Freq, names.arg = TABLEV1$developer, las=2)

#########