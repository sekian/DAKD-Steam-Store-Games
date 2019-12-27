steam = read.csv('D:/FIB/MIRI/DAKD/DAKD Project/steam.csv')

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

?barplot
