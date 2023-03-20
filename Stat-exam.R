library(readxl)
Bing_Dun_Dun_Adventure_Land <- read_excel("/Data/Bing Dun Dun Adventure Land (2).xlsx")

summary(BBD)
plot(`Season Ticket Holders`~Population, 
     data = Bing_Dun_Dun_Adventure_Land,
     main = "Relation of Population and Season Ticket Holders", 
     xlab = "Population",
     ylab = "STH",
     col = "red")
BBD<-lm(Bing_Dun_Dun_Adventure_Land$`Season Ticket Holders`~Bing_Dun_Dun_Adventure_Land$Population,data=Bing_Dun_Dun_Adventure_Land)
abline(BBD)

standard_res <- rstandard(BBD)
plot(Bing_Dun_Dun_Adventure_Land$Population, standard_res, ylab='Standardized Residuals', xlab='Population', col = "red") 
abline(0,0)