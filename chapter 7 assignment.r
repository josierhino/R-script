#josephine rhino IS 372
setwd("C:Users/Chang/Downloads/Data files/Data files")
library(boot);
library(car);
library(QuantPsyc)


#7.4.2
album1<-read.delim("Album Sales 1.dat", header = TRUE) 
albumSales.1 <- lm(album1$sales ~ album1$adverts)
albumSales.1 <- lm(sales ~ adverts, data = album1)
 
summary(albumSales.1) 
sqrt(0.3346) 
cor.test(album1$sales, album1$adverts, method = "pearson")
#r sqrt = correlation under single predictors 

#7.8.2.2
album2<-read.delim("Album Sales 2.dat", header = TRUE)
albumSales.2 <- lm(sales ~ adverts, data = album2)
summary(albumSales.2)

albumSales.3 <- lm(sales ~ adverts + airplay + attract, data = album2)
albumSales.2<-update(albumSales.2, .~. + airplay + atrract)
summary(albumSales.3)

lm.beta(albumSales.3) 
confint(albumSales.3)

#7.8.4 - Comparing models 
anova(albumSales.2, albumSales.3)

#7.9.2 - OUtliers and influential cases  
album2$residuals<-resid(albumSales.3)
album2$standardized.residuals<- rstandard(albumSales.3)
album2$studentized.residuals<-rstudent(albumSales.3) 
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta<-dfbeta(albumSales.3)
album2$dffit<-dffits(albumSales.3)
album2$leverage<-hatvalues(albumSales.3)
album2$covariance.ratios<-covratio(albumSales.3)

round(album2, digits = 3) 
View(album2)
round(album2, digits = 3)
View(album2)

#saving our data set 
write.table(album2, "Album Sales With Diagnosticis. dat", sep="\t", row.names = FALSE)


 
album2$standardized.residuals > 2 | album2$standardized.residuals < -2

album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2

sum(album2$large.residual)
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]


#7.9.3 - independent error 
durbinWatsonTest(albumSales.3)
#can also use dwt(albumSales.3)


#mulitcollinearity 
1/vif(albumSales.3)
vif(albumSales.3)
 

histogram<-ggplot(album2, aes(studentized.residuals)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Studentized Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1) 
ggsave(file = paste("07 Sales ggplot Hist.png", sep = "/"))

qqplot.resid <- qplot(sample = album2$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values") 
qqplot.resid


hist(album2$studentized.residuals)
hist(rstudent(albumSales.3)) 

bootReg <- function (formula, data, indices)
{
  d <- data[indices,]
  fit <- lm(formula,data=d)
  return(coef(fit))
}

bootResults<-boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = album2, R = 2000)
boot.ci(bootResults, type = "bca", index = 1)
#adverts 
boot.ci(bootResults, type = "bca", index = 2)
#airplay
boot.ci(bootResults, type = "bca", index = 3)
#attract 
boot.ci(bootResults, type = "bca", index = 4)


gfr<-read.delim(file="GlastonburyFestivalRegression.dat", header = TRUE)
gfr$music = as.factor(gfr$music)
contrasts(gfr$music)<-contr.treatment(4, base = 4)
gfr$music
crusty_v_NMA<-c(1, 0, 0, 0)
indie_v_NMA<-c(0, 1, 0, 0)
metal_v_NMA<-c(0, 0, 1, 0)
contrasts(gfr$music)<-cbind(crusty_v_NMA, indie_v_NMA, metal_v_NMA)

glastonburyModel<-lm(change ~ music, data = gfr)
summary(glastonburyModel)
lm(change ~ music, data = gfr)
round(tapply(gfr$change, gfr$music, mean, na.rm=TRUE), 3)
