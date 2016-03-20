## Amelia R script
## Biostatistics Methods 2
## created by Nicholas Reich, March 2015


## load libraries
library(Amelia)
library(Zelig)
library(openintro)
library(ggplot2)
library(Hmisc)

## load data, look at missingness
data(ncbirths)
missmap(ncbirths)

## run linear model
m1 <- lm(weight ~ fage + mage + weeks + visits + marital + gained + 
             gender + habit + whitemom, data = ncbirths)
summary(m1)

## boxcox to transform predictor?
boxcox(m1, plotit=T)
bc <- boxcox(m1, plotit=T, lambda=seq(1.1, 1.6, by=.01))
lam <- with(bc, x[which(y==max(y))])

## plot data
qplot(gained, weight^lam, data=ncbirths) + geom_smooth()
qplot(gained, weight, data=ncbirths)
qplot(visits, weight, data=ncbirths)

## impute data
imp_ncb <- amelia(x = ncbirths, m = 10,  
                  noms = c("mature", "premie", "lowbirthweight", 
                           "marital", "gender", "habit", "whitemom"))

imp_ncb <- amelia(x = ncbirths, m = 10,  
                  noms = c("mature", "premie", "lowbirthweight", 
                           "marital", "gender", "habit", "whitemom"),
                  logs="gained")

## plot imputed data
i=1
one_imp <- imp_ncb$imputations[[i]]$gained
true_data <- ncbirths$gained

hist(true_data, prob=TRUE, breaks=20, xlim=c(-20, 90))
lines(density(true_data[!is.na(true_data)]))
rug(one_imp[is.na(true_data)], col="red", ticksize=.2)

plot(imp_ncb)

## showing how imputations would do on observed data had it been missing.
overimpute(imp_ncb, var = "gained")
overimpute(imp_ncb, var = "fage")
overimpute(imp_ncb, var = "visits")

## run model with imputed data
imp_models <- zelig(weight ~ fage + mage + weeks + visits + marital + 
                        gained + gender + habit + whitemom, 
                    data = imp_ncb, model = "ls")

## print model summaries
aa <- summary(imp_models)
print(summary(imp_models), subset=2)

## compare to original model estimates
