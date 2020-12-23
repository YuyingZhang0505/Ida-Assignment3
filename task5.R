load("NHANES2.Rdata")
library(JointAI)
library(devtools)
library(reshape2)
library(RColorBrewer)
library(ggplot2)

dim(NHANES2)
str(NHANES2)
summary(NHANES2)
# inspect the missing data pattern 
md_pattern(NHANES2,pattern=FALSE,color=c('#34111b','#e30f41'))
# visualize how the observed parts of the incomplete variables are distributed
par(mar=c(3,3,2,1),mgp=c(2,0.6,0))
plot_all(NHANES2,breaks=25,ncol=4)

# imputation step
imp5=mice(NHANES2,maxit=20,m=30,seed=11,printFlag=FALSE)
imp5$loggedEvents

# check converge
plot(imp5,layout=c(4,4))
densityplot(imp5)

source_url("https://gist.githubusercontent.com/NErler/0d00375da460dd33839b98faeee2fdab/raw/c6f537ecf80eddcefd94992ec7926aa57d454536/propplot.R")
propplot(imp5)

# analysis model
fits5=with(imp5,lm(wgt~gender+age+hgt+WC))
comp1=complete(imp5,1)
# fitted values versus residuals
par(mar=c(4,4,2,4))
plot(fits5$analyses[[1]]$fitted.values,residuals(fits5$analyses[[1]]),
     xlab="Fitted values",ylab="Residuals")
# QQplot
qqnorm(rstandard(fits5$analyses[[1]]),xlim=c(-4,4),ylim=c(-6,6))
qqline(rstandard(fits5$analyses[[1]]),col=2)

# combine results
pool_ests=pool(fits5)
summary5=summary(pool_ests,conf.int=TRUE)
df5=data.frame("Estimate"=summary5[ ,2],
               "lq"=summary5[ ,7],
               "uq"=summary5[ ,8])
rownames(df5)=c("beta0","beta1","beta2","beta3","beta4")
colnames(df5)=c("Estimate","2.5% quantile","97.5% quantile")
knitr::kable(df5,escape=FALSE,digits=3,
             caption="Regression coefficient estimates and 95% CI")

# evaluate model fit
pool.r.squared(pool_ests,adjusted=TRUE)
