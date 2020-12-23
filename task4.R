load("dataex4.Rdata")
library(mice)
library(knitr)

n=1000
x1=rnorm(n,0,1)
x2=rnorm(n,1.5,1)

# question1
ests1=pool(with(mice(dataex4,printFlag=FALSE,seed=1,m=50),lm(y~x1+x2+x1*x2)))
summary1=summary(ests1,conf.int=TRUE)
df1=data.frame("Estimate"=summary1[ ,2][2:4],
               "lq"=summary1[ ,7][2:4],
               "uq"=summary1[ ,8][2:4])
rownames(df1)=c("beta1","beta2","beta3")
colnames(df1)=c("Estimate","2.5% quantile","97.5% quantile")
knitr::kable(df1,escape=FALSE,digits=3,
             caption="Regression coefficient estimates and 95% CI")

# question2
# append a new variable to dataset
x3=dataex4[ ,2]*dataex4[ ,3]
dataex4_2=cbind(dataex4,x3)
imp0=mice(dataex4_2,printFlag=FALSE,seed=1,m=50)
# change the imputation method of x3 variable
meth=imp0$method
meth["x3"]="~I(x1*x2)"
pred=imp0$predictorMatrix
pred[c("x1","x2"),"x3"]=0
imp2=mice(dataex4_2,method=meth,predictorMatrix=pred,printFlag=FALSE,seed=1,m=50)
imp2$loggedEvents
# check convergence
plot(imp2,layout=c(2,3))
# combine the results
ests2=pool(with(imp2,lm(y~x1+x2+x3)))
summary2=summary(ests2,conf.int=TRUE)
df2=data.frame("Estimate"=summary2[ ,2][2:4],
               "lq"=summary2[ ,7][2:4],
               "uq"=summary2[ ,8][2:4])
rownames(df2)=c("beta1","beta2","beta3")
colnames(df2)=c("Estimate","2.5% quantile","97.5% quantile")
knitr::kable(df2,escape=FALSE,digits=3,
             caption="Regression coefficient estimates and 95% CI")

# question3
ests3=pool(with(mice(dataex4_2,printFlag=FALSE,seed=1,m=50),lm(y~x1+x2+x3)))
summary3=summary(ests3,conf.int=TRUE)
df3=data.frame("Estimate"=summary3[ ,2][2:4],
               "lq"=summary3[ ,7][2:4],
               "uq"=summary3[ ,8][2:4])
rownames(df3)=c("beta1","beta2","beta3")
colnames(df3)=c("Estimate","2.5% quantile","97.5% quantile")
knitr::kable(df3,escape=FALSE,digits=3,
             caption="Regression coefficient estimates and 95% CI")
