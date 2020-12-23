load("dataex2.Rdata")
beta1=3
count_fix=0
for (i in 1:100){
  imp_fix=mice(dataex2[ , ,i],method="norm.nob",printFlag=FALSE,seed=1,m=20)
  fits=with(imp_fix,lm(Y~X))
  ests=pool(fits)
  lp=summary(ests,conf.int=TRUE)[ ,7][2]
  up=summary(ests,conf.int=TRUE)[ ,8][2]
  if (beta1<up & beta1>lp){
    count_fix=count_fix+1
  }
  i+1
}
# the empirical coverage probability of the 95% confidence intervals for beta1 
# under the stochastic regression imputation is 88%
emp_covp_fix=count_fix/100

count_uncer=0
for (i in 1:100){
  imp_uncer=mice(dataex2[ , ,i],method="norm.boot",printFlag=FALSE,seed=1,m=20)
  fits=with(imp_uncer,lm(Y~X))
  ests=pool(fits)
  lp=summary(ests,conf.int=TRUE)[ ,7][2]
  up=summary(ests,conf.int=TRUE)[ ,8][2]
  if (beta1<up & beta1>lp){
    count_uncer=count_uncer+1
  }
  i+1
}
# the empirical coverage probability of the 95% confidence intervals for beta1 
# under the bootstrap is 95%
emp_covp_uncer=count_uncer/100

emp_covp_fix
emp_covp_uncer
