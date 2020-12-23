# question1
dim(nhanes)
sum=0
for (i in 1:25){
  if (any(is.na(nhanes[i, ]))==TRUE) {
    sum=sum+1
  }
  i+1
}
# the percentage of the incomplete cases is 48%
incom_per=sum/25

# question2
# lambda represents the proportions of variance
ests_seed1=pool(with(mice(nhanes,printFlag=FALSE,seed=1),lm(bmi~age+hyp+chl)))
# parameter age -  to be most affected by the non-response
ests_seed1$pooled$lambda

# question3
# not the same
ests_seed2=pool(with(mice(nhanes,printFlag=FALSE,seed=2),lm(bmi~age+hyp+chl)))
# parameter age -  to be most affected by the non-response
ests_seed2$pooled$lambda

ests_seed3=pool(with(mice(nhanes,printFlag=FALSE,seed=3),lm(bmi~age+hyp+chl)))
# parameter age -  to be most affected by the non-response
ests_seed3$pooled$lambda

ests_seed4=pool(with(mice(nhanes,printFlag=FALSE,seed=4),lm(bmi~age+hyp+chl)))
# parameter chl -  to be most affected by the non-response
ests_seed4$pooled$lambda

ests_seed5=pool(with(mice(nhanes,printFlag=FALSE,seed=5),lm(bmi~age+hyp+chl)))
# parameter hyp -  to be most affected by the non-response
ests_seed5$pooled$lambda

ests_seed6=pool(with(mice(nhanes,printFlag=FALSE,seed=6),lm(bmi~age+hyp+chl)))
# parameter age -  to be most affected by the non-response
ests_seed6$pooled$lambda

# question4
# prefer M=100
# the pooled estimates, standard errors and the bound of the intervals get more stable
# the effect of variation decreases
# the regression coefficients' corresponding standard error decrease
ests_seed1_100=pool(with(mice(nhanes,printFlag=FALSE,seed=1,m=100),lm(bmi~age+hyp+chl)))
ests_seed2_100=pool(with(mice(nhanes,printFlag=FALSE,seed=2,m=100),lm(bmi~age+hyp+chl)))
ests_seed3_100=pool(with(mice(nhanes,printFlag=FALSE,seed=3,m=100),lm(bmi~age+hyp+chl)))
ests_seed4_100=pool(with(mice(nhanes,printFlag=FALSE,seed=4,m=100),lm(bmi~age+hyp+chl)))
ests_seed5_100=pool(with(mice(nhanes,printFlag=FALSE,seed=5,m=100),lm(bmi~age+hyp+chl)))
ests_seed6_100=pool(with(mice(nhanes,printFlag=FALSE,seed=6,m=100),lm(bmi~age+hyp+chl)))

# define the helper function
sumest=function(est){
  summary=summary(est,conf.int=TRUE)
  df=data.frame("Estimate"=est$pooled$estimate,
                "proportion of variance"=est$pooled$lambda,
                "std"=summary[ ,3],
                "lq"=summary[ ,7],
                "uq"=summary[ ,8])
  rownames(df)=c("beta0","beta1","beta2","beta3")
  colnames(df)=c("Estimate","proportion of variance","standard error","2.5% quantile","97.5% quantile")
  knitr::kable(df,escape=FALSE,digits=3,
               caption="Regression coefficient estimates and 95% CI")
}

# model with m=5
sumest(ests_seed1)
sumest(ests_seed2)
sumest(ests_seed3)
sumest(ests_seed4)
sumest(ests_seed5)
sumest(ests_seed6)

# model with m=100
sumest(ests_seed1_100)
sumest(ests_seed2_100)
sumest(ests_seed3_100)
sumest(ests_seed4_100)
sumest(ests_seed5_100)
sumest(ests_seed6_100)
