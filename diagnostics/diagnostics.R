## Model Diagnostics

print("Outlier and Influential Observations Diagnostics")
print(outlierTest(mod3))
#Reports the Bonferroni p-values for Studentized residuals in linear and 
#generalized linear models, based on a t-test for linear models and 
#normal-distribution test for generalized linear models.
qqPlot(mod3, main="QQ Plot")

leveragePlots(mod3) # basically the same as avPlots(mod3)
#These functions display a generalization, due to Sall (1990) and Cook and Weisberg (1991), 
#of added-variable plots to multiple-df terms in a linear model. When a term has just 1 df, 
#the leverage plot is a rescaled version of the usual added-variable (partial-regression) plot.

## Cook's D plot, identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(data)-length(mod3$coefficients)-2)) 
plot(mod3, which=4, cook.levels=cutoff)

print("Non-normality (of residuals) Diagnostics")
# qq plot for studentized resid
qqPlot(mod3, main="QQ Plot")
sresid <- MASS::studres(mod3) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

print("Homoskedasticity Diagnostics")
spreadLevelPlot(mod3)
print(ncvTest(mod3)) # non-constant error variance test
#Computes a score test of the hypothesis of constant error variance against 
#the alternative that the error variance changes with the level of the response 
#(fitted values), or with a linear combination of predictors.


print("Multicollinearity Diagnostics")
print(vif(mod3)) # variance inflation factors 
sqrt(vif(mod3)) > 2 # problem?


print("Nonlinearity Diagnostics")
crPlots(mod3) # component + residual plot 

print("Durbin Watson Test for Autocorrelated Errors")
print(durbinWatsonTest(mod3))
