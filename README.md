## Stock Market Analysis with R

## Import Pilot data
```{r}
price<-read.csv("price.csv")
head(price)
return<-read.csv("return.csv")
head(return)
```

## Convert into time series data


```{r}
library(zoo)
library(xts)

return=zoo(return[,-1], order.by=as.Date(strptime(as.character(return[,1]), "%d/%m/%Y")))
head(return)
dcc<-as.xts(return)
```

## Install packages
```{r}
library(quantmod)
library(rugarch)
library(rmgarch)
```
## Univariate GARCH Model
Here we are using the functionality provided by the rugarch package written by Alexios Galanos.

## Model Specification
The first thing you need to do is to ensure you know what type of GARCH model you want to estimate and then let R know about this. It is the ugarchspec( ) function which is used to let R know about the model type. There is in fact a default specification and the way to invoke this is as follows

```{r}
ug_spec = ugarchspec()
```
## ug_spec is now a list which contains all the relevant model specifications. Let's look at them:
```{r}
ug_spec
```


The key issues here are the spec for the Mean Model (here an ARMA(1,1) model) and the specification for the GARCH Model, here an sGARCH(1,1) which is basically a GARCH(1,1). To get details on all the possible specifications and how to change them it is best to consult the documentation of the rugarch package.

Let's say you want to change the mean model from an ARMA(1,1) to an ARMA(1,0), i.e. an AR(1) model.

```{r}
ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
ug_spec
```

```{r}
ewma_spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
        distribution.model="norm", fixed.pars=list(omega=0))
```

## Model Estimation
Now that we have specified a model to estimate we need to find the best parameters, i.e. we need to estimate the model. This step is achieved by the ugarchfit function.

```{r}
ugfit = ugarchfit(spec = ug_spec, data = return)
```

## Model Set up
Here we assume that we are using the same univariate volatility model specification for each of the three assets.

# DCC (MVN)

```{r}
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
```

What does this command do? You will recognise that ugarchspec(mean.model = list(armaOrder = c(1,0))) specifies an AR(1)-GARCH(1,1) model. By using replicate(3, ugarchspec...) we replicate this model 3 times (as we have three assets, IBM, Google/Alphabet and BP).

We now estimate these univariate GARCH models using the multifit command.

```{r}
multf = multifit(uspec.n,return)
```

The results are saved in multf and you can type multf into the command window to see the estimated parameters for these three models. But we will here proceed to specify the DCC model (I assume that you know what a DCC model is. This is not the place to elaborate on this and many textbooks or indeed the documentation to this package provide details). To specify the correlation specification we use the dccspec function.

```{r}
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
```

In this specification we have to state how the univariate volatilities are modeled (as per uspec.n) and how complex the dynamic structure of the correlation matrix is (here we are using the most standard dccOrder = c(1, 1) specification).

## Model Estimation
Now we are in a position to estimate the model using the dccfit function.

```{r}
fit1 = dccfit(spec1, data = return, fit.control = list(eval.se = TRUE), fit = multf)
```

We want to estimate the model as specified in spec1, using the data in rX. The option fit.control = list(eval.se = TRUE) ensures that the estimation procedure produces standard errors for estimated parameters. Importantly fit = multf indicates that we ought to use the already estimated univariate models as they were saved in multf. The way to learn how to use these functions is by a combination of looking at the functions's help (?dccfit) and googling.

When you estimate a multivariate volatility model like the DCC model you are typically interested in the estimated covariance or correlation matrices. After all it is at the core of these models that you allow for time-variation in the correlation between the assets (there are also constant correlation models, but we do not discuss this here). Therefore we will now learn how we extract these.

# Get the model based time varying covariance (arrays) and correlation matrices

```{r}
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
```

To understand the object we have at our hands here we can have a look at the imension:

```{r}
dim(cor1)
```
[1]    3    3 2850

We get three outputs which tells us that we have a three dimensional object. The firts two dimensions have 3 elements each (think a 3×3 correlation matrix) and then there is a third dimension with 2850 elements. This tells us that cor1 stores 2850 (3×3) sorrelation matrices, one for each day of data.

Let's have a look at the correlation matrix for the last day, day 2853;

```{r}
cor1[,,dim(cor1)[3]]
```
```{r}
         STOXX50     DAX30     CAC40
STOXX50 1.0000000 0.9754082 0.9863061
DAX30   0.9754082 1.0000000 0.9571282
CAC40   0.9863061 0.9571282 1.0000000
```


So let's say we want to plot the time-varying correlation between Google and BP, which is 0.275244 on that last day. In our matrix with returns rX BP is the second asset and Google the 3rd. So in any particular correlation matrix we want the element in row 2 and column 3.

```{r}
cor_BG <- cor1[2,1,]   # leaving the last dimension empty implies that we want all elements
cor_BG <- as.xts(cor_BG)  # imposes the xts time series format - useful for plotting
```

And now we plot this.
```{r}
plot(cor_BG)


```


Pre analysis
Import Pilot data
price<-read.csv("price.csv")
head(price)
##         Date  STOXX50    DAX30    CAC40
## 1 01/01/2010 7.994619 8.692394 8.278004
## 2 04/01/2010 8.012283 8.707533 8.297536
## 3 05/01/2010 8.010479 8.704811 8.297272
## 4 06/01/2010 8.009582 8.705220 8.298457
## 5 07/01/2010 8.008811 8.702736 8.300230
## 6 08/01/2010 8.012300 8.705764 8.305271
return<-read.csv("return.csv")
head(return)
##         Date      STOXX50        DAX30        CAC40
## 1 04/01/2010  0.017664547  0.015138061  0.019531959
## 2 05/01/2010 -0.001804264 -0.002721820 -0.000264112
## 3 06/01/2010 -0.000896709  0.000409408  0.001185469
## 4 07/01/2010 -0.000771149 -0.002483888  0.001773087
## 5 08/01/2010  0.003488690  0.003027297  0.005040941
## 6 11/01/2010 -0.002524847  0.000478552 -0.000506910
Convert into time series data
library(zoo)
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
library(xts)

return=zoo(return[,-1], order.by=as.Date(strptime(as.character(return[,1]), "%d/%m/%Y")))
head(return)
##                 STOXX50        DAX30        CAC40
## 2010-01-04  0.017664547  0.015138061  0.019531959
## 2010-01-05 -0.001804264 -0.002721820 -0.000264112
## 2010-01-06 -0.000896709  0.000409408  0.001185469
## 2010-01-07 -0.000771149 -0.002483888  0.001773087
## 2010-01-08  0.003488690  0.003027297  0.005040941
## 2010-01-11 -0.002524847  0.000478552 -0.000506910
dcc<-as.xts(return)
Install packages
library(quantmod)
## Loading required package: TTR
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
## Version 0.4-0 included new data defaults. See ?getSymbols.
library(rugarch)
## Loading required package: parallel
## 
## Attaching package: 'rugarch'
## The following object is masked from 'package:stats':
## 
##     sigma
library(rmgarch)
## 
## Attaching package: 'rmgarch'
## The following objects are masked from 'package:xts':
## 
##     first, last
Univariate GARCH Model
Here we are using the functionality provided by the rugarch package written by Alexios Galanos.

##Model Specification The first thing you need to do is to ensure you know what type of GARCH model you want to estimate and then let R know about this. It is the ugarchspec( ) function which is used to let R know about the model type. There is in fact a default specification and the way to invoke this is as follows

ug_spec = ugarchspec()
ug_spec is now a list which contains all the relevant model specifications. Let’s look at them:
ug_spec
## 
## *---------------------------------*
## *       GARCH Model Spec          *
## *---------------------------------*
## 
## Conditional Variance Dynamics    
## ------------------------------------
## GARCH Model      : sGARCH(1,1)
## Variance Targeting   : FALSE 
## 
## Conditional Mean Dynamics
## ------------------------------------
## Mean Model       : ARFIMA(1,0,1)
## Include Mean     : TRUE 
## GARCH-in-Mean        : FALSE 
## 
## Conditional Distribution
## ------------------------------------
## Distribution :  norm 
## Includes Skew    :  FALSE 
## Includes Shape   :  FALSE 
## Includes Lambda  :  FALSE
The key issues here are the spec for the Mean Model (here an ARMA(1,1) model) and the specification for the GARCH Model, here an sGARCH(1,1) which is basically a GARCH(1,1). To get details on all the possible specifications and how to change them it is best to consult the documentation of the rugarch package.

Let’s say you want to change the mean model from an ARMA(1,1) to an ARMA(1,0), i.e. an AR(1) model.

ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
ug_spec
## 
## *---------------------------------*
## *       GARCH Model Spec          *
## *---------------------------------*
## 
## Conditional Variance Dynamics    
## ------------------------------------
## GARCH Model      : sGARCH(1,1)
## Variance Targeting   : FALSE 
## 
## Conditional Mean Dynamics
## ------------------------------------
## Mean Model       : ARFIMA(1,0,0)
## Include Mean     : TRUE 
## GARCH-in-Mean        : FALSE 
## 
## Conditional Distribution
## ------------------------------------
## Distribution :  norm 
## Includes Skew    :  FALSE 
## Includes Shape   :  FALSE 
## Includes Lambda  :  FALSE
ewma_spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
        distribution.model="norm", fixed.pars=list(omega=0))
Model Estimation
Now that we have specified a model to estimate we need to find the best parameters, i.e. we need to estimate the model. This step is achieved by the ugarchfit function.

ugfit = ugarchfit(spec = ug_spec, data = return)
Model Set up
Here we assume that we are using the same univariate volatility model specification for each of the three assets.

DCC (MVN)
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
What does this command do? You will recognise that ugarchspec(mean.model = list(armaOrder = c(1,0))) specifies an AR(1)-GARCH(1,1) model. By using replicate(3, ugarchspec…) we replicate this model 3 times (as we have three assets, IBM, Google/Alphabet and BP).

We now estimate these univariate GARCH models using the multifit command.

multf = multifit(uspec.n,return)
The results are saved in multf and you can type multf into the command window to see the estimated parameters for these three models. But we will here proceed to specify the DCC model (I assume that you know what a DCC model is. This is not the place to elaborate on this and many textbooks or indeed the documentation to this package provide details). To specify the correlation specification we use the dccspec function.

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
In this specification we have to state how the univariate volatilities are modeled (as per uspec.n) and how complex the dynamic structure of the correlation matrix is (here we are using the most standard dccOrder = c(1, 1) specification).

Model Estimation
Now we are in a position to estimate the model using the dccfit function.

fit1 = dccfit(spec1, data = return, fit.control = list(eval.se = TRUE), fit = multf)
We want to estimate the model as specified in spec1, using the data in rX. The option fit.control = list(eval.se = TRUE) ensures that the estimation procedure produces standard errors for estimated parameters. Importantly fit = multf indicates that we ought to use the already estimated univariate models as they were saved in multf. The way to learn how to use these functions is by a combination of looking at the functions’s help (?dccfit) and googling.

When you estimate a multivariate volatility model like the DCC model you are typically interested in the estimated covariance or correlation matrices. After all it is at the core of these models that you allow for time-variation in the correlation between the assets (there are also constant correlation models, but we do not discuss this here). Therefore we will now learn how we extract these.

Get the model based time varying covariance (arrays) and correlation matrices
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
To understand the object we have at our hands here we can have a look at the imension:

dim(cor1)
## [1]    3    3 2698
[1] 3 3 2850
We get three outputs which tells us that we have a three dimensional object. The firts two dimensions have 3 elements each (think a 3×3 correlation matrix) and then there is a third dimension with 2850 elements. This tells us that cor1 stores 2850 (3×3) sorrelation matrices, one for each day of data.

Let’s have a look at the correlation matrix for the last day, day 2853;

cor1[,,dim(cor1)[3]]
##           STOXX50     DAX30     CAC40
## STOXX50 1.0000000 0.9754082 0.9863061
## DAX30   0.9754082 1.0000000 0.9571282
## CAC40   0.9863061 0.9571282 1.0000000
So let’s say we want to plot the time-varying correlation between Google and BP, which is 0.275244 on that last day. In our matrix with returns rX BP is the second asset and Google the 3rd. So in any particular correlation matrix we want the element in row 2 and column 3.

cor_BG <- cor1[2,1,]   # leaving the last dimension empty implies that we want all elements
cor_BG <- as.xts(cor_BG)  # imposes the xts time series format - useful for plotting
And now we plot this.

plot(cor_BG)


If you transformed cor_BG to be a xts series the plot function automatically picks up the date information. As you can see there is significant variation through time with the correaltion typically varying between 0.2 and 0.5.

Let’s plot all three correlations between the three assets.

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,]),main="STOXX50 & DAX30")
plot(as.xts(cor1[1,3,]),main="STOXX50 & CAC40")
plot(as.xts(cor1[2,3,]),main="DAX30 & CAC40")


Forecasts Often you will want to use your estimated model to produce forecasts for the covariance or correlation matrix

dccf1 <- dccforecast(fit1, n.ahead = 10)
dccf1
## 
## *---------------------------------*
## *       DCC GARCH Forecast        *
## *---------------------------------*
## 
## Distribution         :  mvnorm
## Model                :  DCC(1,1)
## Horizon              :  10
## Roll Steps           :  0
## -----------------------------------
## 
## 0-roll forecast: 
## 
## First 2 Correlation Forecasts
## , , 1
## 
##        [,1]   [,2]   [,3]
## [1,] 1.0000 0.9751 0.9862
## [2,] 0.9751 1.0000 0.9566
## [3,] 0.9862 0.9566 1.0000
## 
## , , 2
## 
##        [,1]   [,2]   [,3]
## [1,] 1.0000 0.9744 0.9859
## [2,] 0.9744 1.0000 0.9557
## [3,] 0.9859 0.9557 1.0000
## 
## . . .
## . . .
## 
## Last 2 Correlation Forecasts
## , , 1
## 
##        [,1]   [,2]   [,3]
## [1,] 1.0000 0.9705 0.9844
## [2,] 0.9705 1.0000 0.9499
## [3,] 0.9844 0.9499 1.0000
## 
## , , 2
## 
##        [,1]   [,2]   [,3]
## [1,] 1.0000 0.9700 0.9842
## [2,] 0.9700 1.0000 0.9492
## [3,] 0.9842 0.9492 1.0000
The actual forecasts for the correlation can be addresse via

Rf <- dccf1@mforecast$R    # use H for the covariance forecast
When checking the structure of Rf

str(Rf)
## List of 1
##  $ : num [1:3, 1:3, 1:10] 1 0.975 0.986 0.975 1 ...
you realise that the object Rf is a list with one element. It turns out that this one list item is then a 3 dimensional matrix/array which contains the the 10 forecasts of 3×3 correlation matrices. If we want to extract, say, the 10 forecasts for the correlation between IBM (1st asset) and BP (2nd asset), we have to do this in the following way:

corf_IB <- Rf[[1]][1,2,]  # Correlation forecasts between IBM and BP
corf_IG <- Rf[[1]][1,3,]  # Correlation forecasts between IBM and Google
corf_BG <- Rf[[1]][2,3,]  # Correlation forecasts between BP and Google
[ [1] ] tells R to go to the first (and here only) list item and then [1,2,] instructs R to select the (1,2) element of all available correlation matrices.

As for the univariate volatililty model let us display the forecast along with the last in-sample estimates of correlation.

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
c_IB <- c(tail(cor1[1,2,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_IB <- c(rep(NA,20),corf_IB) # gets the 10 forecasts
plot(c_IB,type = "l",main="Correlation IBM and BP")
lines(cf_IB,type = "l", col = "orange")

c_IG <- c(tail(cor1[1,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_IG <- c(rep(NA,20),corf_IG) # gets the 10 forecasts
plot(c_IG,type = "l",main="Correlation IBM and Google")
lines(cf_IG,type = "l", col = "orange")

c_BG <- c(tail(cor1[2,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_BG <- c(rep(NA,20),corf_BG) # gets the 10 forecasts
plot(c_BG,type = "l",main="Correlation BP and Google")
lines(cf_BG,type = "l", col = "orange")
