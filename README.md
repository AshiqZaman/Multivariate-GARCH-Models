## Stock Market Analysis with R

### Multivariate GARCH Models

In line with Engle (2002), the DCC-GARCH can be presented as follows:

![dcc equation](https://user-images.githubusercontent.com/47462688/81708341-0e437400-9469-11ea-8cc4-d1f824c04e59.PNG)

### Import Pilot data

```{r}
price<-read.csv("price.csv")
head(price)
return<-read.csv("return.csv")
head(return)
```

### Convert into time series data

```{r}
library(zoo)
library(xts)

return=zoo(return[,-1], order.by=as.Date(strptime(as.character(return[,1]), "%d/%m/%Y")))
head(return)
dcc<-as.xts(return)
```

### Install packages

```{r}
library(quantmod)
library(rugarch)
library(rmgarch)
```
### Univariate GARCH Model

Here we are using the functionality provided by the rugarch package written by Alexios Galanos.

### Model Specification

The first thing you need to do is to ensure you know what type of GARCH model you want to estimate and then let R know about this. It is the ugarchspec( ) function which is used to let R know about the model type. There is in fact a default specification and the way to invoke this is as follows

```{r}
ug_spec = ugarchspec()
```
### ug_spec is now a list which contains all the relevant model specifications. Let's look at them:

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

### Model Estimation

Now that we have specified a model to estimate we need to find the best parameters, i.e. we need to estimate the model. This step is achieved by the ugarchfit function.

```{r}
ugfit = ugarchfit(spec = ug_spec, data = return)
```

### Model Set up
Here we assume that we are using the same univariate volatility model specification for each of the three assets.

### DCC (MVN)

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

### Model Estimation
Now we are in a position to estimate the model using the dccfit function.

```{r}
fit1 = dccfit(spec1, data = return, fit.control = list(eval.se = TRUE), fit = multf)
```

We want to estimate the model as specified in spec1, using the data in rX. The option fit.control = list(eval.se = TRUE) ensures that the estimation procedure produces standard errors for estimated parameters. Importantly fit = multf indicates that we ought to use the already estimated univariate models as they were saved in multf. The way to learn how to use these functions is by a combination of looking at the functions's help (?dccfit) and googling.

When you estimate a multivariate volatility model like the DCC model you are typically interested in the estimated covariance or correlation matrices. After all it is at the core of these models that you allow for time-variation in the correlation between the assets (there are also constant correlation models, but we do not discuss this here). Therefore we will now learn how we extract these.

### Get the model based time varying covariance (arrays) and correlation matrices

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

![cor_BG](https://user-images.githubusercontent.com/47462688/81700237-50b48300-9460-11ea-8074-1f24f9dedddb.PNG)

### Pre analysis
Import Pilot data

```{r}
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
```
If you transformed cor_BG to be a xts series the plot function automatically picks up the date information. As you can see there is significant variation through time with the correaltion typically varying between 0.2 and 0.5.

Let's plot all three correlations between the three assets.

```{r}
par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,]),main="STOXX50 & DAX30")
plot(as.xts(cor1[1,3,]),main="STOXX50 & CAC40")
plot(as.xts(cor1[2,3,]),main="DAX30 & CAC40")
```
![dcc1](https://user-images.githubusercontent.com/47462688/81701775-5c08ae00-9462-11ea-9e91-de78bcdcd8d9.PNG)
![dcc2](https://user-images.githubusercontent.com/47462688/81701789-6165f880-9462-11ea-9578-d84f83efcc08.PNG)
![dcc3](https://user-images.githubusercontent.com/47462688/81701817-69259d00-9462-11ea-95d0-9f623500a975.PNG)
