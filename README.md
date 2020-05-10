## Stock Market Analysis with R

##Data upload: Here we will use a convenient data retrieval function (getSymbols) delivered by the quantmod package in order to retrieve some data. This function works, for instance, to retrieve stock data. The default source is Yahoo Finance. If you want to find out what stock has which symbol you should be able to search the internet to find a list of ticker symbols. The following shows how to use the function. But note that my experience is that sometimes the connection does not work and you may get an error message. In that case just retry a few seconds later and it may well work.

```{r}
startDate = as.Date("2007-01-03") #Specify period of time we are interested in
endDate = as.Date("2018-04-30")
 
getSymbols("IBM", from = startDate, to = endDate)
getSymbols("GOOG", from = startDate, to = endDate)
getSymbols("BP", from = startDate, to = endDate)
```
##In your environment you can see that each of these commands loads an object with the respective ticker symbol name. Let's have a look at one of these dataframes to understand what data these are:

```{r}
head(IBM)
str(IBM)
chartSeries(GOOG)
```

## When we are estimating volatility models we work with returns. There is a function that transforms the data to returns.
```{r}
rIBM <- dailyReturn(IBM)
rBP <- dailyReturn(BP)
rGOOG <- dailyReturn(GOOG)
```

## We put all data into a data frame for use in the multivariate model

```{r}
rX <- data.frame(rIBM, rBP, rGOOG)
names(rX)[1] <- "rIBM"
names(rX)[2] <- "rBP"
names(rX)[3] <- "rGOOG"
```

## Univariate GARCH Model
Here we are using the functionality provided by the rugarch package written by Alexios Galanos.

##Model Specification
The first thing you need to do is to ensure you know what type of GARCH model you want to estimate and then let R know about this. It is the ugarchspec( ) function which is used to let R know about the model type. There is in fact a default specification and the way to invoke this is as follows

```{r}
ug_spec = ugarchspec()
```
## ug_spec is now a list which contains all the relevant model specifications. Let's look at them:

```{r}
ug_spec
```



# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/AshiqZaman/hello-world/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
