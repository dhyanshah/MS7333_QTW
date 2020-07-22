---
title: "United States Flu Study - Case Study: Arellano, Clark, Shah, Vaughn"
author: "Samuel Arellano, Daniel Clark, Dhyan Shah, Chandler Vaughn"
date: "2020-06-30"
output: 
  html_document:
    toc: true
    number_sections: true
    theme: united
    highlight: haddock
    df_print: paged
    keep_md: TRUE
    fig_width: 10
    fig_height: 10
    fig_retina: true
---

### Abstract

As the world adjusts to a new normal of illness prevention and caution towards viral infections, the microscope for drug and treatment companies gets larger and larger to be able to produce and supply medicines and equipment to meet the resource needs of the population. In our study, we looked into weekly cases of positive and negative test results for influenza, or most commonly known as the flu. Our dataset traced back to 2017 through June of 2020 and we saw a cyclic behavior of spikes around the Q4 - Q1 season each year, or when weather is cold in the northern hemisphere. We ran a series of models using Maximum Likelihood Estimation, Burg, Yule Walker, Extreme Learning Machine, and Multi-layer Perceptron to forecast the next 45 weeks of flu cases. After checking through 26 and 52 week seasonality periods, we were able to accurately predict historical forecasts and provide a roadmap to meet the demand for flu treatments in Q4 - Q1 of 2020/2021.

# Introduction

With infectious diseases taking the forefront of our news, culture and daily life, it’s more important than ever for drug and treatment companies to stay on top of the trends with the spread of disease to ensure they are meeting the resource needs. For cases like the flu, which has a direct seasonal spike around winter in the United States, it is critical to be able to predict these spikes accurately so you can best plan and serve your customers. In this study, we will look at data from the CDC spanning from 2017 through to 2020 that outlines the positive and negative cases of flu screenings in the U.S. We will explore some of the trends that occurred with historical data and use that to predict future flu cases 45 weeks out. 


```r
library(ggplot2)
library(tswge)
library(ggplot2)
library(ggthemes)
library(forecast)
```

```
## Warning: package 'forecast' was built under R version 3.6.2
```

```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```r
library(tseries)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(datetime)
library(caret)
```

```
## Loading required package: lattice
```

```r
library(corrplot)
```

```
## corrplot 0.84 loaded
```

```r
library(DMwR)
```

```
## Loading required package: grid
```

```r
library(Hmisc)
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
library(ROCR)
```

```
## Loading required package: gplots
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
library(stringr)
library(RVAideMemoire)
```

```
## *** Package RVAideMemoire v 0.9-75 ***
```

```
## 
## Attaching package: 'RVAideMemoire'
```

```
## The following object is masked from 'package:DMwR':
## 
##     bootstrap
```

```r
#install.packages("plotly")
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 3.6.2
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:Hmisc':
## 
##     subplot
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
#install.packages('TSstudio')
library(TSstudio)
library(doParallel)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

```
## Loading required package: parallel
```

```r
registerDoParallel(cores=16)
library(nnfor)
```

```
## Registered S3 method overwritten by 'greybox':
##   method     from
##   print.pcor lava
```

```r
library(vars)
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:plotly':
## 
##     select
```

```
## Loading required package: strucchange
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## Loading required package: sandwich
```

```
## 
## Attaching package: 'strucchange'
```

```
## The following object is masked from 'package:stringr':
## 
##     boundary
```

```
## Loading required package: urca
```

```
## Loading required package: lmtest
```

```r
library(knitr)
library(kableExtra)
ase <- function(x, xhat){
  s <- length(x) - length(xhat$f) + 1
  n <- length(x)
  mean((xhat$f-x[s:n])^2)
}
```


```r
getwd()
```

```
## [1] "/Users/chandlervaughn/Dropbox/4. Chandler/Development/git_repositories/MS7333_QTW/Case8"
```

```r
#fluDf = read.csv(file.choose(),header = TRUE)
#fluDf = read.csv("/Volumes/Dhyan-MacPC/Education/SMU /MSDS/DS 7333 Quantifying the World/Unit 8 - Case Study/flu_tidy.csv")
fluDf = read.csv("https://raw.githubusercontent.com/dhyanshah/MS7333_QTW/master/Case8/flu_tidy.csv")


head(fluDf)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["year"],"name":[1],"type":["int"],"align":["right"]},{"label":["week"],"name":[2],"type":["int"],"align":["right"]},{"label":["week.year"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Positive"],"name":[4],"type":["int"],"align":["right"]},{"label":["Negative"],"name":[5],"type":["int"],"align":["right"]}],"data":[{"1":"2017","2":"1","3":"1.2017","4":"9770","5":"32027","_rn_":"1"},{"1":"2017","2":"2","3":"2.2017","4":"11527","5":"31696","_rn_":"2"},{"1":"2017","2":"3","3":"3.2017","4":"12064","5":"31760","_rn_":"3"},{"1":"2017","2":"4","3":"4.2017","4":"12385","5":"32838","_rn_":"4"},{"1":"2017","2":"5","3":"5.2017","4":"14466","5":"34640","_rn_":"5"},{"1":"2017","2":"6","3":"6.2017","4":"17591","5":"38442","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
dim(fluDf)
```

```
## [1] 179   5
```

```r
summary(fluDf)
```

```
##       year           week         week.year         Positive      
##  Min.   :2017   Min.   : 1.00   Min.   : 1.202   Min.   :   24.0  
##  1st Qu.:2017   1st Qu.:12.00   1st Qu.:12.202   1st Qu.:  358.5  
##  Median :2018   Median :23.00   Median :23.202   Median : 1601.0  
##  Mean   :2018   Mean   :24.64   Mean   :24.839   Mean   : 6568.2  
##  3rd Qu.:2019   3rd Qu.:37.50   3rd Qu.:37.702   3rd Qu.:11047.0  
##  Max.   :2020   Max.   :52.00   Max.   :52.202   Max.   :30771.0  
##     Negative    
##  Min.   : 4440  
##  1st Qu.:10738  
##  Median :22117  
##  Mean   :24692  
##  3rd Qu.:35908  
##  Max.   :67857
```

Running a quick summary of our data frame, we can see that our model is running from the first week of 2017 through June of 2020. The dataset includes 5 columns that denote the time frame that we are specifically targeting in our study. Additionally, we have columns that call out the number of positive and negative tests that were administered during the week of study. From a positive standpoint, we are maxing out at 30k total positives with a min of 24, so we have a pretty large range that we are working with. Among positive tests, the median is at 1,601 while the mean is at 6,500, which suggests our positive tests are right skewed. 

Among our negative tests, we can see that our median and mean values per week are much closer, which shows that the negative data set is more normal than our positive tests dataset. 

### Methods

Considering we are going to be looking for the most accurate forecast of the upcoming data, we will need to be judicious to ensure that our historical data will be relevant to future data. Considering the recent changes to the supply chain and the strain of urbanization has left on companies’ ability to meet resource needs of consumers, we are choosing to use the most recent 3 years of data in our forecast. This will allow us to gather a sense of seasonality in our data across 4 quarters and to be able to see how that seasonality tends to change year over year in a broader linear trend. 

From here, we will want to account for the weekly seasonality in our dataset and forecast based on a 52 week and a 26 week seasonality period. In doing so, we will be looking forward 45 weeks to completely cover the time frame from June through the start of 2021, which will account for us to include the peak season in our final forecast. 


### Exploratory Data Analysis

## Week Year Review


```r
fig1 <- plot_ly(fluDf, x = ~Positive, y = ~week.year, name = "Positive", type = 'scatter',
               mode = "markers", marker = list(color = "pink"))
fig1 <- fig1 %>% add_trace(x = ~Negative, y = ~week.year, name = "Negative",type = 'scatter',
                         mode = "markers", marker = list(color = "blue"))
fig1 <- fig1 %>% layout(
  title = "Flu Cases",
  xaxis = list(title = "Number of Cases"),
  margin = list(l = 100)
)

fig1
```

```
## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
## Please use `arrange()` instead.
## See vignette('programming') for more help
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

<!--html_preserve--><div id="htmlwidget-f2b1ab48c1167076b405" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-f2b1ab48c1167076b405">{"x":{"visdat":{"1406e3012fd1c":["function () ","plotlyVisDat"]},"cur_data":"1406e3012fd1c","attrs":{"1406e3012fd1c":{"x":{},"y":{},"mode":"markers","marker":{"color":"pink"},"name":"Positive","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"1406e3012fd1c.1":{"x":{},"y":{},"mode":"markers","marker":{"color":"blue"},"name":"Negative","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":100,"t":25,"r":10},"title":"Flu Cases","xaxis":{"domain":[0,1],"automargin":true,"title":"Number of Cases"},"yaxis":{"domain":[0,1],"automargin":true,"title":"week.year"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"y":[1.2017,2.2017,3.2017,4.2017,5.2017,6.2017,7.2017,8.2017,9.2017,10.2017,11.2017,12.2017,13.2017,14.2017,15.2017,16.2017,17.2017,18.2017,19.2017,20.2017,21.2017,22.2017,23.2017,24.2017,25.2017,26.2017,27.2017,28.2017,29.2017,30.2017,31.2017,32.2017,33.2017,34.2017,35.2017,36.2017,37.2017,38.2017,39.2017,40.2017,41.2017,42.2017,43.2017,44.2017,45.2017,46.2017,47.2017,48.2017,49.2017,50.2017,51.2017,52.2017,1.2018,2.2018,3.2018,4.2018,5.2018,6.2018,7.2018,8.2018,9.2018,10.2018,11.2018,12.2018,13.2018,14.2018,15.2018,16.2018,17.2018,18.2018,19.2018,20.2018,21.2018,22.2018,23.2018,24.2018,25.2018,26.2018,27.2018,28.2018,29.2018,30.2018,31.2018,32.2018,33.2018,34.2018,35.2018,36.2018,37.2018,38.2018,39.2018,40.2018,41.2018,42.2018,43.2018,44.2018,45.2018,46.2018,47.2018,48.2018,49.2018,50.2018,51.2018,52.2018,1.2019,2.2019,3.2019,4.2019,5.2019,6.2019,7.2019,8.2019,9.2019,10.2019,11.2019,12.2019,13.2019,14.2019,15.2019,16.2019,17.2019,18.2019,19.2019,20.2019,21.2019,22.2019,23.2019,24.2019,25.2019,26.2019,27.2019,28.2019,29.2019,30.2019,31.2019,32.2019,33.2019,34.2019,35.2019,36.2019,37.2019,38.2019,39.2019,40.2019,41.2019,42.2019,43.2019,44.2019,45.2019,46.2019,47.2019,48.2019,49.2019,50.2019,51.2019,52.2019,1.202,2.202,3.202,4.202,5.202,6.202,7.202,8.202,9.202,10.202,11.202,12.202,13.202,14.202,15.202,16.202,17.202,18.202,19.202,20.202,21.202,22.202,23.202],"mode":"markers","marker":{"color":"pink","line":{"color":"rgba(31,119,180,1)"}},"name":"Positive","type":"scatter","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[32027,31696,31760,32838,34640,38442,39335,37663,34264,32740,31217,29746,27905,25497,22778,20956,18404,16014,15082,14136,12727,11541,10616,9556,8524,7592,7155,7519,7341,6933,6914,7058,7797,8371,9618,10519,11951,13165,14184,16070,17102,18472,19124,20540,22117,23851,23144,26598,28965,31436,38764,43049,47846,52262,52775,60927,62357,61920,57573,48353,41268,36118,32314,29552,27014,24955,23119,20202,18481,16609,14801,12996,11805,10861,9811,9074,8614,7825,6920,7075,6908,6891,6396,6867,7169,7893,8821,9930,12224,13447,15067,16975,18564,20797,21957,23086,24036,24924,24598,28467,28467,31004,34092,37141,40210,37747,36444,36881,39146,42040,40913,41377,40904,41051,40822,39246,35698,32384,28950,26442,24025,19889,17621,16990,15513,14333,12616,11442,10549,9234,8907,9226,8395,7992,7965,8151,8380,8942,10421,11792,13479,15276,17515,20411,22688,23793,25268,25875,28981,30741,34688,32662,38739,39268,43767,47931,51163,48218,46218,47967,52647,51990,50369,48379,47880,51533,62955,67857,43546,29692,19256,14638,12926,10395,8983,7621,7231,5650,4440],"y":[1.2017,2.2017,3.2017,4.2017,5.2017,6.2017,7.2017,8.2017,9.2017,10.2017,11.2017,12.2017,13.2017,14.2017,15.2017,16.2017,17.2017,18.2017,19.2017,20.2017,21.2017,22.2017,23.2017,24.2017,25.2017,26.2017,27.2017,28.2017,29.2017,30.2017,31.2017,32.2017,33.2017,34.2017,35.2017,36.2017,37.2017,38.2017,39.2017,40.2017,41.2017,42.2017,43.2017,44.2017,45.2017,46.2017,47.2017,48.2017,49.2017,50.2017,51.2017,52.2017,1.2018,2.2018,3.2018,4.2018,5.2018,6.2018,7.2018,8.2018,9.2018,10.2018,11.2018,12.2018,13.2018,14.2018,15.2018,16.2018,17.2018,18.2018,19.2018,20.2018,21.2018,22.2018,23.2018,24.2018,25.2018,26.2018,27.2018,28.2018,29.2018,30.2018,31.2018,32.2018,33.2018,34.2018,35.2018,36.2018,37.2018,38.2018,39.2018,40.2018,41.2018,42.2018,43.2018,44.2018,45.2018,46.2018,47.2018,48.2018,49.2018,50.2018,51.2018,52.2018,1.2019,2.2019,3.2019,4.2019,5.2019,6.2019,7.2019,8.2019,9.2019,10.2019,11.2019,12.2019,13.2019,14.2019,15.2019,16.2019,17.2019,18.2019,19.2019,20.2019,21.2019,22.2019,23.2019,24.2019,25.2019,26.2019,27.2019,28.2019,29.2019,30.2019,31.2019,32.2019,33.2019,34.2019,35.2019,36.2019,37.2019,38.2019,39.2019,40.2019,41.2019,42.2019,43.2019,44.2019,45.2019,46.2019,47.2019,48.2019,49.2019,50.2019,51.2019,52.2019,1.202,2.202,3.202,4.202,5.202,6.202,7.202,8.202,9.202,10.202,11.202,12.202,13.202,14.202,15.202,16.202,17.202,18.202,19.202,20.202,21.202,22.202,23.202],"mode":"markers","marker":{"color":"blue","line":{"color":"rgba(255,127,14,1)"}},"name":"Negative","type":"scatter","error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Reviewing the number of flu cases among our years' data assigned to the week number throughout the year, we can see that the number of cases spike more heavily during the early months and later months of the year, which would coincide during the winter months throughout the year. This makes sense as we would expect the colder weather to have a higher correlation with instances of the flu.

During the middle months out of the year, the number of flu instances drop almost to zero particularly from week 20 through week 40. This also, makes sense as the warmer weather summer months, are better correlated to lower instances of the flu. 

## Year Review


```r
fig2 <- plot_ly(fluDf, x = ~Positive, y = ~year, name = "Positive", type = 'scatter',
                mode = "markers", marker = list(color = "pink"))
fig2 <- fig2 %>% add_trace(x = ~Negative, y = ~year, name = "Negative",type = 'scatter',
                           mode = "markers", marker = list(color = "blue"))
fig2 <- fig2 %>% layout(
  title = "Flu Cases",
  xaxis = list(title = "Number of Cases"),
  margin = list(l = 100)
)

fig2
```

<!--html_preserve--><div id="htmlwidget-6e1cb1a3ef8908183a20" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-6e1cb1a3ef8908183a20">{"x":{"visdat":{"1406e6af374e1":["function () ","plotlyVisDat"]},"cur_data":"1406e6af374e1","attrs":{"1406e6af374e1":{"x":{},"y":{},"mode":"markers","marker":{"color":"pink"},"name":"Positive","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"1406e6af374e1.1":{"x":{},"y":{},"mode":"markers","marker":{"color":"blue"},"name":"Negative","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":100,"t":25,"r":10},"title":"Flu Cases","xaxis":{"domain":[0,1],"automargin":true,"title":"Number of Cases"},"yaxis":{"domain":[0,1],"automargin":true,"title":"year"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"y":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"mode":"markers","marker":{"color":"pink","line":{"color":"rgba(31,119,180,1)"}},"name":"Positive","type":"scatter","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[32027,31696,31760,32838,34640,38442,39335,37663,34264,32740,31217,29746,27905,25497,22778,20956,18404,16014,15082,14136,12727,11541,10616,9556,8524,7592,7155,7519,7341,6933,6914,7058,7797,8371,9618,10519,11951,13165,14184,16070,17102,18472,19124,20540,22117,23851,23144,26598,28965,31436,38764,43049,47846,52262,52775,60927,62357,61920,57573,48353,41268,36118,32314,29552,27014,24955,23119,20202,18481,16609,14801,12996,11805,10861,9811,9074,8614,7825,6920,7075,6908,6891,6396,6867,7169,7893,8821,9930,12224,13447,15067,16975,18564,20797,21957,23086,24036,24924,24598,28467,28467,31004,34092,37141,40210,37747,36444,36881,39146,42040,40913,41377,40904,41051,40822,39246,35698,32384,28950,26442,24025,19889,17621,16990,15513,14333,12616,11442,10549,9234,8907,9226,8395,7992,7965,8151,8380,8942,10421,11792,13479,15276,17515,20411,22688,23793,25268,25875,28981,30741,34688,32662,38739,39268,43767,47931,51163,48218,46218,47967,52647,51990,50369,48379,47880,51533,62955,67857,43546,29692,19256,14638,12926,10395,8983,7621,7231,5650,4440],"y":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"mode":"markers","marker":{"color":"blue","line":{"color":"rgba(255,127,14,1)"}},"name":"Negative","type":"scatter","error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The plot above provides a visual of 2d scatter plots of our year in review, as well as the number of instances of positive and negative tests per week. From 2017 to 2019, the dots for the positive tests leaned more on the lower end of the spectrum, while the negative tests are on the higher end of the spectrum. This would indicate that during those 3 years, the proportion of positive tests is much lower than the instances of negative tests. 

For the 2020 data specifically, we see much more overlap between the positive and negative weeks for weeks the number of results were lower than 40k. This is likely due to the fact that our time frame of the research study ran between Jan through the end of May, and we have equal numbers of months where we have cold weather and warm weather during that time of the year. 


```r
fig3 <- plot_ly(data = fluDf, x = ~Positive, y = ~Negative,
               marker = list(size = 10,
                             color = ~year,
                             line = list(color = ~year,
                                         width = 2)))
fig3 <- fig3 %>% layout(title = 'Flu Cases',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

fig3
```

```
## No trace type specified:
##   Based on info supplied, a 'scatter' trace seems appropriate.
##   Read more about this trace type -> https://plot.ly/r/reference/#scatter
```

```
## No scatter mode specifed:
##   Setting the mode to markers
##   Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode
```

<!--html_preserve--><div id="htmlwidget-46b03fad7378bc9223c2" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-46b03fad7378bc9223c2">{"x":{"visdat":{"1406e29d370b4":["function () ","plotlyVisDat"]},"cur_data":"1406e29d370b4","attrs":{"1406e29d370b4":{"x":{},"y":{},"marker":{"size":10,"color":{},"line":{"color":{},"width":2}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20]}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Flu Cases","yaxis":{"domain":[0,1],"automargin":true,"zeroline":false,"title":"Negative"},"xaxis":{"domain":[0,1],"automargin":true,"zeroline":false,"title":"Positive"},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"y":[32027,31696,31760,32838,34640,38442,39335,37663,34264,32740,31217,29746,27905,25497,22778,20956,18404,16014,15082,14136,12727,11541,10616,9556,8524,7592,7155,7519,7341,6933,6914,7058,7797,8371,9618,10519,11951,13165,14184,16070,17102,18472,19124,20540,22117,23851,23144,26598,28965,31436,38764,43049,47846,52262,52775,60927,62357,61920,57573,48353,41268,36118,32314,29552,27014,24955,23119,20202,18481,16609,14801,12996,11805,10861,9811,9074,8614,7825,6920,7075,6908,6891,6396,6867,7169,7893,8821,9930,12224,13447,15067,16975,18564,20797,21957,23086,24036,24924,24598,28467,28467,31004,34092,37141,40210,37747,36444,36881,39146,42040,40913,41377,40904,41051,40822,39246,35698,32384,28950,26442,24025,19889,17621,16990,15513,14333,12616,11442,10549,9234,8907,9226,8395,7992,7965,8151,8380,8942,10421,11792,13479,15276,17515,20411,22688,23793,25268,25875,28981,30741,34688,32662,38739,39268,43767,47931,51163,48218,46218,47967,52647,51990,50369,48379,47880,51533,62955,67857,43546,29692,19256,14638,12926,10395,8983,7621,7231,5650,4440],"marker":{"color":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"size":10,"line":{"color":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"width":2}},"type":"scatter","mode":"markers","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Combining the years and number of cases, we provided a scatter chart that shows the relationship of the number of positive cases and negative cases denoted by year. The first thing we can see is that we never have a week where the number of positive cases exceed the number of negative cases. Additionally, we can see there are a couple of interesting outliers that appear in the upper left corner. However, these cases would reflect instances where we likely saw a higher number of tests took place. 


```r
fig4 <- plot_ly(fluDf, x = ~year, y = ~Positive, type = 'bar', name = 'Positive')
fig4 <- fig4 %>% add_trace(y = ~Negative, name = 'Negative')
fig4 <- fig4 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig4
```

<!--html_preserve--><div id="htmlwidget-b36764c943267bee86cd" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-b36764c943267bee86cd">{"x":{"visdat":{"1406e438e504d":["function () ","plotlyVisDat"]},"cur_data":"1406e438e504d","attrs":{"1406e438e504d":{"x":{},"y":{},"name":"Positive","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"1406e438e504d.1":{"x":{},"y":{},"name":"Negative","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"barmode":"stack","xaxis":{"domain":[0,1],"automargin":true,"title":"year"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"y":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"name":"Positive","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"y":[32027,31696,31760,32838,34640,38442,39335,37663,34264,32740,31217,29746,27905,25497,22778,20956,18404,16014,15082,14136,12727,11541,10616,9556,8524,7592,7155,7519,7341,6933,6914,7058,7797,8371,9618,10519,11951,13165,14184,16070,17102,18472,19124,20540,22117,23851,23144,26598,28965,31436,38764,43049,47846,52262,52775,60927,62357,61920,57573,48353,41268,36118,32314,29552,27014,24955,23119,20202,18481,16609,14801,12996,11805,10861,9811,9074,8614,7825,6920,7075,6908,6891,6396,6867,7169,7893,8821,9930,12224,13447,15067,16975,18564,20797,21957,23086,24036,24924,24598,28467,28467,31004,34092,37141,40210,37747,36444,36881,39146,42040,40913,41377,40904,41051,40822,39246,35698,32384,28950,26442,24025,19889,17621,16990,15513,14333,12616,11442,10549,9234,8907,9226,8395,7992,7965,8151,8380,8942,10421,11792,13479,15276,17515,20411,22688,23793,25268,25875,28981,30741,34688,32662,38739,39268,43767,47931,51163,48218,46218,47967,52647,51990,50369,48379,47880,51533,62955,67857,43546,29692,19256,14638,12926,10395,8983,7621,7231,5650,4440],"name":"Negative","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
fig5 <- plot_ly(fluDf, x = ~week, y = ~Positive, type = 'bar', name = 'Positive')
fig5 <- fig5 %>% add_trace(y = ~Negative, name = 'Negative')
fig5 <- fig5 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig5
```

<!--html_preserve--><div id="htmlwidget-d4048b3f505120993ff1" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-d4048b3f505120993ff1">{"x":{"visdat":{"1406e343a11e1":["function () ","plotlyVisDat"]},"cur_data":"1406e343a11e1","attrs":{"1406e343a11e1":{"x":{},"y":{},"name":"Positive","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"1406e343a11e1.1":{"x":{},"y":{},"name":"Negative","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"barmode":"stack","xaxis":{"domain":[0,1],"automargin":true,"title":"week"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23],"y":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"name":"Positive","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23],"y":[32027,31696,31760,32838,34640,38442,39335,37663,34264,32740,31217,29746,27905,25497,22778,20956,18404,16014,15082,14136,12727,11541,10616,9556,8524,7592,7155,7519,7341,6933,6914,7058,7797,8371,9618,10519,11951,13165,14184,16070,17102,18472,19124,20540,22117,23851,23144,26598,28965,31436,38764,43049,47846,52262,52775,60927,62357,61920,57573,48353,41268,36118,32314,29552,27014,24955,23119,20202,18481,16609,14801,12996,11805,10861,9811,9074,8614,7825,6920,7075,6908,6891,6396,6867,7169,7893,8821,9930,12224,13447,15067,16975,18564,20797,21957,23086,24036,24924,24598,28467,28467,31004,34092,37141,40210,37747,36444,36881,39146,42040,40913,41377,40904,41051,40822,39246,35698,32384,28950,26442,24025,19889,17621,16990,15513,14333,12616,11442,10549,9234,8907,9226,8395,7992,7965,8151,8380,8942,10421,11792,13479,15276,17515,20411,22688,23793,25268,25875,28981,30741,34688,32662,38739,39268,43767,47931,51163,48218,46218,47967,52647,51990,50369,48379,47880,51533,62955,67857,43546,29692,19256,14638,12926,10395,8983,7621,7231,5650,4440],"name":"Negative","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Looking further into the ratio of negative tests with positive tests, we can see that among the three years of data we have available, we can see that roughly 20% of our tests are coming out positive in the first 3 full years of our research study. This can prove handy in our forecast for Q3 and Q4 of 2020 when we are going to want to ensure our forecast normalizes for the continued warm months and ramps up in the cold months of Q4. 

Reviewing our monthly data in the plot above, we can see that our 20% ratio is not consistent in each month and that we see ranges from nearly 0% to nearly 40-50%, so our forecasting model will need to account for this if we want to accurately predict Q3 and Q4. 


```r
fig6 <- plot_ly(
  type = "scatter",
  x = fluDf$year, 
  y = fluDf$Positive,
  name = 'Positive FLu Cases',
  mode = "markers"
)
fig6 <- fig6 %>%
  layout(
    title = "Flu Cases",
    xaxis = list(
      type = "Year"
    )
  )
fig6
```

<!--html_preserve--><div id="htmlwidget-af68270a388b190e625f" style="width:960px;height:960px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-af68270a388b190e625f">{"x":{"visdat":{"1406e5bac7a2f":["function () ","plotlyVisDat"]},"cur_data":"1406e5bac7a2f","attrs":{"1406e5bac7a2f":{"x":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"y":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"mode":"markers","name":"Positive FLu Cases","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Flu Cases","xaxis":{"domain":[0,1],"automargin":true,"type":"Year","title":[]},"yaxis":{"domain":[0,1],"automargin":true,"title":[]},"hovermode":"closest","showlegend":false},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020],"y":[9770,11527,12064,12385,14466,17591,17896,17171,12240,11117,9850,9714,8529,6302,4557,3266,2164,1652,1434,1213,927,777,600,467,359,301,268,245,209,214,223,226,180,217,306,370,408,404,455,573,627,705,915,1265,1601,2207,2851,3882,5791,8961,16447,20855,24240,27507,27154,30485,30771,30721,27573,20553,14671,10999,9522,7920,7067,5579,4468,3192,2424,1694,968,589,426,251,200,156,114,112,97,96,109,95,91,96,86,156,161,222,261,284,277,410,447,620,809,1011,1391,1747,2241,2872,3981,6557,9528,11095,10437,10274,11193,12928,15595,18461,18483,18870,18914,18699,18624,14882,11688,8496,6350,4095,2806,1861,1508,1250,965,767,629,511,399,359,358,289,302,278,279,246,285,335,477,504,504,596,664,473,583,675,936,1236,2383,3131,4919,5623,7173,10413,16793,23142,21991,20449,22377,25704,30419,29965,27572,24773,21919,19897,15808,7509,1662,506,192,130,77,67,68,65,29,24,25],"mode":"markers","name":"Positive FLu Cases","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Going back to our 2d scatter plot of just positive cases, broken out by year, we can see that we have some inconsistency with the number of positive outcomes that appear in the given years. For 2017 and 2018, we can see the peak is generally under 20k with a single outlier above. For the positive cases in 2018 and 2020, we can see there are many more weeks where we had more than 20k positive flu cases. 

### Flu Cases Forecasting


```r
#all data
plotts.wge(fluDf$Positive)
```

![](Flustudy_case5_files/figure-html/forecast-1.png)<!-- -->

```r
plotts.wge(fluDf$Negative)
```

![](Flustudy_case5_files/figure-html/forecast-2.png)<!-- -->

```r
plotts.sample.wge(fluDf$Positive)
```

![](Flustudy_case5_files/figure-html/forecast-3.png)<!-- -->

```
## $autplt
##  [1]  1.000000000  0.972441770  0.907231719  0.818963941  0.717286643
##  [6]  0.603686636  0.476159140  0.342530360  0.212965494  0.095616484
## [11] -0.008450992 -0.101563204 -0.183031484 -0.248521198 -0.297941347
## [16] -0.334317848 -0.359944739 -0.376893446 -0.386217865 -0.389930784
## [21] -0.391639674 -0.391655011 -0.390301664 -0.386428238 -0.380553095
## [26] -0.376647924
## 
## $freq
##  [1] 0.005586592 0.011173184 0.016759777 0.022346369 0.027932961 0.033519553
##  [7] 0.039106145 0.044692737 0.050279330 0.055865922 0.061452514 0.067039106
## [13] 0.072625698 0.078212291 0.083798883 0.089385475 0.094972067 0.100558659
## [19] 0.106145251 0.111731844 0.117318436 0.122905028 0.128491620 0.134078212
## [25] 0.139664804 0.145251397 0.150837989 0.156424581 0.162011173 0.167597765
## [31] 0.173184358 0.178770950 0.184357542 0.189944134 0.195530726 0.201117318
## [37] 0.206703911 0.212290503 0.217877095 0.223463687 0.229050279 0.234636872
## [43] 0.240223464 0.245810056 0.251396648 0.256983240 0.262569832 0.268156425
## [49] 0.273743017 0.279329609 0.284916201 0.290502793 0.296089385 0.301675978
## [55] 0.307262570 0.312849162 0.318435754 0.324022346 0.329608939 0.335195531
## [61] 0.340782123 0.346368715 0.351955307 0.357541899 0.363128492 0.368715084
## [67] 0.374301676 0.379888268 0.385474860 0.391061453 0.396648045 0.402234637
## [73] 0.407821229 0.413407821 0.418994413 0.424581006 0.430167598 0.435754190
## [79] 0.441340782 0.446927374 0.452513966 0.458100559 0.463687151 0.469273743
## [85] 0.474860335 0.480446927 0.486033520 0.491620112 0.497206704
## 
## $db
##  [1]  -0.01518981   5.84783089  14.33374978  12.48958630   9.09067644
##  [6] -17.51603895  13.31811286   4.98606130  -3.26538002   4.76691328
## [11] -10.69640964  -0.45870603  -8.29133900 -12.94698870  -2.82905990
## [16] -17.74166390 -11.16230195  -5.63678016 -15.92476754  -8.23226884
## [21] -13.82070819 -13.19527395 -20.00966221 -10.62446277 -14.70155853
## [26] -21.77912150 -25.93177843  -6.33760011 -12.95076761 -14.19384672
## [31]  -8.16896008 -20.08476810 -16.90770666 -14.49386115  -7.81048915
## [36] -11.00070178 -11.82866227 -12.44874034 -20.94107342 -32.07273947
## [41] -25.93211857 -27.86293746 -21.59045316 -20.78794780 -30.28875453
## [46] -28.01376539 -35.59293004 -22.03289099 -26.90207610 -20.65578568
## [51] -19.23042071 -22.57041569 -21.70922478 -25.09045652 -26.93842194
## [56] -26.64877179 -27.22649504 -21.19040536 -28.65026139 -35.99004035
## [61] -24.89073780 -28.22929193 -25.83419809 -31.42661446 -20.37688374
## [66] -22.96533851 -24.70662912 -21.31400846 -28.96774370 -23.94459197
## [71] -25.19492243 -27.39499606 -31.19146514 -21.12692923 -19.70433513
## [76] -26.13428530 -23.16422046 -23.70231873 -21.92799441 -30.30495711
## [81] -24.47755683 -20.65073462 -32.23520305 -27.27912711 -23.82444488
## [86] -33.51002541 -30.17022865 -33.25060453 -43.69046035
## 
## $dbz
##  [1]   9.6480393   9.6611229   9.6445856   9.5548132   9.3493965   8.9933871
##  [7]   8.4612502   7.7365812   6.8114277   5.6862639   4.3709832   2.8866795
## [13]   1.2672221  -0.4412214  -2.1843714  -3.9062212  -5.5507912  -7.0533635
## [19]  -8.3317039  -9.3066146  -9.9578629 -10.3552324 -10.6147340 -10.8284505
## [25] -11.0321563 -11.2158951 -11.3554275 -11.4424103 -11.4954585 -11.5491927
## [31] -11.6357530 -11.7741584 -11.9715735 -12.2318401 -12.5642718 -12.9873474
## [37] -13.5258708 -14.2039415 -15.0371510 -16.0260300 -17.1512215 -18.3705979
## [43] -19.6190775 -20.8118684 -21.8517343 -22.6445732 -23.1315669 -23.3281692
## [49] -23.3294978 -23.2654881 -23.2463855 -23.3358353 -23.5500832 -23.8689699
## [55] -24.2511680 -24.6502125 -25.0252032 -25.3396042 -25.5518113 -25.6148453
## [61] -25.4989802 -25.2220412 -24.8520119 -24.4754305 -24.1621041 -23.9490400
## [67] -23.8419144 -23.8246647 -23.8711680 -23.9556767 -24.0591848 -24.1702556
## [73] -24.2820795 -24.3899398 -24.4920580 -24.5928366 -24.7045138 -24.8441550
## [79] -25.0268222 -25.2588302 -25.5350733 -25.8422664 -26.1666839 -26.5013440
## [85] -26.8462046 -27.1987088 -27.5392964 -27.8227006 -27.9875805
```

```r
plotts.sample.wge(fluDf$Negative)
```

![](Flustudy_case5_files/figure-html/forecast-4.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.96956192  0.91213289  0.84217716  0.76694393  0.68651530
##  [7]  0.59654899  0.49816704  0.39482616  0.29403181  0.20086480  0.11290537
## [13]  0.03365269 -0.04830513 -0.13283032 -0.21403858 -0.28979144 -0.35759788
## [19] -0.41987435 -0.47468177 -0.52423058 -0.56565385 -0.59815015 -0.62096431
## [25] -0.63741212 -0.64722732
## 
## $freq
##  [1] 0.005586592 0.011173184 0.016759777 0.022346369 0.027932961 0.033519553
##  [7] 0.039106145 0.044692737 0.050279330 0.055865922 0.061452514 0.067039106
## [13] 0.072625698 0.078212291 0.083798883 0.089385475 0.094972067 0.100558659
## [19] 0.106145251 0.111731844 0.117318436 0.122905028 0.128491620 0.134078212
## [25] 0.139664804 0.145251397 0.150837989 0.156424581 0.162011173 0.167597765
## [31] 0.173184358 0.178770950 0.184357542 0.189944134 0.195530726 0.201117318
## [37] 0.206703911 0.212290503 0.217877095 0.223463687 0.229050279 0.234636872
## [43] 0.240223464 0.245810056 0.251396648 0.256983240 0.262569832 0.268156425
## [49] 0.273743017 0.279329609 0.284916201 0.290502793 0.296089385 0.301675978
## [55] 0.307262570 0.312849162 0.318435754 0.324022346 0.329608939 0.335195531
## [61] 0.340782123 0.346368715 0.351955307 0.357541899 0.363128492 0.368715084
## [67] 0.374301676 0.379888268 0.385474860 0.391061453 0.396648045 0.402234637
## [73] 0.407821229 0.413407821 0.418994413 0.424581006 0.430167598 0.435754190
## [79] 0.441340782 0.446927374 0.452513966 0.458100559 0.463687151 0.469273743
## [85] 0.474860335 0.480446927 0.486033520 0.491620112 0.497206704
## 
## $db
##  [1]   2.9079323   7.1191196  15.5016310  13.6439763   8.1563127  -6.4150994
##  [7]   7.8955254   4.5018539 -10.2812387   4.0965116   0.1394168 -16.7176096
## [13]  -1.5300283  -2.6884105  -9.8814007  -5.1829553 -13.7499896  -9.5881868
## [19] -17.0555650 -10.1019636 -11.6741316 -13.9281370  -8.0385423 -15.3642552
## [25]  -8.9502537  -7.3214275 -10.8237919  -6.0567063  -6.9369024  -9.7756209
## [31]  -6.6748446 -14.5425166 -21.7265500 -15.3752505 -15.6855072 -17.4481634
## [37] -15.2748496 -12.2386998 -13.6079547 -13.2756139 -13.5638782 -14.4339339
## [43] -16.6815016 -17.8499195 -13.6425576 -17.4948473 -20.1254348 -19.7428014
## [49] -25.2289914 -26.2750475 -22.8012177 -20.4676804 -17.2252532 -17.0934519
## [55] -16.9846490 -16.8429729 -17.7869457 -16.8118419 -18.2582552 -18.2173162
## [61] -17.2604627 -26.2414270 -30.8630645 -26.9125657 -23.2328335 -20.8292565
## [67] -23.7774381 -21.8306151 -16.9745759 -33.1166552 -17.6940729 -16.4101647
## [73] -19.1384855 -15.5490514 -18.0490075 -21.7608315 -21.5166364 -23.0123528
## [79] -20.8242547 -30.2252276 -23.1324601 -40.3644125 -32.4082039 -22.1613632
## [85] -17.9049840 -20.2021036 -17.7040061 -21.1880215 -27.7485731
## 
## $dbz
##  [1]  10.378723  10.288684  10.114488   9.826551   9.393262   8.785687
##  [7]   7.980686   6.963488   5.731380   4.300221   2.714406   1.056269
## [13]  -0.559702  -2.027763  -3.313511  -4.471134  -5.582182  -6.672182
## [19]  -7.667023  -8.422667  -8.838539  -8.957418  -8.922599  -8.863707
## [25]  -8.845330  -8.882367  -8.973842  -9.126953  -9.362682  -9.706697
## [31] -10.175231 -10.763771 -11.442026 -12.157508 -12.849905 -13.472846
## [37] -14.009577 -14.469810 -14.872810 -15.234559 -15.568689 -15.894894
## [43] -16.242167 -16.640785 -17.107118 -17.629445 -18.162052 -18.634709
## [49] -18.980773 -19.170653 -19.223115 -19.185876 -19.108826 -19.032125
## [55] -18.988568 -19.008565 -19.120076 -19.343196 -19.682659 -20.121222
## [61] -20.616276 -21.102768 -21.505690 -21.760705 -21.833105 -21.724477
## [67] -21.467248 -21.114723 -20.730490 -20.377544 -20.108965 -19.962551
## [73] -19.959633 -20.106459 -20.396198 -20.810001 -21.315932 -21.865352
## [79] -22.389160 -22.802021 -23.025652 -23.027514 -22.842933 -22.554497
## [85] -22.248695 -21.985858 -21.793551 -21.674406 -21.619131
```

Starting with the initial step of our time series analysis, we decided to compare the realization plots, auto correlation plots, periodogram plots and spectral density plots for both the positve and negative cases. See below for a quick overview of what each plot and what they tell us. 

* Realization plot - Provides a raw view of a response variable chronologically over a period of time. 
* Auto-correlation Function Plot (ACF) - Provides a view of the correlation a point in the time series has with the points adjacent to it. 
* Frequency periodogram - Provides a view of the values that appear in a cyclic manner and plots where you can expect to see high frequency points and low frequency points with respect to the time frame your analyzing. 
* Parzen Window (Spectral Density) - The parzen window provides a similar view as the periodogram, but also provides some more insight to the specific frequency peaks which we can accurately measure. Additionally, a Parzen window provides some detail on the wandering nature of the data, as well as the change in variance over time. 

On the instance of positive tests, we can see that there are 4 major cyclic peaks within our realization, which is reflected in the spike at 0.2. Additionally, we can see evidence of spikes at 0.05 and 0.3 and 0.4 which we can unpack further. 

Looking at the negative data, we can see that the valleys between our peaks on the positive instances is less pronounced with a smoother regression. This was reflected in our parzen window with a little smoother view of the density peaks. 


```r
#truncated to last year
plotts.sample.wge(fluDf$Positive[0:100])
```

![](Flustudy_case5_files/figure-html/last year-1.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.97548311  0.91622746  0.83135879  0.73164732  0.62184594
##  [7]  0.50173200  0.37897443  0.25991181  0.15330723  0.06116274 -0.01658386
## [13] -0.08313047 -0.14008496 -0.18826220 -0.22892196 -0.26254795 -0.29017131
## [19] -0.31267119 -0.33097648 -0.34580059 -0.35758955 -0.36709872 -0.37484966
## [25] -0.38109960 -0.38604129
## 
## $freq
##  [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15
## [16] 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28 0.29 0.30
## [31] 0.31 0.32 0.33 0.34 0.35 0.36 0.37 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.45
## [46] 0.46 0.47 0.48 0.49 0.50
## 
## $db
##  [1]   5.219694  14.760987   2.241532  10.172096  -2.416058   4.161394
##  [7]  -6.849382  -2.521004 -12.400777 -11.893330 -16.460969 -20.811947
## [13] -14.183965 -11.202582 -14.284735  -9.368501 -15.069770 -10.615643
## [19] -16.249506 -14.456717 -17.739381 -20.039786 -21.363150 -24.591954
## [25] -27.886755 -22.621252 -40.812248 -22.200920 -31.838753 -26.567388
## [31] -27.888125 -26.088031 -27.089684 -24.137702 -27.170447 -24.082440
## [37] -26.717122 -23.792806 -27.519557 -22.027594 -26.955364 -19.828742
## [43] -26.028846 -19.947958 -24.970795 -23.056242 -26.208468 -30.500461
## [49] -34.175602 -49.665272
## 
## $dbz
##  [1]   9.665026   9.348471   8.789134   7.947990   6.783485   5.259352
##  [7]   3.353649   1.073633  -1.518120  -4.269199  -6.925237  -9.183381
## [13] -10.777314 -11.585215 -11.807360 -11.882160 -12.161496 -12.785305
## [19] -13.731631 -14.897039 -16.185830 -17.575060 -19.076531 -20.606763
## [25] -21.932088 -22.889660 -23.641292 -24.459392 -25.365726 -26.076617
## [31] -26.336547 -26.251859 -26.050519 -25.780990 -25.400102 -24.953886
## [37] -24.557413 -24.265591 -24.036993 -23.810991 -23.598335 -23.482280
## [43] -23.547093 -23.826705 -24.308359 -24.966650 -25.782729 -26.701508
## [49] -27.522829 -27.872502
```

```r
plotts.sample.wge(fluDf$Positive[100:175])
```

![](Flustudy_case5_files/figure-html/last year-2.png)<!-- -->

```
## $autplt
##  [1]  1.000000000  0.961959741  0.880948473  0.780321217  0.669855920
##  [6]  0.546173175  0.403334403  0.251747970  0.113721725  0.002041615
## [11] -0.088615372 -0.166448883 -0.233210246 -0.280060004 -0.302180595
## [16] -0.305475299 -0.302091344 -0.297126934 -0.289573395 -0.276352985
## [21] -0.258728492 -0.248305784 -0.245665525 -0.246949155 -0.249693712
## [26] -0.252976118
## 
## $freq
##  [1] 0.01315789 0.02631579 0.03947368 0.05263158 0.06578947 0.07894737
##  [7] 0.09210526 0.10526316 0.11842105 0.13157895 0.14473684 0.15789474
## [13] 0.17105263 0.18421053 0.19736842 0.21052632 0.22368421 0.23684211
## [19] 0.25000000 0.26315789 0.27631579 0.28947368 0.30263158 0.31578947
## [25] 0.32894737 0.34210526 0.35526316 0.36842105 0.38157895 0.39473684
## [31] 0.40789474 0.42105263 0.43421053 0.44736842 0.46052632 0.47368421
## [37] 0.48684211 0.50000000
## 
## $db
##  [1]  10.2207487  10.6298243  11.1662572  -0.5504897  -1.3070528  -4.0347598
##  [7] -10.0418450 -15.6356317 -10.5420021 -11.5758214 -20.6686830 -11.5311095
## [13]  -8.0498151 -11.8519664  -9.5850324 -11.9384701 -21.8521685 -29.0102497
## [19] -21.7698553 -21.9714420 -26.1794159 -22.4015317 -25.5389105 -29.4115102
## [25] -22.3635958 -23.0403913 -29.6110051 -24.1404970 -29.5636168 -25.5408275
## [31] -24.1247118 -54.1828809 -27.3330474 -38.6464698 -28.7492265 -27.3530759
## [37] -31.3456822 -38.5663876
## 
## $dbz
##  [1]   8.9658234   8.5627049   7.8343073   6.7123117   5.1237160   3.0030367
##  [7]   0.3159832  -2.8648491  -6.1505214  -8.6370232  -9.6811657  -9.7789207
## [13]  -9.6173976  -9.5892193  -9.9356382 -10.7907510 -12.1786960 -14.0053076
## [19] -16.0358044 -17.8988638 -19.2093269 -19.8170710 -19.9347953 -19.9276501
## [25] -20.0308851 -20.2805731 -20.6081239 -20.9648152 -21.3527401 -21.7493300
## [31] -22.0694853 -22.2582411 -22.3818771 -22.5548459 -22.8086995 -23.0698787
## [37] -23.2464895 -23.3048753
```

```r
#truncated to 6 months 
plotts.sample.wge(fluDf$Negative[0:100])
```

![](Flustudy_case5_files/figure-html/last year-3.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.98102935  0.94090464  0.88100975  0.80586462  0.72082348
##  [7]  0.62479673  0.52313439  0.42040339  0.31839504  0.21994292  0.12482439
## [13]  0.03281813 -0.05474150 -0.13867012 -0.21804320 -0.29245500 -0.36162417
## [19] -0.42440821 -0.48152306 -0.53171750 -0.57397260 -0.60819140 -0.63418938
## [25] -0.65177430 -0.66025399
## 
## $freq
##  [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15
## [16] 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28 0.29 0.30
## [31] 0.31 0.32 0.33 0.34 0.35 0.36 0.37 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.45
## [46] 0.46 0.47 0.48 0.49 0.50
## 
## $db
##  [1]   4.69691053  16.10965414  -0.07541341   4.24146233  -1.43572303
##  [6]   0.04877594  -7.04304932  -5.63662916 -12.05173605 -12.96328491
## [11] -16.40496246 -15.45613471 -18.78831932 -13.23492508 -22.58705628
## [16] -13.73640522 -18.76129025 -16.22944016 -21.77235391 -21.26965852
## [21] -22.62789172 -25.84555556 -24.91658984 -30.62964705 -27.03874155
## [26] -33.71273359 -27.92588294 -32.83395232 -27.47510461 -34.51045122
## [31] -29.51892053 -27.43206854 -33.06629928 -25.68160379 -30.54366269
## [36] -22.97710282 -24.05516979 -21.89176564 -23.65323859 -23.79384933
## [41] -30.01088847 -25.94691106 -27.72467909 -30.65470150 -32.99537054
## [46] -33.57166169 -34.31863380 -28.93358367 -41.63258642 -27.20222103
## 
## $dbz
##  [1]  10.1204838   9.6975345   8.9659298   7.8897100   6.4256013   4.5298897
##  [7]   2.1726139  -0.6264901  -3.7164575  -6.7189323  -9.1826999 -11.0472131
## [13] -12.5003971 -13.4501203 -13.8111656 -13.9664079 -14.3837780 -15.2278040
## [19] -16.3515406 -17.4311752 -18.2736750 -19.0164306 -19.8602173 -20.7637184
## [25] -21.4959842 -21.9859123 -22.4527958 -23.0774138 -23.7223139 -24.0197277
## [31] -23.8271572 -23.3888338 -22.9355232 -22.4992184 -22.0593749 -21.6749971
## [37] -21.4520875 -21.4487782 -21.6375922 -21.9362643 -22.2725045 -22.6248137
## [43] -22.9966655 -23.3647109 -23.6791148 -23.9220891 -24.1361515 -24.3642442
## [49] -24.5727766 -24.6615671
```

```r
plotts.sample.wge(fluDf$Negative[100:175])
```

![](Flustudy_case5_files/figure-html/last year-4.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.94787587  0.85857109  0.76413492  0.67722191  0.59464552
##  [7]  0.51461279  0.43621626  0.37838511  0.31886266  0.25250309  0.17766636
## [13]  0.09237947  0.00508330 -0.07560636 -0.14658132 -0.21491666 -0.27377297
## [19] -0.32655154 -0.36623667 -0.40054418 -0.42817988 -0.45272471 -0.47086038
## [25] -0.48984558 -0.49870269
## 
## $freq
##  [1] 0.01315789 0.02631579 0.03947368 0.05263158 0.06578947 0.07894737
##  [7] 0.09210526 0.10526316 0.11842105 0.13157895 0.14473684 0.15789474
## [13] 0.17105263 0.18421053 0.19736842 0.21052632 0.22368421 0.23684211
## [19] 0.25000000 0.26315789 0.27631579 0.28947368 0.30263158 0.31578947
## [25] 0.32894737 0.34210526 0.35526316 0.36842105 0.38157895 0.39473684
## [31] 0.40789474 0.42105263 0.43421053 0.44736842 0.46052632 0.47368421
## [37] 0.48684211 0.50000000
## 
## $db
##  [1]  12.47948015  10.30196798   5.52530642   0.55285665  -1.58570630
##  [6]   1.18554610  -0.02080284  -4.33003801  -6.40387545  -9.85189697
## [11] -10.26212748  -9.00771638  -7.65519132 -10.92080538  -9.44054517
## [16]  -9.04125197 -10.80389410 -13.07795563 -12.52043867 -16.34259215
## [21] -17.35994582 -23.82264732 -20.09714370 -18.95299118 -18.51833946
## [26] -14.81981289 -16.10032857 -17.48104122 -16.82414625 -17.49547951
## [31] -19.91042215 -36.16424581 -21.80470073 -28.07998012 -20.88223880
## [36] -15.79213610 -16.59333396 -25.22318435
## 
## $dbz
##  [1]   9.25728027   8.61267619   7.53824959   6.04644514   4.18664983
##  [6]   2.08713273  -0.01562294  -1.86386395  -3.39379814  -4.75847085
## [11]  -6.06560938  -7.21676096  -8.05423397  -8.62757599  -9.16820905
## [16]  -9.85364808 -10.71831435 -11.71639205 -12.80707014 -13.96964456
## [21] -15.13088501 -16.10084487 -16.66684650 -16.80933056 -16.72151289
## [26] -16.62869243 -16.69399254 -17.02331718 -17.66637824 -18.58310708
## [31] -19.58182931 -20.29595493 -20.38803622 -19.89385160 -19.14883866
## [36] -18.45813158 -17.99266709 -17.83012583
```

```r
length(fluDf$Positive[1:100]) 
```

```
## [1] 100
```

```r
length(fluDf$Negative[1:100]) 
```

```
## [1] 100
```

```r
acf(fluDf$Positive[1:100])
acf(fluDf$Negative[1:100])
```

![](Flustudy_case5_files/figure-html/last year-5.png)<!-- -->

Looking specifically at the first half of our dataset along with the second half, we can see that our ACF functions look to be relatively consistent no matter where you are in the plot. This is important when you are looking for our time series to apply to the concept of stationarity, which requires the following criteria.

* Consistent mean throughout the time frame
* Consistent variance throughout the time frame
* Auto-correlations are independent of where you are in time. 

In the plots above (particularly the view of the second half acfs compared to the first), we can see that we have very similar looking plots, this would indicate that our positive instances variable is meeting the third criteria of our stationary requirement. 

#### Time Series Modeling



```r
aic5.wge(fluDf$Positive,p=0:15,q=0:12,type = 'bic')

#Five Smallest Values of  bic 
#p    q        bic
#92     7    0   14.30477
#32     2    5   14.31919
#105    8    0   14.33046
#80     6    1   14.33107
#93     7    1   14.33112

aic5.wge(fluDf$Positive,p=0:15,q=0:12,type = 'aic')

#Five Smallest Values of  aic 
#p    q        aic
#92     7    0   14.16232
#34     2    7   14.16889
#105    8    0   14.17020
#93     7    1   14.17086
#32     2    5   14.17674

aic5.wge(fluDf$Negative,p=0:12,q=0:12,type = 'bic')

#Five Smallest Values of  bic 
#p    q        bic
#27    2    0   15.92028
#28    2    1   15.94886
#40    3    0   15.94888
#16    1    2   15.96290
#79    6    0   15.96787

aic5.wge(fluDf$Negative,p=0:12,q=0:12,type = 'aic')

#Five Smallest Values of  aic 
#p    q        aic
#84     6    5   15.78323
#157   12    0   15.81752
#86     6    7   15.81909
#158   12    1   15.82814
#144   11    0   15.83928
```

As we move forward into the fitting of our model to forecast future demand for flu treatment products, we first need to look into possible model frameworks that would be preferred for us to use in our final forecast model. Here we will use the Akaike information criterion as our key performance indicator for the best model (we will be looking for the lowest AIC score). The AIC is an estimator of out-of-sample prediction error, and thus the relative quality of our statistical models for a given set of parameters. 

Our output of the AIC score will be a preferred combination of Ps and Qs to use as the framework of our final model fit. The Ps represent the Auto-regression score, which indicates which observations from previous models can be input into a regression equation. The Qs represent the moving average model, which specifies how the output variable depends linearly on the current and previous values. The combination of Ps and Qs in our final model fit is known as an "ARMA" model. 

Here, to help keep our models from being too computationally intense, we will start with a test of an AIC score using Ps and Qs of less than 15 for the P and 12 for the Qs.

After testing various combinations of ARMA models, we can see that our AIC prefers an AR(7) model most of all with the low AIC score of 14.16 for the positive data, and an ARMA (6,5) model for the negative data. 

### Estimate Model Parameters


```r
e1 = est.arma.wge(fluDf$Positive, p=7, q=0)
```

```
## 
## Coefficients of Original polynomial:  
## 1.7641 -0.8814 -0.0677 0.1764 0.3703 -0.6535 0.2381 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1-1.8341B+0.8734B^2    1.0500+-0.2060i      0.9346       0.0308
## 1-0.6787B+0.7435B^2    0.4565+-1.0661i      0.8623       0.1856
## 1+1.3156B+0.6470B^2   -1.0166+-0.7155i      0.8044       0.4024
## 1-0.5668B              1.7643               0.5668       0.0000
##   
## 
```

```r
e1
```

```
## $phi
## [1]  1.76405274 -0.88142813 -0.06765031  0.17643910  0.37028688 -0.65347541
## [7]  0.23814701
## 
## $theta
## [1] 0
## 
## $res
##   [1]   877.199385  -127.802266  -237.230049   224.577977  1555.647645
##   [6]  1222.959014 -1559.256409   286.678781 -3438.680040  2278.403461
##  [11] -1330.457166  1006.677803  -537.571983   597.864559  -682.532945
##  [16]    -4.767767  -819.199909   401.392136   418.451534  -242.978869
##  [21]  -303.396465   -88.098035  -452.160800  -376.911501  -304.684201
##  [26]  -261.504657  -343.872054  -307.991468  -348.068620  -296.603788
##  [31]  -341.314085  -348.122960  -384.027770  -257.351158  -295.450973
##  [36]  -350.971970  -333.225299  -331.504972  -295.802877  -277.838850
##  [41]  -368.370013  -270.907979  -140.850827  -140.265324  -250.248881
##  [46]   116.942667   -46.984096   303.439102   932.106953  1634.216508
##  [51]  5078.758742  -900.053223  1079.757804  2021.381543 -2305.619483
##  [56]  2823.896404  -948.773832   676.767127 -1890.780599 -2510.496793
##  [61] -1079.555100  1387.726900   696.434072   368.898527  2383.625169
##  [66]  -123.404033   -21.383957  -876.003995   179.991754  -511.805921
##  [71]  -142.198320   -68.601005   -15.983712  -442.690935  -153.883286
##  [76]  -231.634975  -384.285827  -337.967466  -331.896134  -359.931796
##  [81]  -326.472921  -366.004283  -347.496968  -332.475331  -367.017670
##  [86]  -274.665456  -387.002077  -285.444490  -344.275932  -340.396524
##  [91]  -383.995138  -180.225030  -425.329622  -180.671207  -251.564951
##  [96]  -243.112981  -106.986327  -186.137297   -76.433604    21.234762
## [101]   417.089101  1509.825928   938.049126  -542.481923 -1363.047871
## [106]   646.428868   475.371697   443.665789  1712.905879  2054.495211
## [111] -1416.091795  1042.464187   143.404031  -384.920116   248.824735
## [116] -2386.107517   121.108703  -470.587139   -24.725039  -986.630590
## [121]  1546.174736    60.686655   450.543854  -237.642524   -22.356663
## [126]  -287.379491  -237.862047  -440.556093  -339.238453  -239.072869
## [131]  -303.346613  -403.013351  -251.452621  -354.987615  -334.959640
## [136]  -376.920481  -248.320428  -341.568811  -221.990441  -412.103280
## [141]  -319.480048  -239.141124  -341.831721  -605.654475   -21.671049
## [146]  -319.874837  -189.626024  -192.870771   735.033274  -456.221805
## [151]  1080.811277  -748.064919  1021.823724  1846.216884  4010.169927
## [156]  1494.557478 -4557.597407   847.255935  3430.233747   184.602744
## [161]  1875.382758  -505.377957   250.170422  -443.126412  -936.245051
## [166]  -503.477071  -851.361029 -3952.062462  1184.204931  2905.393887
## [171]  -812.767116   601.378578  2216.900170   144.125873 -1249.178830
## [176]  -502.105352  -430.192550  -318.611005  -357.067758
## 
## $avar
## [1] 1293595
## 
## $aic
## [1] 14.16232
## 
## $aicc
## [1] 15.17944
## 
## $bic
## [1] 14.30477
## 
## $se.phi
## [1] 0.005205212 0.020085343 0.023812880 0.023555472 0.023484395 0.020247706
## [7] 0.005274554
## 
## $se.theta
## [1] 0
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

```r
e2 = est.arma.wge(fluDf$Negative, p=6, q=5)
```

```
## 
## Coefficients of Original polynomial:  
## 1.8429 -0.8968 -0.6407 1.5967 -1.0526 0.1229 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1-1.9607B+0.9747B^2    1.0058+-0.1196i      0.9873       0.0188
## 1+0.9720B             -1.0288               0.9720       0.5000
## 1-0.7066B+0.8792B^2    0.4019+-0.9879i      0.9376       0.1885
## 1-0.1476B              6.7754               0.1476       0.0000
##   
## 
```

```r
e2
```

```
## $phi
## [1]  1.8428774 -0.8968270 -0.6407014  1.5967127 -1.0525593  0.1229353
## 
## $theta
## [1]  0.45129414  0.24224825 -0.53596162  0.75762713  0.08476119
## 
## $res
##   [1]    944.507433   1783.682585   1466.107466   1179.045205   3038.523835
##   [6]   4936.610985   2223.020950   -149.421558   -341.676589   1552.951684
##  [11]   1258.359757    593.422891    944.921822    270.492770    715.726192
##  [16]    818.906860    -52.354187   -115.456699   1919.497948    741.853196
##  [21]    544.380143    276.746651    952.858444     80.854301    445.758544
##  [26]   -229.172215    589.281845    533.214089     78.421627   -560.784827
##  [31]    283.619036   -447.038833    337.298515   -712.694050    547.603138
##  [36]   -648.937975    443.041382   -732.886215   -326.414266      4.869801
##  [41]   -634.436990   -588.377882   -955.877326   -466.267991   -173.272284
##  [46]   -562.449590  -2510.980322   2277.439449   -190.662218    100.489187
##  [51]   5428.501500    729.637317   3278.122736   2680.715461   -360.275039
##  [56]   8456.258479   -802.669355    586.440777  -1906.453218  -5014.698637
##  [61]  -2071.421066  -1838.711993  -1395.052640      8.081422    573.905938
##  [66]    960.273538    312.467487  -1055.731914    812.243538    474.690675
##  [71]    355.980628   -148.370277    293.309505    346.575430    222.499990
##  [76]    348.028918    213.745445   -471.531577   -476.448576    520.009135
##  [81]   -437.255270   -439.637811  -1111.508492     10.207080   -765.602747
##  [86]   -461.813460   -663.701275   -501.896011    487.476201  -1065.682578
##  [91]   -363.670785   -241.754143   -721.376238      1.807224  -1342.175881
##  [96]   -844.367986  -1073.578758  -1063.359106  -2422.322578   2371.536611
## [101]  -3255.826134   1323.760522    659.142551   1004.884728    645.678418
## [106]  -4016.202966   -826.203837    547.958211    838.999254   1096.241402
## [111]  -2470.190229   1959.190549   -187.743915    469.575958   -867.733235
## [116]   -715.902939  -1947.412887   -558.842782  -2209.232276  -1123.484504
## [121]  -1661.269801  -2189.173765   -261.558323    539.683046  -2020.728490
## [126]   -569.582676  -1254.918071     44.473341  -1237.166403  -1666.276900
## [131]  -1038.613789    136.476702  -1965.464800   -890.460158  -1682.558413
## [136]  -1059.532594  -1732.653859   -805.604722   -904.273880   -938.320411
## [141]  -1251.272581   -516.537251   -718.150390    263.235019  -1309.542331
## [146]  -1465.062271  -1087.521153  -1535.117536    484.714050  -1363.940606
## [151]   1209.810361  -4750.092952   5643.137332  -3292.930217   3024.062824
## [156]   1256.418874   1758.466780  -4201.568718   -173.979548   2013.048089
## [161]   3846.088671  -2830.661361    578.098516    385.762822   2185.157219
## [166]   3729.876716  11271.483003   2589.058516 -20842.570560    309.551120
## [171]  -3220.916260  -2710.781727  -2024.901996     65.292627   3847.439631
## [176]   1745.934958   -455.756678  -2525.227716    458.613852
## 
## $avar
## [1] 6256618
## 
## $aic
## [1] 15.78323
## 
## $aicc
## [1] 16.80673
## 
## $bic
## [1] 15.99691
## 
## $se.phi
## [1] 0.4783432 1.8311687 0.2568318 0.6886451 1.1536891 0.2076721
## 
## $se.theta
## [1] 0.48003574 0.17431655 0.25747933 0.17245118 0.08399616
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

So that we can actually build a model using our AR(7) and ARMA(6,5) framework, we will use the maximum likelihood estimate to provide us the phis and thetas to help us build a model. A maximum likelihood estimation method is a probability distribution that maximizes a likelihood function to help us determine the best probability parameters for our fit. 

For our Positive Dataset, we can see that the respective phis for our AR(7) model is 1.76405274 -0.88142813 -0.06765031  0.17643910  0.37028688 -0.65347541  0.23814701. This gives us the model below:

1-1.76B^2 + 0.88B^3 + 0.067B^4 - 0.18B^5 - 0.37B^6 + 0.65B^7 - 0.24B^8 = a_t

For our Negative Dataset, we can see the respective phis for our ARMA(6,5) model are 1.8428774 -0.8968270 -0.6407014  1.5967127 -1.0525593  0.1229353, and our respective thetas are 0.45129414  0.24224825 -0.53596162  0.75762713  0.08476119. This gives us the model below: 

(1-1.84B^2 + 0.89B^3 + 0.64B^4 - 1.6B^5 + 1.05B^6 - 0.122B^7)X_t = -0.45a_t^2 - 0.24a_t^3 + 0.54a_t^4 - 0.76a_t^5 - 0.08a_t^6

Now lets look into the Seasonality of these models. 

#### Differences


```r
## Positive ## 

s1 = artrans.wge(fluDf$Positive, c(rep(0,1),1))
```

![](Flustudy_case5_files/figure-html/first difference model-1.png)<!-- -->

```r
parzen.wge(s1)
```

```
## $freq
##  [1] 0.005649718 0.011299435 0.016949153 0.022598870 0.028248588 0.033898305
##  [7] 0.039548023 0.045197740 0.050847458 0.056497175 0.062146893 0.067796610
## [13] 0.073446328 0.079096045 0.084745763 0.090395480 0.096045198 0.101694915
## [19] 0.107344633 0.112994350 0.118644068 0.124293785 0.129943503 0.135593220
## [25] 0.141242938 0.146892655 0.152542373 0.158192090 0.163841808 0.169491525
## [31] 0.175141243 0.180790960 0.186440678 0.192090395 0.197740113 0.203389831
## [37] 0.209039548 0.214689266 0.220338983 0.225988701 0.231638418 0.237288136
## [43] 0.242937853 0.248587571 0.254237288 0.259887006 0.265536723 0.271186441
## [49] 0.276836158 0.282485876 0.288135593 0.293785311 0.299435028 0.305084746
## [55] 0.310734463 0.316384181 0.322033898 0.327683616 0.333333333 0.338983051
## [61] 0.344632768 0.350282486 0.355932203 0.361581921 0.367231638 0.372881356
## [67] 0.378531073 0.384180791 0.389830508 0.395480226 0.401129944 0.406779661
## [73] 0.412429379 0.418079096 0.423728814 0.429378531 0.435028249 0.440677966
## [79] 0.446327684 0.451977401 0.457627119 0.463276836 0.468926554 0.474576271
## [85] 0.480225989 0.485875706 0.491525424 0.497175141
## 
## $pzgram
##  [1]   5.96606118   6.24985463   6.62071624   6.98284604   7.26280739
##  [6]   7.41494483   7.41568250   7.25637995   6.93818179   6.46912801
## [11]   5.86293275   5.13879803   4.32170848   3.44265810   2.53821563
## [16]   1.64889776   0.81618853   0.07873190  -0.53121269  -0.99019543
## [21]  -1.28503535  -1.41411647  -1.38863328  -1.23262048  -0.98051255
## [26]  -0.67245961  -0.34908893  -0.04746032   0.20105272   0.37120950
## [31]   0.44333265   0.40232630   0.23667696  -0.06234559  -0.50116346
## [36]  -1.08392831  -1.81194627  -2.68218805  -3.68441600  -4.79640274
## [41]  -5.97727832  -7.16141112  -8.26042249  -9.18478489  -9.88463425
## [46] -10.38017692 -10.74973543 -11.08829839 -11.47389278 -11.95555506
## [51] -12.55364240 -13.26133467 -14.04221541 -14.82496314 -15.50417549
## [56] -15.96436149 -16.13256544 -16.02232485 -15.72059090 -15.33388092
## [61] -14.94744683 -14.61513305 -14.36579786 -14.21247916 -14.15921306
## [66] -14.20512026 -14.34661227 -14.57854750 -14.89490675 -15.28933454
## [71] -15.75573116 -16.28895103 -16.88554282 -17.54435954 -18.26679667
## [76] -19.05641094 -19.91772320 -20.85407231 -21.86441363 -22.93898990
## [81] -24.05409499 -25.16724875 -26.21648962 -27.12980660 -27.84735211
## [86] -28.34562051 -28.64256552 -28.77663010
```

```r
aic5.wge(s1)
```

```
## ---------WORKING... PLEASE WAIT... 
## 
## 
## Five Smallest Values of  aic
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["   p"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["   q"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["       aic"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"1","3":"14.40133","_rn_":"14"},{"1":"5","2":"1","3":"14.40151","_rn_":"17"},{"1":"2","2":"1","3":"14.41201","_rn_":"8"},{"1":"3","2":"1","3":"14.42316","_rn_":"11"},{"1":"1","2":"1","3":"14.43202","_rn_":"5"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plotts.sample.wge(s1)
```

![](Flustudy_case5_files/figure-html/first difference model-2.png)<!-- -->![](Flustudy_case5_files/figure-html/first difference model-3.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.83957743  0.54524409  0.34318631  0.27449694  0.24406773
##  [7]  0.11975898 -0.06926317 -0.21626285 -0.26465276 -0.23633525 -0.22077178
## [13] -0.25416571 -0.28056682 -0.25559360 -0.20416513 -0.16297150 -0.13846811
## [19] -0.11769618 -0.09473144 -0.07199215 -0.05196673 -0.03801987 -0.03004991
## [25] -0.02630430 -0.02692315
## 
## $freq
##  [1] 0.005649718 0.011299435 0.016949153 0.022598870 0.028248588 0.033898305
##  [7] 0.039548023 0.045197740 0.050847458 0.056497175 0.062146893 0.067796610
## [13] 0.073446328 0.079096045 0.084745763 0.090395480 0.096045198 0.101694915
## [19] 0.107344633 0.112994350 0.118644068 0.124293785 0.129943503 0.135593220
## [25] 0.141242938 0.146892655 0.152542373 0.158192090 0.163841808 0.169491525
## [31] 0.175141243 0.180790960 0.186440678 0.192090395 0.197740113 0.203389831
## [37] 0.209039548 0.214689266 0.220338983 0.225988701 0.231638418 0.237288136
## [43] 0.242937853 0.248587571 0.254237288 0.259887006 0.265536723 0.271186441
## [49] 0.276836158 0.282485876 0.288135593 0.293785311 0.299435028 0.305084746
## [55] 0.310734463 0.316384181 0.322033898 0.327683616 0.333333333 0.338983051
## [61] 0.344632768 0.350282486 0.355932203 0.361581921 0.367231638 0.372881356
## [67] 0.378531073 0.384180791 0.389830508 0.395480226 0.401129944 0.406779661
## [73] 0.412429379 0.418079096 0.423728814 0.429378531 0.435028249 0.440677966
## [79] 0.446327684 0.451977401 0.457627119 0.463276836 0.468926554 0.474576271
## [85] 0.480225989 0.485875706 0.491525424 0.497175141
## 
## $db
##  [1]  -6.0057726   0.8104559   9.9336706   7.0327924   6.7707826  -2.0985300
##  [7]  13.5198107   7.5264257  -1.2169877   9.0409526  -3.3662713   5.2151601
## [13]  -0.7831832 -13.0404415   4.7233424  -4.5057170  -3.5109435   3.3443116
## [19]  -7.6315554   2.2277583  -4.8962545  -7.3098157  -5.2914439   1.1591924
## [25]  -9.1289755  -6.6230051   1.6529951  -0.1254455  -3.9731473  -5.6351430
## [31]   6.4891237  -2.0457652  -1.0671831   2.3884306  -4.8839973  -3.0598447
## [37]  -2.1406235  -0.6544461  -4.3564456  -8.7671505 -14.8913362 -28.5609090
## [43] -12.7553828  -6.2256538 -27.9784161 -14.7628656  -9.4374075 -11.2048630
## [49] -11.2261248 -14.8272892  -9.8329427 -14.7989361 -14.9871315 -21.1258173
## [55] -19.7037862 -19.4202367 -19.5453743 -13.7107877 -15.2977080 -25.6154491
## [61] -11.2142238 -15.6184736 -21.3628964 -13.9682693 -31.9663890  -9.8021883
## [67] -15.3295931 -16.5899462 -10.5726049 -18.2653804 -28.3421152 -24.9547679
## [73] -13.4536297 -16.1311390 -48.2077433 -16.7027833 -24.5052478 -21.2292689
## [79] -28.0536687 -30.4485928 -26.0511098 -28.3934749 -35.8444548 -36.5183028
## [85] -29.2717660 -29.9461558 -28.4603966 -28.6353913
## 
## $dbz
##  [1]   5.96606118   6.24985463   6.62071624   6.98284604   7.26280739
##  [6]   7.41494483   7.41568250   7.25637995   6.93818179   6.46912801
## [11]   5.86293275   5.13879803   4.32170848   3.44265810   2.53821563
## [16]   1.64889776   0.81618853   0.07873190  -0.53121269  -0.99019543
## [21]  -1.28503535  -1.41411647  -1.38863328  -1.23262048  -0.98051255
## [26]  -0.67245961  -0.34908893  -0.04746032   0.20105272   0.37120950
## [31]   0.44333265   0.40232630   0.23667696  -0.06234559  -0.50116346
## [36]  -1.08392831  -1.81194627  -2.68218805  -3.68441600  -4.79640274
## [41]  -5.97727832  -7.16141112  -8.26042249  -9.18478489  -9.88463425
## [46] -10.38017692 -10.74973543 -11.08829839 -11.47389278 -11.95555506
## [51] -12.55364240 -13.26133467 -14.04221541 -14.82496314 -15.50417549
## [56] -15.96436149 -16.13256544 -16.02232485 -15.72059090 -15.33388092
## [61] -14.94744683 -14.61513305 -14.36579786 -14.21247916 -14.15921306
## [66] -14.20512026 -14.34661227 -14.57854750 -14.89490675 -15.28933454
## [71] -15.75573116 -16.28895103 -16.88554282 -17.54435954 -18.26679667
## [76] -19.05641094 -19.91772320 -20.85407231 -21.86441363 -22.93898990
## [81] -24.05409499 -25.16724875 -26.21648962 -27.12980660 -27.84735211
## [86] -28.34562051 -28.64256552 -28.77663010
```

```r
s2 = artrans.wge(s1, c(rep(0,51),1))
```

![](Flustudy_case5_files/figure-html/first difference model-4.png)<!-- -->

```r
parzen.wge(s2)
```

```
## $freq
##  [1] 0.008 0.016 0.024 0.032 0.040 0.048 0.056 0.064 0.072 0.080 0.088 0.096
## [13] 0.104 0.112 0.120 0.128 0.136 0.144 0.152 0.160 0.168 0.176 0.184 0.192
## [25] 0.200 0.208 0.216 0.224 0.232 0.240 0.248 0.256 0.264 0.272 0.280 0.288
## [37] 0.296 0.304 0.312 0.320 0.328 0.336 0.344 0.352 0.360 0.368 0.376 0.384
## [49] 0.392 0.400 0.408 0.416 0.424 0.432 0.440 0.448 0.456 0.464 0.472 0.480
## [61] 0.488 0.496
## 
## $pzgram
##  [1]   3.5279844   4.2042926   5.0338229   5.8121960   6.4277961   6.8315938
##  [7]   7.0059879   6.9473895   6.6584311   6.1452499   5.4175348   4.4907002
## [13]   3.3907333   2.1631353   0.8868879  -0.3117814  -1.2685101  -1.8456502
## [19]  -2.0266036  -1.9310251  -1.7294539  -1.5637699  -1.5263207  -1.6709760
## [25]  -2.0278280  -2.6125284  -3.4297927  -4.4715783  -5.7093969  -7.0802827
## [31]  -8.4717962  -9.7288679 -10.7161207 -11.4107977 -11.9166686 -12.3736976
## [37] -12.8678844 -13.3950830 -13.8614871 -14.1218285 -14.0717570 -13.7355995
## [43] -13.2465742 -12.7526176 -12.3568804 -12.1131271 -12.0417043 -12.1443809
## [49] -12.4135555 -12.8371271 -13.4008295 -14.0892686 -14.8864112 -15.7759307
## [55] -16.7414654 -17.7663923 -18.8321523 -19.9136523 -20.9702716 -21.9329047
## [61] -22.6944867 -23.1242954
```

```r
aic5.wge(s2)
```

```
## ---------WORKING... PLEASE WAIT... 
## 
## 
## Five Smallest Values of  aic
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["   p"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["   q"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["       aic"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"1","3":"14.59263","_rn_":"14"},{"1":"3","2":"1","3":"14.60484","_rn_":"11"},{"1":"2","2":"1","3":"14.61124","_rn_":"8"},{"1":"1","2":"1","3":"14.62149","_rn_":"5"},{"1":"2","2":"0","3":"14.78728","_rn_":"7"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plotts.sample.wge(s2)
```

![](Flustudy_case5_files/figure-html/first difference model-5.png)<!-- -->![](Flustudy_case5_files/figure-html/first difference model-6.png)<!-- -->

```
## $autplt
##  [1]  1.000000000  0.828325440  0.516026498  0.237472654  0.035216843
##  [6] -0.108623899 -0.279604200 -0.412891680 -0.437010268 -0.356821874
## [11] -0.223020444 -0.128507450 -0.088618855 -0.046585811  0.005017106
## [16]  0.037682385  0.044338483  0.036130435  0.024756385  0.017729035
## [21]  0.016779916  0.020855096  0.022057044  0.017210531  0.010944624
## [26]  0.004927502
## 
## $freq
##  [1] 0.008 0.016 0.024 0.032 0.040 0.048 0.056 0.064 0.072 0.080 0.088 0.096
## [13] 0.104 0.112 0.120 0.128 0.136 0.144 0.152 0.160 0.168 0.176 0.184 0.192
## [25] 0.200 0.208 0.216 0.224 0.232 0.240 0.248 0.256 0.264 0.272 0.280 0.288
## [37] 0.296 0.304 0.312 0.320 0.328 0.336 0.344 0.352 0.360 0.368 0.376 0.384
## [49] 0.392 0.400 0.408 0.416 0.424 0.432 0.440 0.448 0.456 0.464 0.472 0.480
## [61] 0.488 0.496
## 
## $db
##  [1]   2.2828491  -4.3494835   2.5114662   6.3528092  -6.6242239  12.6327432
##  [7] -12.8819814  10.7454485   3.6286618   3.6382778   6.7861746  -0.1954222
## [13]   4.1513475  -0.1213190   1.4968897  -9.2700464  -2.6902504  -7.8230159
## [19]  -3.4845698  -5.5304116  -3.0098230  -3.1904788   0.4291869   2.4693912
## [25]  -2.0400316  -2.5945061  -6.8668909  -6.6221078  -7.3625254 -10.8212428
## [31] -20.0090522 -12.2271799 -13.6765447 -11.4518932  -9.1665251  -9.5851479
## [37] -11.6482975 -29.1435684 -25.6537135 -19.8378666 -21.6485478 -13.8762334
## [43] -21.2144604  -8.7436605 -17.6608634  -8.3951900 -13.2395535 -17.3032695
## [49]  -8.3533345 -16.7865503 -12.4202015 -17.2355007 -18.9842835 -13.9384679
## [55] -15.2803035 -20.6431277 -20.5960261 -24.5816317 -28.3244794 -24.7287049
## [61] -26.6963275 -28.0540042
## 
## $dbz
##  [1]   3.5279844   4.2042926   5.0338229   5.8121960   6.4277961   6.8315938
##  [7]   7.0059879   6.9473895   6.6584311   6.1452499   5.4175348   4.4907002
## [13]   3.3907333   2.1631353   0.8868879  -0.3117814  -1.2685101  -1.8456502
## [19]  -2.0266036  -1.9310251  -1.7294539  -1.5637699  -1.5263207  -1.6709760
## [25]  -2.0278280  -2.6125284  -3.4297927  -4.4715783  -5.7093969  -7.0802827
## [31]  -8.4717962  -9.7288679 -10.7161207 -11.4107977 -11.9166686 -12.3736976
## [37] -12.8678844 -13.3950830 -13.8614871 -14.1218285 -14.0717570 -13.7355995
## [43] -13.2465742 -12.7526176 -12.3568804 -12.1131271 -12.0417043 -12.1443809
## [49] -12.4135555 -12.8371271 -13.4008295 -14.0892686 -14.8864112 -15.7759307
## [55] -16.7414654 -17.7663923 -18.8321523 -19.9136523 -20.9702716 -21.9329047
## [61] -22.6944867 -23.1242954
```

```r
## Negative ## 

s3 = artrans.wge(fluDf$Negative, c(rep(0,1),1))
```

![](Flustudy_case5_files/figure-html/first difference model-7.png)<!-- -->

```r
parzen.wge(s3)
```

```
## $freq
##  [1] 0.005649718 0.011299435 0.016949153 0.022598870 0.028248588 0.033898305
##  [7] 0.039548023 0.045197740 0.050847458 0.056497175 0.062146893 0.067796610
## [13] 0.073446328 0.079096045 0.084745763 0.090395480 0.096045198 0.101694915
## [19] 0.107344633 0.112994350 0.118644068 0.124293785 0.129943503 0.135593220
## [25] 0.141242938 0.146892655 0.152542373 0.158192090 0.163841808 0.169491525
## [31] 0.175141243 0.180790960 0.186440678 0.192090395 0.197740113 0.203389831
## [37] 0.209039548 0.214689266 0.220338983 0.225988701 0.231638418 0.237288136
## [43] 0.242937853 0.248587571 0.254237288 0.259887006 0.265536723 0.271186441
## [49] 0.276836158 0.282485876 0.288135593 0.293785311 0.299435028 0.305084746
## [55] 0.310734463 0.316384181 0.322033898 0.327683616 0.333333333 0.338983051
## [61] 0.344632768 0.350282486 0.355932203 0.361581921 0.367231638 0.372881356
## [67] 0.378531073 0.384180791 0.389830508 0.395480226 0.401129944 0.406779661
## [73] 0.412429379 0.418079096 0.423728814 0.429378531 0.435028249 0.440677966
## [79] 0.446327684 0.451977401 0.457627119 0.463276836 0.468926554 0.474576271
## [85] 0.480225989 0.485875706 0.491525424 0.497175141
## 
## $pzgram
##  [1]   6.68399868   6.68000194   6.64516424   6.54864678   6.36368643
##  [6]   6.07492449   5.68279379   5.20566366   4.67940550   4.15229622
## [11]   3.67317630   3.27516533   2.96394929   2.71958415   2.51042175
## [16]   2.30890766   2.10101263   1.88801102   1.68318428   1.50624818
## [21]   1.37738414   1.31197178   1.31662389   1.38693637   1.50737308
## [26]   1.65345600   1.79568888   1.90393541   1.95096881   1.91456642
## [31]   1.77825712   1.53122436   1.16791567   0.68779052   0.09549806
## [36]  -0.59834017  -1.37569701  -2.20926201  -3.06085770  -3.88178460
## [41]  -4.61755278  -5.21869608  -5.65534611  -5.92795159  -6.06675178
## [46]  -6.12061848  -6.14300618  -6.18177916  -6.27444604  -6.44701026
## [51]  -6.71426031  -7.08013000  -7.53757554  -8.06807133  -8.64153745
## [56]  -9.21834216  -9.75536837 -10.21657907 -10.58448258 -10.86563888
## [61] -11.08602064 -11.27931816 -11.47524521 -11.69255530 -11.93739761
## [66] -12.20570050 -12.48813602 -12.77622327 -13.06771502 -13.36932840
## [71] -13.69593898 -14.06710854 -14.50287009 -15.02036919 -15.63188410
## [76] -16.34385493 -17.15623306 -18.06165303 -19.04450887 -20.08090480
## [81] -21.14126273 -22.19690358 -23.22862516 -24.23008672 -25.19695245
## [86] -26.09885468 -26.84579008 -27.28624090
```

```r
aic5.wge(s3)
```

```
## ---------WORKING... PLEASE WAIT... 
## 
## 
## Five Smallest Values of  aic
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["   p"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["   q"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["       aic"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2","3":"15.93681","_rn_":"6"},{"1":"5","2":"2","3":"15.93890","_rn_":"18"},{"1":"4","2":"2","3":"15.94446","_rn_":"15"},{"1":"4","2":"1","3":"15.96768","_rn_":"14"},{"1":"2","2":"1","3":"15.97260","_rn_":"8"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plotts.sample.wge(s3)
```

![](Flustudy_case5_files/figure-html/first difference model-8.png)<!-- -->![](Flustudy_case5_files/figure-html/first difference model-9.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.76426054  0.38369773  0.18819597  0.16544804  0.21860855
##  [7]  0.22177908  0.13975321  0.02556657 -0.01863369  0.02816731  0.06185734
## [13]  0.01957163 -0.05544283 -0.10631505 -0.13384263 -0.14210304 -0.14079001
## [19] -0.16276902 -0.19405248 -0.20878212 -0.20711157 -0.20194861 -0.20398352
## [25] -0.22156848 -0.24894561
## 
## $freq
##  [1] 0.005649718 0.011299435 0.016949153 0.022598870 0.028248588 0.033898305
##  [7] 0.039548023 0.045197740 0.050847458 0.056497175 0.062146893 0.067796610
## [13] 0.073446328 0.079096045 0.084745763 0.090395480 0.096045198 0.101694915
## [19] 0.107344633 0.112994350 0.118644068 0.124293785 0.129943503 0.135593220
## [25] 0.141242938 0.146892655 0.152542373 0.158192090 0.163841808 0.169491525
## [31] 0.175141243 0.180790960 0.186440678 0.192090395 0.197740113 0.203389831
## [37] 0.209039548 0.214689266 0.220338983 0.225988701 0.231638418 0.237288136
## [43] 0.242937853 0.248587571 0.254237288 0.259887006 0.265536723 0.271186441
## [49] 0.276836158 0.282485876 0.288135593 0.293785311 0.299435028 0.305084746
## [55] 0.310734463 0.316384181 0.322033898 0.327683616 0.333333333 0.338983051
## [61] 0.344632768 0.350282486 0.355932203 0.361581921 0.367231638 0.372881356
## [67] 0.378531073 0.384180791 0.389830508 0.395480226 0.401129944 0.406779661
## [73] 0.412429379 0.418079096 0.423728814 0.429378531 0.435028249 0.440677966
## [79] 0.446327684 0.451977401 0.457627119 0.463276836 0.468926554 0.474576271
## [85] 0.480225989 0.485875706 0.491525424 0.497175141
## 
## $db
##  [1]  -1.1854820   3.4367891  12.0556565   8.3084229   5.9367714  -5.0583537
##  [7]   6.9840447   6.3613646  -5.0251525   6.2138956   4.8908095  -8.8761061
## [13]   2.0703064   4.2925067  -3.8262178   3.6519339  -0.2199637   1.8659535
## [19]   1.5800691   0.1565913   2.7381733   1.2429775   2.0517263   3.4932669
## [25]  -1.0258119   0.2234122   1.3572890  -0.9233304   2.6370227   0.9436668
## [31]   4.8969209   2.4288529   0.3511250   4.0059952  -3.4809275  -5.1389280
## [37]  -0.2195225 -11.8991418  -7.0231512  -5.2546970 -10.4193738  -5.0508272
## [43]  -6.7433371  -7.3908505  -3.7149131  -7.7615856  -6.3002133  -6.4139416
## [49]  -3.7824865  -7.8133164  -6.2811514  -6.9073508  -7.3026143  -7.7637026
## [55]  -8.1668053 -11.1932077 -12.6872587 -14.0154765 -12.6351268 -10.8793061
## [61]  -9.3530490  -9.3737946 -17.4451387 -12.4481739  -9.2649093 -15.6520322
## [67] -16.6169884  -9.9728641 -18.5833571 -11.7434915 -13.1251117 -23.5181464
## [73] -10.2903810 -16.7461233 -24.0187036 -13.3941375 -38.6934303 -28.4253005
## [79] -21.2755129 -23.3812022 -21.1157236 -22.0905110 -24.3137001 -23.7984077
## [85] -35.3326612 -29.4873290 -33.6401242 -39.9861751
## 
## $dbz
##  [1]   6.68399868   6.68000194   6.64516424   6.54864678   6.36368643
##  [6]   6.07492449   5.68279379   5.20566366   4.67940550   4.15229622
## [11]   3.67317630   3.27516533   2.96394929   2.71958415   2.51042175
## [16]   2.30890766   2.10101263   1.88801102   1.68318428   1.50624818
## [21]   1.37738414   1.31197178   1.31662389   1.38693637   1.50737308
## [26]   1.65345600   1.79568888   1.90393541   1.95096881   1.91456642
## [31]   1.77825712   1.53122436   1.16791567   0.68779052   0.09549806
## [36]  -0.59834017  -1.37569701  -2.20926201  -3.06085770  -3.88178460
## [41]  -4.61755278  -5.21869608  -5.65534611  -5.92795159  -6.06675178
## [46]  -6.12061848  -6.14300618  -6.18177916  -6.27444604  -6.44701026
## [51]  -6.71426031  -7.08013000  -7.53757554  -8.06807133  -8.64153745
## [56]  -9.21834216  -9.75536837 -10.21657907 -10.58448258 -10.86563888
## [61] -11.08602064 -11.27931816 -11.47524521 -11.69255530 -11.93739761
## [66] -12.20570050 -12.48813602 -12.77622327 -13.06771502 -13.36932840
## [71] -13.69593898 -14.06710854 -14.50287009 -15.02036919 -15.63188410
## [76] -16.34385493 -17.15623306 -18.06165303 -19.04450887 -20.08090480
## [81] -21.14126273 -22.19690358 -23.22862516 -24.23008672 -25.19695245
## [86] -26.09885468 -26.84579008 -27.28624090
```

```r
s4 = artrans.wge(s3, c(rep(0,51),1))
```

![](Flustudy_case5_files/figure-html/first difference model-10.png)<!-- -->

```r
parzen.wge(s4)
```

```
## $freq
##  [1] 0.008 0.016 0.024 0.032 0.040 0.048 0.056 0.064 0.072 0.080 0.088 0.096
## [13] 0.104 0.112 0.120 0.128 0.136 0.144 0.152 0.160 0.168 0.176 0.184 0.192
## [25] 0.200 0.208 0.216 0.224 0.232 0.240 0.248 0.256 0.264 0.272 0.280 0.288
## [37] 0.296 0.304 0.312 0.320 0.328 0.336 0.344 0.352 0.360 0.368 0.376 0.384
## [49] 0.392 0.400 0.408 0.416 0.424 0.432 0.440 0.448 0.456 0.464 0.472 0.480
## [61] 0.488 0.496
## 
## $pzgram
##  [1]   1.3080345   1.8082858   2.4583193   3.1143344   3.6889267   4.1433475
##  [7]   4.4686479   4.6713031   4.7647106   4.7647327   4.6877308   4.5500900
## [13]   4.3684003   4.1593707   3.9385573   3.7174755   3.4997699   3.2782878
## [19]   3.0350345   2.7445898   2.3797377   1.9173709   1.3434237   0.6566621
## [25]  -0.1283272  -0.9789018  -1.8438093  -2.6566146  -3.3463475  -3.8568541
## [31]  -4.1694021  -4.3149041  -4.3652114  -4.4090939  -4.5285770  -4.7849034
## [37]  -5.2127175  -5.8168957  -6.5681909  -7.3989636  -8.2083766  -8.8912164
## [43]  -9.3865773  -9.7059574  -9.9090217 -10.0563238 -10.1847727 -10.3135889
## [49] -10.4621451 -10.6614327 -10.9525716 -11.3769094 -11.9652764 -12.7300101
## [55] -13.6592637 -14.7127367 -15.8209517 -16.8936596 -17.8403471 -18.5928460
## [61] -19.1119069 -19.3768023
```

```r
aic5.wge(s4)
```

```
## ---------WORKING... PLEASE WAIT... 
## 
## 
## Five Smallest Values of  aic
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["   p"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["   q"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["       aic"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"16.19479","_rn_":"5"},{"1":"3","2":"1","3":"16.23473","_rn_":"11"},{"1":"4","2":"1","3":"16.29747","_rn_":"14"},{"1":"0","2":"1","3":"16.31230","_rn_":"2"},{"1":"2","2":"1","3":"16.33648","_rn_":"8"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plotts.sample.wge(s4)
```

![](Flustudy_case5_files/figure-html/first difference model-11.png)<!-- -->![](Flustudy_case5_files/figure-html/first difference model-12.png)<!-- -->

```
## $autplt
##  [1]  1.00000000  0.67549788  0.17341495 -0.11672406 -0.21585977 -0.17785721
##  [7] -0.10947495 -0.07289684 -0.11685249 -0.13578314 -0.06586625 -0.01244790
## [13] -0.01141658 -0.02098042 -0.02103722 -0.04175782 -0.03485792  0.01609384
## [19]  0.01254622 -0.02062024 -0.03035633 -0.01499525  0.01142683  0.02998334
## [25]  0.02653704  0.00341478
## 
## $freq
##  [1] 0.008 0.016 0.024 0.032 0.040 0.048 0.056 0.064 0.072 0.080 0.088 0.096
## [13] 0.104 0.112 0.120 0.128 0.136 0.144 0.152 0.160 0.168 0.176 0.184 0.192
## [25] 0.200 0.208 0.216 0.224 0.232 0.240 0.248 0.256 0.264 0.272 0.280 0.288
## [37] 0.296 0.304 0.312 0.320 0.328 0.336 0.344 0.352 0.360 0.368 0.376 0.384
## [49] 0.392 0.400 0.408 0.416 0.424 0.432 0.440 0.448 0.456 0.464 0.472 0.480
## [61] 0.488 0.496
## 
## $db
##  [1]   1.8655871  -2.6189569   1.6857278   2.4075398  -3.5849705   7.5090595
##  [7]   3.1408837   7.3882545   6.1194004   6.3896509   3.6794000   2.2329304
## [13]  -9.2339664   4.2491208   0.5774457   6.3526299   3.8827020   5.9186191
## [19]   2.8124332   3.6555849   1.6687019   0.8117278  -2.0820221   0.8006924
## [25]  -4.5076322   0.3578375  -1.2761251  -1.7160504  -3.8722576  -8.8063560
## [31]  -7.6287539  -3.7328246 -10.8101355  -4.4867322  -2.6585512  -2.9120180
## [37]  -2.6134241  -6.5578906  -9.3956151 -13.9535724 -10.5124181 -11.3831269
## [43]  -7.7037791 -11.9718178  -8.9449383  -8.8109960 -10.3287169 -31.6566092
## [49]  -7.5259258 -19.6508101 -10.2879064  -8.9826410 -11.2547381 -10.5331385
## [55] -18.0087427 -25.6315479 -14.2952187 -19.8497229 -29.8666717 -23.6675964
## [61] -21.0509457 -22.8393313
## 
## $dbz
##  [1]   1.3080345   1.8082858   2.4583193   3.1143344   3.6889267   4.1433475
##  [7]   4.4686479   4.6713031   4.7647106   4.7647327   4.6877308   4.5500900
## [13]   4.3684003   4.1593707   3.9385573   3.7174755   3.4997699   3.2782878
## [19]   3.0350345   2.7445898   2.3797377   1.9173709   1.3434237   0.6566621
## [25]  -0.1283272  -0.9789018  -1.8438093  -2.6566146  -3.3463475  -3.8568541
## [31]  -4.1694021  -4.3149041  -4.3652114  -4.4090939  -4.5285770  -4.7849034
## [37]  -5.2127175  -5.8168957  -6.5681909  -7.3989636  -8.2083766  -8.8912164
## [43]  -9.3865773  -9.7059574  -9.9090217 -10.0563238 -10.1847727 -10.3135889
## [49] -10.4621451 -10.6614327 -10.9525716 -11.3769094 -11.9652764 -12.7300101
## [55] -13.6592637 -14.7127367 -15.8209517 -16.8936596 -17.8403471 -18.5928460
## [61] -19.1119069 -19.3768023
```

```r
e1 = est.arma.wge(s1, p=7, q=0)
```

```
## 
## Coefficients of Original polynomial:  
## 1.6300 -1.3112 0.8167 -0.5769 0.7320 -0.6282 0.1521 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1-0.6925B+0.8270B^2    0.4186+-1.0168i      0.9094       0.1878
## 1-1.6500B+0.7445B^2    1.1082+-0.3393i      0.8628       0.0473
## 1+1.0828B+0.6670B^2   -0.8117+-0.9167i      0.8167       0.3653
## 1-0.3703B              2.7004               0.3703       0.0000
##   
## 
```

```r
e1
```

```
## $phi
## [1]  1.6300425 -1.3112100  0.8167003 -0.5768581  0.7319607 -0.6282234  0.1520832
## 
## $theta
## [1] 0
## 
## $res
##   [1]   169.560924  -300.363012  1787.552530  1070.214957 -1821.898120
##   [6]  -667.825459 -3031.624596  1270.890105   -26.102081  -651.416397
##  [11]  1206.505433 -1261.987389   588.938530 -1937.217698  -159.297953
##  [16]    52.822093   854.593029  -175.319042  -542.755891   -49.408469
##  [21]  -181.620546  -281.254625    29.620624    64.519669    17.788996
##  [26]   -42.507369    19.567700    23.959461    19.929440   -14.342493
##  [31]   -21.584244    91.623654    98.975191   -20.626338    27.356510
##  [36]    19.407637    73.068971    72.852496   -19.974713    79.020128
##  [41]   225.644867   214.357979    65.143117   394.397614   330.775528
##  [46]   503.622317  1248.308653  1833.625846  5263.627826  -656.183487
##  [51]  -636.277874  2504.189727 -3380.737964  2412.948532 -1115.388723
##  [56]  -128.094388 -1351.077908 -4998.706288    34.253215  -305.182714
##  [61]  1635.517324  -580.382923  1592.506618  -117.961982 -2001.248805
##  [66] -1239.943967  -570.958473   144.136582  -582.335739   423.852312
##  [71]  -112.872611  -164.330404  -125.493123    93.207013  -119.879003
##  [76]   -92.547187   -13.599100    27.233838     3.345982     6.869440
##  [81]    17.521109    30.149049     7.499379    89.604453    -7.585805
##  [86]    60.070815    49.252711   -11.810929    10.478638   143.442691
##  [91]     4.595727    99.932643   178.594529    44.422920   276.534566
##  [96]   100.051545   259.098493   318.118373   686.922450  1841.058711
## [101]  1183.047705  -670.167658 -1599.788462   569.186246  1026.809985
## [106]   233.047595  1886.977023  2110.667463 -1586.386248  -292.879219
## [111]   251.056490 -1151.629760   219.945868 -2969.710886   -10.366847
## [116]  -422.192249  -717.048542  -548.891028   504.922438  1193.289149
## [121]  -868.670299   -96.325532  -607.496886    35.686065  -410.071068
## [126]  -142.726613  -178.226639   116.311915    48.042588  -114.949781
## [131]    67.828455    15.194638   -17.010554   -11.808654    75.548881
## [136]    75.218232    73.445996    -9.919868   -33.034547   165.389239
## [141]   -16.066359  -251.005463   275.378693   174.629334    56.463391
## [146]   204.504155   982.528546    80.100415   932.085422  -112.086041
## [151]   606.910417  2667.283594  3654.182845  2061.810544 -6045.527133
## [156]   -16.795374  4298.173191  -160.464267  1200.308139 -1074.965503
## [161]     6.124868 -1243.319010 -2935.831224  -307.582818 -1820.830023
## [166] -3638.542715   141.444428  3321.694544 -1181.508102  -463.528078
## [171]  2121.816558   315.785350 -2452.651052 -1342.742788   376.608881
## [176]    84.752195    61.456307
## 
## $avar
## [1] 1681058
## 
## $aic
## [1] 14.42533
## 
## $aicc
## [1] 15.44272
## 
## $bic
## [1] 14.56888
## 
## $se.phi
## [1] 0.005485588 0.018341124 0.025361771 0.027497281 0.025384374 0.018634969
## [7] 0.005673148
## 
## $se.theta
## [1] 0
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

```r
e2 = est.arma.wge(s4, p=6, q=5)
```

```
## 
## Coefficients of Original polynomial:  
## 0.3814 0.8633 -0.0168 -0.6291 0.2483 -0.0027 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1+1.5479B+0.8551B^2   -0.9051+-0.5918i      0.9247       0.4078
## 1-0.8051B              1.2420               0.8051       0.0000
## 1-1.1129B+0.3504B^2    1.5883+-0.5759i      0.5919       0.0554
## 1-0.0112B              89.0954               0.0112       0.0000
##   
## 
```

```r
e2
```

```
## $phi
## [1]  0.381421775  0.863263591 -0.016825684 -0.629097010  0.248276928
## [6] -0.002707381
## 
## $theta
## [1] -0.91536609  0.97176462  1.30514574  0.02815755 -0.38984795
## 
## $res
##   [1]   4905.4487   4354.8327   5049.1258  -6783.4663   4528.7853  -7488.9760
##   [7]   5024.8136  -4750.9655   3860.3823  -4402.2084   5140.4017  -4422.2617
##  [13]   6414.5971  -6238.0437   6466.7895  -3753.1796   3756.9726  -4463.7918
##  [19]   5761.1364  -4158.4920   5119.2242  -3985.4083   5916.8336  -4255.8644
##  [25]   4866.1918  -3967.8009   5459.8873  -3578.0528   4806.5143  -3503.5036
##  [31]   5044.4074  -3729.4513   5218.8537  -3595.9540   6290.7760  -4044.4628
##  [37]   6106.1119  -3750.9342   6263.5569  -2955.5342   6221.9795  -4087.5682
##  [43]   5688.7715  -4058.4627   6703.0475  -3097.2950   3366.9464  -2559.6845
##  [49]   1882.5808  -3768.5781   4569.2060 -10486.2148   5232.2007 -11038.9154
##  [55]   6767.4245  -1925.2917   5632.7405   2353.9717   9170.1142  -2413.9472
##  [61]   8715.8857  -2545.7066   5023.6027  -2085.2954   5328.6786  -1068.7086
##  [67]   6179.0502  -5018.6019   7633.6871  -1583.4870   5998.8553  -2867.7425
##  [73]   6187.5730  -2752.0857   6337.5750  -2989.2185   7375.6823  -2744.5496
##  [79]   5976.2797  -2889.0002   7504.3862  -3262.1197   6797.6220  -2732.3412
##  [85]   7354.8069  -2513.3684   6145.8431  -1784.8288   7483.5573  -1846.2403
##  [91]   7466.7628  -3621.0513   7826.5549  -2615.0800   9335.6306  -1937.3132
##  [97]  11308.9918  -9209.0644  15726.7862  -5227.6231   9344.6854   -755.2950
## [103]   8478.1568  -2997.2431   8860.5935   -446.1209  10386.4857  -5319.3201
## [109]   8386.5750  -2373.5975   8218.6649   2247.8651  18875.6555    811.3313
## [115] -13308.0325  -3921.2679   7136.4041  -2968.9032   7150.5651   -652.5159
## [121]   4804.0582  -2791.7021   7350.9081  -3650.2653   7807.0732
## 
## $avar
## [1] 35983342
## 
## $aic
## [1] 17.59057
## 
## $aicc
## [1] 18.6328
## 
## $bic
## [1] 17.86208
## 
## $se.phi
## [1] -1.55412257  0.33723288 -0.46907306  0.18297572 -0.13642441 -0.01978541
## 
## $se.theta
## [1] -1.56835225 -1.52508244 -0.07663541 -1.52535296 -0.24597971
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

Considering, visually, we have a large seasonality associated with our positive tests, so let's see if we can reduce the impact of it by adjusting for the weekly cyclic behavior in the data. 

Accounting for the weekly seasonality by running ar trans on the weekly data we have over 3 years, we can see that we have a smooth spectral density and a realization that relies less on on the 4 seasonal factors in the winter and shows more of the change in demand year over year. We can see that we have an improved aic score on the weekly data. This makes sense as our dataset is broken out in week long intervals. 

Regarding the negative instances, we can see that our weekly seasonality model has the lower AIC score  and a smoother spectral density plot. That said, it doesn't improve our AIC score, so we will will not difference the data before moving forward. 

### Forecast


```r
f1 = fore.aruma.wge(fluDf$Positive, phi = e1$phi,  s=52, n.ahead = 45, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast1-1.png)<!-- -->

```r
#ase1 = mean(((f1$f-fluDf$Positive[135:179])))
#ase1

ase1 = ase(fluDf$Positive, f1)

f1_neg = fore.aruma.wge(fluDf$Negative, phi = e2$phi, theta = e2$theta, s=52, n.ahead = 45, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast1-2.png)<!-- -->

```r
#ase2 = mean(((f2$f-fluDf$Negative[135:179])))
ase2 = ase(fluDf$Negative, f1_neg)


f2 = fore.aruma.wge(fluDf$Positive, phi = e1$phi, s=26, n.ahead = 13, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast1-3.png)<!-- -->

```r
#ase1 = mean(((f1$f-fluDf$Positive[154:179])))
ase3 = ase(fluDf$Positive, f2)


f2_neg = fore.aruma.wge(fluDf$Negative, phi = e2$phi, theta = e2$theta, s=26, n.ahead = 13, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast1-4.png)<!-- -->

```r
#ase1 = mean(((f2$f-fluDf$Negative[154:179])))
ase4 = ase(fluDf$Negative, f2_neg)
```

Now that we have our models and have addressed the seasonality phenomenon with regards to the spike that we see in our positive and negative instances dataset, we will want to begin to forecast the future diagnoses of the flu so that we can mobilize resources to best serve the need in the winter of Q4. In order to ensure that we are best forecasting future data without actuals to measure the accuracy against, we will need to look in the past to see if we can use our model to best predict the most recent 9 months of data. 

In order to do this, we will create a prediction based on 2017 through the first half of 2019 data, and we will overlay that with the actual data that occurred from September 2019 through June 2020. From there, we will test the difference between our predicted value and the respective actuals aggregated over the 45 week window and use the Average Squared Error score to evaluate performance. 

And then we will look to use a bi-weekly seasonality model to which we are looking to predict the next 13 weeks of data. Doing so for both Positive and Negative Results. 


From this, we get our ASE score baseline which we can look to improve upon. 

Now lets run a Burg Model to see if we can improve on our AIC scores. 

### Burg Model


```r
be1 = est.ar.wge(fluDf$Positive, p=7, type = 'burg')
```

```
## 
## Coefficients of Original polynomial:  
## 1.7661 -0.8820 -0.0626 0.1667 0.3807 -0.6633 0.2422 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1-1.8380B+0.8763B^2    1.0487+-0.2034i      0.9361       0.0305
## 1-0.6768B+0.7440B^2    0.4548+-1.0664i      0.8625       0.1858
## 1+1.3185B+0.6518B^2   -1.0114+-0.7150i      0.8074       0.4021
## 1-0.5698B              1.7549               0.5698       0.0000
##   
## 
```

```r
be1
```

```
## $phi
## [1]  1.76613344 -0.88202450 -0.06256226  0.16666095  0.38071794 -0.66334254
## [7]  0.24216554
## 
## $res
##   [1]   868.654260  -145.041790  -254.403708   208.754840  1540.981500
##   [6]  1207.634595 -1573.411947   265.111348 -3459.219188  2275.999103
##  [11] -1340.147929  1036.244310  -552.966175   623.376814  -698.817874
##  [16]     7.900269  -811.712578   413.615556   432.672824  -234.635814
##  [21]  -297.696389   -80.062988  -445.336332  -368.863568  -296.286717
##  [26]  -251.849223  -335.583612  -298.904408  -339.470689  -287.818387
##  [31]  -332.692394  -339.352809  -375.488573  -248.430176  -286.876615
##  [36]  -342.197221  -325.131273  -323.069628  -287.643820  -269.734267
##  [41]  -360.026716  -263.027804  -133.042958  -132.692976  -243.859957
##  [46]   122.931508   -42.538172   306.981334   932.197922  1632.703893
##  [51]  5068.427765  -925.790012  1045.015835  1962.804956 -2336.577153
##  [56]  2767.021096  -981.167662   657.608439 -1948.456399 -2508.221214
##  [61] -1112.856891  1409.308211   717.042215   382.869635  2403.582225
##  [66]  -126.279222   -19.488528  -881.197197   192.914076  -507.917715
##  [71]  -128.077940   -61.640246    -4.135443  -434.288469  -145.220103
##  [76]  -222.390428  -375.960207  -330.154307  -322.638555  -351.400482
##  [81]  -317.689992  -356.983339  -338.797467  -323.546269  -358.032587
##  [86]  -265.858319  -378.132780  -276.592736  -336.037428  -331.374039
##  [91]  -376.107669  -171.232518  -417.221951  -172.125701  -244.202725
##  [96]  -234.785362  -101.033563  -179.170248   -71.134335    25.347055
## [101]   421.150035  1510.193051   934.561886  -553.254607 -1380.710292
## [106]   633.814925   465.318943   440.244445  1703.435937  2044.627344
## [111] -1441.263430  1013.800932   117.069654  -397.596218   224.316330
## [116] -2389.695549   111.449246  -474.281939    -4.255259  -985.453739
## [121]  1574.930379    67.182034   465.544108  -234.238198   -11.247523
## [126]  -283.456446  -229.635544  -433.695585  -330.988430  -230.048426
## [131]  -294.764729  -394.406125  -242.929183  -346.359898  -326.146625
## [136]  -368.798875  -239.009717  -333.411107  -213.079933  -404.279009
## [141]  -311.049790  -231.664805  -333.299830  -598.089773   -13.217913
## [146]  -311.515102  -180.823540  -186.785331   743.551242  -453.645179
## [151]  1084.434519  -752.875206  1023.100899  1832.852314  4007.199194
## [156]  1463.362067 -4592.234427   801.612762  3395.861513   172.427734
## [161]  1837.731612  -526.728869   213.714327  -497.033254  -946.824922
## [166]  -526.153665  -842.800597 -3947.513162  1193.373544  2934.581227
## [171]  -778.239869   618.640908  2237.159345   150.287361 -1258.068005
## [176]  -495.973847  -422.327698  -309.573003  -348.185277
## 
## $avar
## [1] 1293353
## 
## $aic
## [1] 14.16213
## 
## $aicc
## [1] 15.17926
## 
## $bic
## [1] 14.30459
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

```r
#be2 = est.arma.wge(fluDf$Negative, p=6, q=5, type = 'burg')
#be2
#mean(fluDf$Positive)
```
Note that the Burg model only works on AR models, so we won't be able to use it on the ARMA (6,5) negative data. Doing so, we get a model parameter series of phi = (1.76613344 -0.88202450 -0.06256226  0.16666095  0.38071794 -0.66334254  0.24216554) which is slightly different from the MLE model we ran previously. A Burg estimation works to minimize the least squares through backwards prediction errors while constraining AR parameters. 

Next we will check it against our ASE score. 


```r
bf1 = fore.aruma.wge(fluDf$Positive, phi = be1$phi,  s=52, n.ahead = 45, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast burg-1.png)<!-- -->

```r
ase_bf1 = ase(fluDf$Positive, bf1)


bf2 = fore.aruma.wge(fluDf$Positive, phi = c(1.76613344, -0.88202450, -0.06256226, 0.16666095, 0.38071794, -0.66334254,  0.24216554), s=26, n.ahead = 13, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/forecast burg-2.png)<!-- -->

```r
ase_bf2 = ase(fluDf$Positive, bf2)
```
After running a burg estimation, we did not see an improvement in our ASE score to predict our 52 week model accurately, but we did see a positive result in predicting the bi-weekly report accurately.

Now lets try the Yule Walker estimate approach.


```r
bje1 = est.ar.wge(fluDf$Positive, p=7, type = 'yw')
```

```
## 
## Coefficients of Original polynomial:  
## 1.6394 -0.6972 -0.1047 0.1646 0.2630 -0.4775 0.1489 
## 
## Factor                 Roots                Abs Recip    System Freq 
## 1-1.8485B+0.8856B^2    1.0436+-0.2000i      0.9411       0.0301
## 1-0.6271B+0.6751B^2    0.4645+-1.1250i      0.8216       0.1877
## 1+1.2621B+0.5849B^2   -1.0790+-0.7387i      0.7648       0.4045
## 1-0.4258B              2.3484               0.4258       0.0000
##   
## 
```

```r
bje1
```

```
## $phi
## [1]  1.6393810 -0.6971789 -0.1047377  0.1645558  0.2629677 -0.4774660  0.1488986
## 
## $res
##   [1]   969.811586   138.394067  -347.158588   154.527151  1717.044131
##   [6]  1569.956881 -1407.315729    32.790042 -3601.782736  1897.330307
##  [11]  -968.522514   894.500304  -386.094886   191.965563  -485.162654
##  [16]   -38.589980  -680.951320   332.622897   377.710372  -173.832793
##  [21]  -283.889336  -124.473186  -441.532973  -405.576849  -357.323160
##  [26]  -322.701332  -384.919812  -366.171968  -402.977846  -357.654755
##  [31]  -395.439406  -406.627753  -445.895585  -327.873888  -347.846949
##  [36]  -403.886865  -394.549659  -400.804764  -356.836630  -330.673860
##  [41]  -421.081472  -336.028011  -202.619292  -177.250451  -275.426511
##  [46]    67.460586   -57.949024   291.356019   963.932864  1738.363600
##  [51]  5309.406824  -244.691280  1097.016391  2044.672735 -2150.054679
##  [56]  2927.248922  -765.336587   563.396249 -1795.510329 -3059.091441
##  [61] -1210.599086  1112.348541   990.833912   550.637037  2227.927790
##  [66]   134.688305   146.083711  -660.719074   135.894451  -416.831601
##  [71]  -209.787652   -69.304754   -40.567670  -410.354227  -193.063566
##  [76]  -273.448487  -408.657878  -379.019556  -391.613440  -412.450325
##  [81]  -388.289681  -427.910795  -410.508630  -397.942138  -429.134537
##  [86]  -339.032569  -444.541610  -352.374480  -402.053745  -405.844304
##  [91]  -441.720757  -253.566342  -467.569578  -248.894631  -299.139808
##  [96]  -297.366037  -146.464743  -226.456626  -108.722970    -5.429929
## [101]   399.644631  1559.028182  1122.696249  -389.161846 -1440.513504
## [106]   391.052513   551.906444   623.445436  1800.950486  2106.247569
## [111] -1184.981899   936.378810   229.397034  -267.009521   339.443674
## [116] -2523.051368  -182.543080  -549.746742   -49.296426  -819.828678
## [121]  1263.326248   224.253374   510.303268   -75.923321   -27.297215
## [126]  -237.596871  -249.812273  -433.461372  -382.055372  -298.262776
## [131]  -347.016274  -450.389694  -316.107026  -409.387419  -392.428738
## [136]  -433.243494  -320.476422  -389.579942  -283.187623  -458.263491
## [141]  -391.979189  -300.671406  -393.647020  -655.702633  -122.281721
## [146]  -360.043801  -232.585164  -227.326753   670.353239  -370.913185
## [151]  1035.734228  -640.308192   923.751495  1980.461588  4215.288222
## [156]  2126.565193 -4343.370402   225.317447  3277.135923   841.504268
## [161]  2368.562257  -655.791237   -75.539358  -393.530664  -875.421677
## [166]  -284.401415 -1072.447058 -4205.588841   620.802202  2939.230923
## [171]  -166.599605   768.269877  1970.162127   345.091764  -881.331122
## [176]  -487.059339  -471.263416  -386.498368  -416.322978
## 
## $avar
## [1] 1332903
## 
## $aic
## [1] 14.19226
## 
## $aicc
## [1] 15.20938
## 
## $bic
## [1] 14.33471
```

```r
mean(fluDf$Positive)
```

```
## [1] 6568.207
```

```r
bjf1 = fore.aruma.wge(fluDf$Positive, phi = bje1$phi,  s=52, n.ahead = 45, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/Yule walker-1.png)<!-- -->

```r
ase_bjf1 = ase(fluDf$Positive, bjf1)


bjf2 = fore.aruma.wge(fluDf$Positive, phi = bje1$phi, s=26, n.ahead = 13, lastn = TRUE)
```

![](Flustudy_case5_files/figure-html/Yule walker-2.png)<!-- -->

```r
ase_bjf2 = ase(fluDf$Positive, bjf2)
```
Next, we will use the Yule-Walker estimation to forecast future flu needs. A Yule-Walker esetimate chooses parameters for which the moments are equal to the empirical moments. 


```r
#ase_bjf1
#ase_bjf2
```




Now let us try a neural model to see if we can improve. We will run an MLP model as well as an ELM model to see if we can improve upon the ASE scores for the MLE, Yule Walker and Burg Method. 

#### Neural Forecast


```r
flu_train = ts(fluDf$Positive[1:134], start = c(2017,1),frequency = 52)
nonflu_train = ts(fluDf$Negative[1:134], start = c(2017,1),frequency = 52)

set.seed(255)
fit.mlp= mlp(flu_train, lags=1:52, hd.auto.type="cv", hd.max=20, reps=500)
fit.mlp
```

```
## MLP fit with 2 hidden nodes and 500 repetitions.
## Series modelled in differences: D1.
## Univariate lags: (1,4,5,6,8,10,11,12,13,15,16,20,21,22,26,27,30,32,36,37,39,42,44,45,49,50)
## Forecast combined using the median operator.
## MSE: 76617.6183.
```

```r
plot(fit.mlp)
```

![](Flustudy_case5_files/figure-html/forecast model-1.png)<!-- -->




```r
h=45
frc <- forecast(fit.mlp,h=h)
plot(frc)
```

![](Flustudy_case5_files/figure-html/forecast neural-1.png)<!-- -->

```r
frc$f <- frc$mean
ase_mlp = ase(flu_train, frc)
#ase_mlp

# Plotting Forecast vs actual for MLP
plot(fluDf$Positive, type = "l",
     main='Flu Rates',
     xlab="Week"
     ,ylab="Cases")
lines(seq(134,178,1),frc$mean,col = "blue")
#axis(1, at=1:37, labels=seq(1936,1972,1),las=2)
legend(26, 3, legend=c("MLP Forecast", 'Actual'),
       col=c("green", 'black'), lty=1, cex=0.9)
```

![](Flustudy_case5_files/figure-html/forecast neural-2.png)<!-- -->
Next we will perform a multi-layer perceptron model which is a class of feed forward artificial neural networks. It utilizes 3 layers of nodes: an input layer, output layer and hidden layer, and each node is a neuron that uses back-propagation for training to use on future forecast data. Here, we use it for a 45 week landscape. We can clearly see the our MLP model is under-predicting, which is also reflected in the ASE performance comparatively.


```r
set.seed(255)
fit <- elm(flu_train, m=52, difforder=c(1,52), comb= "mean", reps=500, type = c("lasso", "ridge", "step", "lm"))
print(fit)
```

```
## ELM fit with 27 hidden nodes and 500 repetitions.
## Series modelled in differences: D1D52.
## Univariate lags: (1,52)
## Forecast combined using the mean operator.
## Output weight estimation using: lasso.
## MSE: 1185853.8571.
```

```r
plot(fit)
```

![](Flustudy_case5_files/figure-html/elm-1.png)<!-- -->

```r
frc<-forecast(fit,h=45)
frc$f <- frc$mean
ase_elm = ase(flu_train, frc)
#ase_elm
```

```r
# Plotting Forecast vs actual for ELM
plot(fluDf$Positive, type = "l",
     main='Flu Rates',
     xlab="Week"
     ,ylab="Cases")
lines(seq(134,178,1),frc$mean,col = "blue")
#axis(1, at=1:37, labels=seq(1936,1972,1),las=2)
legend(26, 3, legend=c("MLP Forecast", 'Actual'),
       col=c("green", 'black'), lty=1, cex=0.9)
```

![](Flustudy_case5_files/figure-html/plot-1.png)<!-- -->

Next, we will perform an ELM function which is a Single Hidden Layer Feed-forward Neural Net. Standing for Extreme Learning Machine algorithm (ELM), it randomly generates weights with no gradient-based back-propagation. What this means is that it can create a much more complex neural net than our mlp forecast net, which should allow us to create a better prediction of flu cases at the end of 2020. 

Visually, we can see that it pretty closely traces our historical data with our line much better than our MLE, MLP, Yule Walker and Burg estimation. 


### Results

For our 26 week horizons, we generating the following ASE scores for our forecasts. 

* MLE - 10,432,882
* Burg - 19,395,470
* Yule Walker - 21,524,957

Here, we can see that the MLE approach has the lowest Average Square Error, which means that the distance from the predicted values is the lowest from the actual values across our timeframe estimates. 

On our 52 week Horizons, we are generating the following ASE scores for our forecasts. 

* MLE - 34,279,493
* Burg - 34,348,066
* Yule Walker - 34,315,621
* MLP (neural) - 51,191,123
* ELM (neural) - 72,972,566

Here, we can see the most efficient 52 week model is the Maximum likelihood Estimation, and is likley the best fit given these data.

### Conclusion

After running through 5 different models in our research study, we found that the best overall forecaster (using historical data as a pressure test) was the MLE over a 52 week horizon. It was able to generate a much lower score, for shorter forcasting periods. 

With that said, there is practicality of using a 26 week time frame in our modeling. Considering how quickly the markets and trends change over a span of a few months, a longer tail of time may factor outdated data into our forecast. Since the MLE model performed best for shorter periods, and equally best, albeit marginally, for a 52 week period, we chose the MLE.

Both of the best performing forecast models (MLE and Yule Walker) models suggest there will be a peak of positive flu cases at about 30k at the end of the year, which the company will need to account for in the supply chain and distribution planning.


```r
dt<- data.frame(seq(1,45))
dt$Cases <- f1$f
colnames(dt)<-c("Week Forecast - Starting June 2020", "Cases")
knitr::kable(dt)
```

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Week Forecast - Starting June 2020 </th>
   <th style="text-align:right;"> Cases </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 199.03757 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 157.65820 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 97.83472 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 136.82852 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 144.31953 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 187.51177 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 209.26088 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 228.67874 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 224.03013 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 375.46849 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 427.15142 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 602.83453 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 796.16508 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 1004.40911 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 1394.10439 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 1759.23138 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 2253.54154 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2881.11316 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3988.23717 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 6564.49540 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 9536.93668 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 11102.36073 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 10439.87888 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 10273.49562 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 11191.58508 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 12927.39321 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 15594.70699 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 18459.29556 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 18479.86822 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 18866.95969 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 18912.29563 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 18698.58533 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 18623.72663 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 14881.19386 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 11687.17176 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 8495.90908 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 6350.74193 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 4095.91841 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 2806.44634 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1861.04034 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 1508.13375 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 1250.47409 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 965.56518 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 767.24819 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 628.86231 </td>
  </tr>
</tbody>
</table>

### Deployment

As COVID-19 has created a “new normal” with regards to how society practices safety to prevent the spread of illness, it’s more important than ever for drug and treatment companies to ensure they stay pliable to meet the demands. While our 52 week model gave us the greatest overall forecast, we will also need to realize that the Q4 of 2020 will be much different in the US and Globally than 2019 and the years before. So with this in mind, we would recommend looking more towards a 26 week horizon and allowing for more recent data to dictate the future. 

Per our findings, we can see that we will have a soft peak at around 25k positive cases with an even bigger peak of 30k cases at the height of December going into January. Since it takes our company 6-7 months to produce and roll out treatment materials, it’s very important to begin now to ensure we are producing and supplying resources to meet that demand of 30k individuals. 


### Resources:

https://cran.r-project.org/web/packages/ELMR/ELMR.pdf

http://scikit-learn.org/stable/modules/neural_networks_supervised.html 

https://www.stat.berkeley.edu/~bartlett/courses/153-fall2010/lectures/12.pdf

https://www.mathworks.com/help/dsp/ref/burgarestimator.html

