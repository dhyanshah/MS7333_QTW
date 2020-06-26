library(ggplot2)
library(tswge)
library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
library(lubridate)
library(datetime)
library(caret)
library(corrplot)
library(DMwR)
library(Hmisc)
library(ROCR)
library(stringr)
library(RVAideMemoire)
#install.packages("plotly")
library(plotly)
#install.packages('TSstudio')
library(TSstudio)

getwd()
#fluDf = read.csv(file.choose(),header = TRUE)
fluDf = read.csv("/Volumes/Dhyan-MacPC/Education/SMU /MSDS/DS 7333 Quantifying the World/Unit 8 - Case Study/flu_tidy.csv")

head(fluDf)
dim(fluDf)
summary(fluDf)

############### EDA #################

### week.year ###
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

### Year ###
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

### line ####
fig3 <- plot_ly(data = fluDf, x = ~Positive, y = ~Negative,
               marker = list(size = 10,
                             color = ~year,
                             line = list(color = ~year,
                                         width = 2)))
fig3 <- fig3 %>% layout(title = 'Flu Cases',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

fig3

#####

fig4 <- plot_ly(fluDf, x = ~year, y = ~Positive, type = 'bar', name = 'Positive')
fig4 <- fig4 %>% add_trace(y = ~Negative, name = 'Negative')
fig4 <- fig4 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig4



fig5 <- plot_ly(fluDf, x = ~week, y = ~Positive, type = 'bar', name = 'Positive')
fig5 <- fig5 %>% add_trace(y = ~Negative, name = 'Negative')
fig5 <- fig5 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig5


#####
fig6 <- plot_ly(
  type = "scatter",
  x = fluDf$year, 
  y = fluDf$Positive,
  name = 'Positive FLu Cases',
  mode = "markers",
)
fig6 <- fig6 %>%
  layout(
    title = "Flu Cases",
    xaxis = list(
      type = "Year"
    )
  )
fig6

####


################################
#all data
plotts.wge(fluDf$Positive)
plotts.wge(fluDf$Negative)

plotts.sample.wge(fluDf$Positive)
plotts.sample.wge(fluDf$Negative)

#truncated to last year
plotts.sample.wge(fluDf$Positive[0:100])
plotts.sample.wge(fluDf$Positive[100:175])
#truncated to 6 months 
plotts.sample.wge(fluDf$Negative[0:100])
plotts.sample.wge(fluDf$Negative[100:175])

length(fluDf$Positive[1:100]) 
length(fluDf$Negative[1:100]) 

acf(fluDf$Positive[1:100])
acf(fluDf$Negative[1:100])

################################
### MODELING - TIME SERIES
################################

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

#############

e1 = est.arma.wge(fluDf$Positive, p=7, q=0)
mean(fluDf$Positive)

e2 = est.arma.wge(fluDf$Negative, p=6, q=5)
mean(fluDf$Positive)
###### Differences ######
## Positive ## 

s1 = artrans.wge(fluDf$Positive, c(rep(0,11),1))
parzen.wge(s1)
aic5.wge(s1)
plotts.sample.wge(s1)


s2 = artrans.wge(fluDf$Positive, c(rep(0,51),1))
parzen.wge(s2)
aic5.wge(s2)
plotts.sample.wge(s2)

## Negative ## 

s3 = artrans.wge(fluDf$Negative, c(rep(0,11),1))
parzen.wge(s3)
aic5.wge(s3)
plotts.sample.wge(s3)


s3 = artrans.wge(fluDf$Negative, c(rep(0,51),1))
parzen.wge(s3)
aic5.wge(s3)
plotts.sample.wge(s3)

###### Forecast ######

f1 = fore.aruma.wge(fluDf$Positive,  s=52, n.ahead = 45, lastn = TRUE)
ase1 = mean(((f1$f-fluDf$Positive[135:179])))
ase1


f2 = fore.aruma.wge(fluDf$Negative, s=52, n.ahead = 45, lastn = TRUE)
ase2 = mean(((f2$f-fluDf$Negative[135:179])))
ase2


f1 = fore.aruma.wge(fluDf$Positive, s=26, n.ahead = 13, lastn = TRUE)
ase1 = mean(((f1$f-fluDf$Positive[154:179])))
ase1


f2 = fore.aruma.wge(fluDf$Negative, s=26, n.ahead = 13, lastn = TRUE)
ase2 = mean(((f2$f-fluDf$Negative[154:179])))
ase2
