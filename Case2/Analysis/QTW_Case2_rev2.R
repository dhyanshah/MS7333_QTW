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
library(tidyverse)
library(magrittr)


#txt = readLines("offline.final.trace.txt")

offlinetxt = readLines("http://rdatasciencecases.org/Data/offline.final.trace.txt")
onlinetxt = readLines("http://rdatasciencecases.org/Data/online.final.trace.txt")

sum(substr(offlinetxt, 1, 1) == "#")
length(offlinetxt)
sum(substr(onlinetxt, 1, 1) == "#")
length(onlinetxt)

## Cleaning Offline Data ## 
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)
}

offlineclean = strsplit(offlinetxt[4], "[;=,]")[[1]]
offlineclean[1:10]

### Extract the First Four Variables

offlineclean[c(2, 4, 6:8, 10)]

## strip the first 4 elements
offlineclean[ - (1:10)]

# Need to insure we have one location per row. So we will need to bind each of these sets to 4 values.

cleaning_tmp = matrix(offlineclean[ - (1:10) ], ncol = 4, byrow = TRUE)
cleaning_mat = cbind(matrix(offlineclean[c(2, 4, 6:8, 10)], nrow = nrow(cleaning_tmp), ncol = 6, byrow = TRUE), cleaning_tmp)

dim(cleaning_mat)

## Apply to each row of the text file ##

## Function to parse the data
processLine = function(x) {
  # split x by the 3 delim's (;=,)
  cleaning = strsplit(x, "[;=,]")[[1]]
  
  # if the lenth of the field is equa to 10, then return NULL
  if (length(cleaning) == 10)
    return(NULL)
  # create a variable cleaning_temp = a matrix of with 4 columns 
  # then, cbind the matrix 
  cleaning_tmp = matrix(cleaning[ - (1:10)], ncol = 4, byrow = TRUE) 
  cbind(matrix(cleaning[c(2, 4, 6:8, 10)], nrow = nrow(cleaning_tmp), ncol = 6, byrow = TRUE), cleaning_tmp) 
}

## Test this function 
cleaning_tmp = lapply(offlinetxt[4:20], processLine)
sapply(cleaning_tmp, nrow)


## combine all into a dataframe ##

offline = as.data.frame(do.call("rbind", cleaning_tmp))
dim(offline)

lines = offlinetxt[ substr(offlinetxt, 1, 1) != "#" ]
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)


dim(offline)
head(offline)

names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal",
                   "channel", "type")

head(offline)

offline[offline$time==1139643118358,]

head(offline)

## Convert position, signal and time variables from strings to numeric data types.##

numVars = c("time", "posX", "posY", "posZ", "orientation", "signal")
offline[ numVars ] = lapply(offline[ numVars ], as.numeric)

head(offline)


## Let's change the type of the device to something more comprehensable than the numbers 1 and 3. 
## We are going to change the type to adhoc and access point. 
## However, let's plan to only unse the signal strengths mreasured to 
## the fixed access points to develop and test our models.

offline = offline[ offline$type == "3", ]
offline = offline[ , "type" != names(offline) ]
dim(offline)

## In our documentation, time is measured in the number of milliseconds from midnight on Jan. 1st, 1970. 
## This is the origin used fo rhte POSIXt format, but with POSIXt, it is the number of seconds, 
## not milliseconds. Let's scale the value of time to seconds and then set the class of the time 
## element in order to have the values appear as date-times in IR.

offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")

head(offline)

unlist(lapply(offline, class))

## We have the correct shape for the data and even the correct types. 
## We can run a summary statistic to see which values appear to look reasonable.

## Let's take a look at the descriptive statistics ##

summary(offline[, numVars])

## Notice here that our descriptive statistics indicate that position z's values are all zero. 
## We should be able to drop this value without consequence on our remaining modeling columns.


## Now lets look at the character variables to factors and examine them with. ##

str(offline)

summary(sapply(offline[ , c("mac", "channel", "scanMac")], as.factor))
offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]

## Looking at our summary of our non numeric values, we can see that "scanMac" has only one unique value. 
## So we should be able to drop along with Position Z for a cleaner dataset.

head(offline)

#select(offline,-c(scanMac,channel,type))
#select(offline,-c(channel,type))
select(offline,-c(channel))

#offline[offline['posZ']!='0.0',]

vals = data.frame(table(offline['mac']))
vals[order(-vals$Freq),]

offline$signal %<>% as.integer

#out<-select(offline, -c(channel,scanMac)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))
out<-select(offline, -c(channel)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))

out$nas<-rowSums(is.na(out))

out


##########################
### Now Let's Clean the Online Data
##########################
test = readLines("http://rdatasciencecases.org/Data/online.final.trace.txt")

head(test)

sum(substr(test, 1, 1) == "#")

length(test)

dim(onlinetxt)

### split our columns based on the semi colon, brackets, commas and equals signs that appears between them.
test_cleaning = strsplit(test[4], "[;=,]")[[1]]
test_cleaning[1:10]

test_cleaning[c(2, 4, 6:8, 10)]

## Pull out the first four elements of the array.
test_cleaning[ - (1:10)]


## Like our offline data, we will need our online data to only include one location per line. 
## To do this, we will bind each of these sets of 4 values from the responding device within the 4 values of the line.

test_cleaning_tmp = matrix(test_cleaning[ - (1:10) ], ncol = 4, byrow = TRUE)
test_cleaning_mat = cbind(matrix(test_cleaning[c(2, 4, 6:8, 10)], nrow = nrow(test_cleaning_tmp), ncol = 6, byrow = TRUE), test_cleaning_tmp)

dim(test_cleaning_mat)

## from here, we can iterate for each row of the txt file

## Test this function 
test_cleaning_tmp = lapply(test[4:20], processLine)
sapply(test_cleaning_tmp, nrow)

## now, we can combine all of these individual matrices into a single dataframe.
online = as.data.frame(do.call("rbind", test_cleaning_tmp))
dim(online)

## Discard the "#" character and pass through the process line

lines = test[ substr(test, 1, 1) != "#" ]
test_cleaning_tmp = lapply(lines, processLine)
online_data = as.data.frame(do.call("rbind", test_cleaning_tmp), stringsAsFactors = FALSE)
dim(online_data)

head(online_data)

## Add names to the columns to match the offline data.
names(online_data) = c("time", "scanMac", "posX", "posY", "posZ", "orientation", "mac", "signal", "channel", "type")

head(online_data)

## Convert the numeric data to the correct data type. Paricularly the time, posx, posY, posZ, 
## orientation and signal datasets.

numVars = c("time", "posX", "posY", "posZ", "orientation", "signal")
online_data[ numVars ] = lapply(online_data[ numVars ], as.numeric)

head(online_data)

## CHange the device type to adhoc or access point.

online_data = online_data[ online_data$type == "3", ]
online_data = online_data[ , "type" != names(online_data) ]
dim(online_data)

## Over 100k records from our data frame. Now, lets take a look into the time variable 
## and how it's measured in the number of milliseconds from midnight on January 1st 1970. 
## We are now going to scale the value of time into seconds and and then set the class of 
## the time element in order to have the values appear and operate as date-times.

online_data$rawTime = online_data$time
online_data$time = online_data$time/1000
class(online_data$time) = c("POSIXt", "POSIXct")

head(online_data)

## Review the data types across our dataframe
unlist(lapply(online_data, class))

## Run a quick summary of our dataframe.
summary(online_data[, numVars])

head(online_data)

str(online_data)

## Look into the summary of character variables and convert into factors. 
## Additionally, let's see if scanMac has a single vlaue like we saw with the offline data.

summary(sapply(online_data[ , c("mac", "channel", "scanMac")], as.factor))

## Like we saw before, there's only one value for ScanMac where all the 
## Mac addresses were recorded from. Additionally, we are only seeing that 
## position Z has a singular value of 0. meaning that our measurements were 
## taken from a single floor in a building. A 2 dimensional plain.

online_data = online_data[ , !(names(online_data) %in% c("scanMac", "posZ"))]
head(online_data)

###  EDA: Exploring orientation
## According to our documentation, we should have only 8 values for orientation. Let's check this

length(unique(offline$orientation))

plot(ecdf(offline$orientation), main = 'Empirical CDF of Orientation for the Hand-Held Device', sub = 'Figure 1',
     xlab = 'Empirical CDF', ylab = 'Orientation')


## This empiracle distribution function of orientation shows that there are 8 basic orientations 
## that are 45 degrees apart. We see from the steps in the function that these orientations are 
## not exactly 45, 90, 135, ect. Also, the 0 oreintation is split into the two groups, one near 
## 0 and the other near 360.

roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

## Let's create a function that can round off the angles that we are seeing in our function. 
## We created a function that taks the 45 degree angles and rounds them off.

offline$angle = roundOrientation(offline$orientation)
online_data$angle = roundOrientation(online_data$orientation)


## For our data angle, we are going to focus on rounding them to the nearest 45 to make 
## for easier classification modeling. We can check the results int he boxplot below.

with(offline, boxplot(orientation ~ angle, 
                      xlab = 'nearest 45 degree angle', 
                      ylab = 'orientation'))

## We can see that the new values look correct and the original values near 360 degrees are mapped to zero. Also, 
## we can see that the slight changes in the location of zero from our map indicate that there is slight variability.

#### Exploring MAC Addresses ###

## from our descriptive summary statistics, it seems like there might be a one-to-one mapping between 
## the MAC address of the access points and channel. To help us ascertain of we do one to one mapping, 
## we look at the relatioship between MAC address and channel.

c(length(unique(offline$mac)), length(unique(offline$channel)))

## Here, we can see that there are 12 MAC adderesses and 8 channels. 
## We can get the impression from the building plan that there are 6 access points. 
## Rereading the documentation, we can see that there are addtiional access points 
## that are not part of the testing area.

## Below we can check the counts of observations for various MAC addresses with table()

table(offline$mac)

## Here, we can see that the first MAC address (00:04:0e:5c:23:fc) and 
## the last two (00:30:bd:f8:7f:c5 & 00:e3:63:82:8b:a9) were not near 
## the testing area or they were only activated for a short period of time.

## Moving forward, let's just focus on the records from the top 7 devices, which we can do with the following code:

subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
offline = offline[offline$mac %in% subMacs, ]

subMacs

## Next, we can create a table of counts for the remaining MACx channel combinations 
## and confirm if there is one non-zero entry in each row:

macChannel = with(offline, table(mac, channel))
apply(macChannel, 1, function(x) sum(x >0))

## Since we now see that there is a one-to-one correspondence between 
## MAC and channel for the 7 devices we searched, we can eliminate channel 
## from our offline dataset to further clean or data.

offline = offline[ , 'channel' != names(offline)]
online_data = online_data[ , "channel" != names(online_data)]

## Looking further into the position of hand held devices using the position variables posX and posY, 
## we can look into how may different locations that are available to indicate where we have data.

## The by() function below can tally up the numbers of rows in each x,y combination.

dim(offline)
head(offline)

###  Exploring the Position of Handheld Device ### 

## Lastly consider the position variables, of posX and poxY. 
## We can use the by function to tally the number of rows in 
## our data frame for each unique (x,y) combination. To do so, 
## we can create a list containing the data frame for each location.

locDF = with(offline, by(offline, list(posX, posY), function(x) x))
length(locDF)

## 476 is longer than the number of actual x, y locations at which the 
## measurements were recorded, many of which were empty.

sum(sapply(locDF, is.null))

## We can drop these unneeded elements as follows.
locDF = locDF[ !sapply(locDF, is.null)]
length(locDF)
locCounts = sapply(locDF, nrow)

## below. we can can keep the position information with the location. 
## Below, we can operate on each of these data frames to detirmine the 
## number of observations recorded at each location

locCounts = sapply(locDF, nrow)
locCounts = sapply(locDF, 
                   function(df)
                     c(df[1, c('posX', 'posY')], count = nrow(df)))
class(locCounts)
dim(locCounts)

## Let's examine some of the counts.
locCounts[ , 1:8]

## We can see that there are over 5,500 recordings at each position, 
## which aligns with our 8 orientations, 110 replications and 7 access 
## points which should give us 6,160 measurements.

## Lets visualize all 166 counts by adding the counts as texts to their 
## respective locations. To do so, we can transpose the matrix so that 
## the locations are columns of the matrix.

locCounts = t(locCounts)
plot(locCounts, type = 'n', xlab = '', ylab = '')
text(locCounts, labels = locCounts[,3], cex = 0.8, srt = 45)

## Signal Strength Analysis ##

## We can now investigate the properties of the response variable which we will use as signal strength. 
## To do so, we will need to ask the question of how the signal strengths behave. In other words, 
## what is the distribution of the repeated measuremeants at each location and orientation? 
## Does it behave similarly at all location?

## In a laboratory stetting, the signal strength tends to deplete linearly over time 
## with a log distance and a simple triangulation using the signal strength from 3 
## access points that can accurately pinpoint the location of a device. 
## In practice, buildings have walls and human activity rarely acts in a similar 
## fashion as a lab setting. So to account for this, we can delve further into the signal strengthe distribution.

## Signal Strength Distribution ## 

## Next we will compare the signal strength of different orientations and 
## for different access points, we will fix the locations on the map. 
## We are wanting to look into the distributions and if they are normal or skeweed as well as the variances.

###################################

#install.packages('readData')
#offlineRedo = readData()

## These are the counts of signals detected at each position. 
## These are plotted at each location in the building there 
## the number of signals detected from all access points for 
## the offline data. These include 110 signals at 8 angles 
## for each of 6 access points, for a total of 5280 recordings. 
## These can include a 7th Mac address, and doesn't account or 
## all signals, so we have about 5,500 recordingsat each location.

library(lattice)  
bwplot(signal ~ factor(angle) | mac, data = offline,  
       subset = posX == 2 & posY == 12  & mac != "00:0f:a3:39:dd:cd",  layout = c(2,3))

summary(offline$signal)

## The small values such as -98 correspond to weak signals and the large values such as -25 are strong singnals.
## With that said the above suggests that we have a similar dependence of signal strength on the angle.

with(offline, table(mac, posX, posY))

## looking specifically at Mac Positions where there are two that appear in the same location, 
## we can find through grouping by position y that there are a couple cases where 
## two Mac ids are in the same x/y position. These cases are as follows:

###     00:0f:a3:39:dd:cd & 00:0f:a3:39:el:c0 at PosY 3, Pos X 6 as well as posY 7 and posX 15
###     00:14:bf:b1:97:8d & 00:14:bf:b1:97:90 at PosY 3, Pos X 15 as well as Posy 4 and posX 32

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 6 & posY == 3  & mac == "00:0f:a3:39:dd:cd")
bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 6 & posY == 3  & mac == "00:0f:a3:39:e1:c0")


## Comparing the signal strength of the position X 6 and Position Y 3 location for 00:0f:a3:39:e1:c0 and 00:0f:a3:39:dd:cd. 
## Here we can see the average signal strength is much higher with 00:0f:a3:39:e1:c0 at the same location.

## Next we can test the two addresses at the next shared location:
##   Pos X 15 and PosY 7

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 15 & posY == 7  & mac == "00:0f:a3:39:dd:cd")

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 15 & posY == 7  & mac == "00:0f:a3:39:e1:c0")

## Like we saw before, the 00:0f:a3:39:e1:c0 address has a higher signal strength.

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 15 & posY == 3  & mac == "00:14:bf:b1:97:8d")
bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 15 & posY == 3  & mac == "00:14:bf:b1:97:90")

## Comparing position X == 15 and position y == 3, the signal strength of 00:14:bf:b1:97:8d is much higher than 00:14:bf:b1:97:90.

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 32 & posY == 4  & mac == "00:14:bf:b1:97:8d")

bwplot(signal ~ factor(angle) | mac, data = offline,  subset = posX == 32 & posY == 4  & mac == "00:14:bf:b1:97:90")

## At position X 32 and Position Y 4, we can see that the signal strength of 00:14:bf:b1:97:8d is highe

summary(offline$signal)

### Next, we can compare the distributions of signal strength for different angles and 
## MAC addesses and the central location of x = 23 and y = 4. 
## This helps us produce 48 density curves for this one location.

densityplot( ~ signal | mac + factor(angle), data = offline,  subset = posX == 24 & posY == 4 &  mac != "00:0f:a3:39:dd:cd",  bw = 0.5, plot.points = FALSE)


## Many of these distributions look approximately normal, but these are some serious departures within 
## the secondary modes and skewness. Also, the cneter of the distibution varies with angle and MacAddress, 
## which indicates that conditioning on angle and MAC address is warranted.

## Next, we can look into examing the distribution of signal strength for all 166 locations, 8 angles, 
## and 6 access points, we need to create thousands of boxplots and density curves. Instead, we can examine 
## the summary statistics at all access point combinations. For each combination, we have 100 observations. 
## We can create a factor that will help to compute summary statistics at these combinations.

offline$posXY = paste(offline$posX, offline$posY, sep = "-") 

byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x)) 

signalSummary = lapply(byLocAngleAP, function(oneLoc) { 
  ans = oneLoc[1, ] 
  ans$medSignal = median(oneLoc$signal) 
  ans$avgSignal = mean(oneLoc$signal) 
  ans$num = length(oneLoc$signal) 
  ans$sdSignal = sd(oneLoc$signal) 
  ans$iqrSignal = IQR(oneLoc$signal)
  ans })

offlineSummary = do.call("rbind", signalSummary)

## To check if the standard deviations vary with the average signal strength, 
## we prepare boxplots of sdSignal for subgroups of avgSignal. Also, we examine 
## the skewness of signal strength by plotting the diﬀerence, avgSignal - medSignal, 
## against the number of observations.

byLocAngleAP = with(offline,  by(offline, list(posXY, angle, mac),  function(x) x))

signalSummary =  
  lapply(byLocAngleAP,  
         function(oneLoc) {  
           ans = oneLoc[1, ]  
           ans$medSignal = median(oneLoc$signal)  
           ans$avgSignal = mean(oneLoc$signal)  
           ans$num = length(oneLoc$signal) 
           ans$sdSignal = sd(oneLoc$signal)  
           ans$iqrSignal = IQR(oneLoc$signal)  
           ans  
         }) 

offlineSummary = do.call("rbind", signalSummary)

## Below we can check if the standard deviations vary with the average signal strength via boxplots 
## for sdSignal for subgroups of avgSignal. We can also examine the difference, avgSignal - medSignal 
## against the number of observations.

breaks = seq(-90, -30, by = 5)  
bwplot(sdSignal ~ cut(avgSignal, breaks = breaks),  
       data = offlineSummary,  
       subset = mac != "00:0f:a3:39:dd:cd",  
       xlab = "Mean Signal", ylab = "SD Signal")

## We can see that the weakest signals have the small standard deviations and that it appears that the SD increases 
## with the average signal strength. If we plan to model the behavior of signal strength, then we take these features 
## into consideration.

with(offlineSummary,  
     smoothScatter((avgSignal - medSignal) ~ num,  
                   xlab = "Number of Observations",  
                   ylab = "mean - median"))  
abline(h = 0, col = "#984ea3", lwd = 2)

## Here, we can see the skewness of signal strength by plotting the difference, avg singnal - med signal against the 
## number of observations. We can do this via a smooth scatter function. We can then use loess() to locally smooth 
## the differences bewteen the mean and median with the below:

lo.obj =  with(offlineSummary,  
               loess(diff ~ num,  
                     data = data.frame(diff = (avgSignal - medSignal),  
                                       num = num)))

## We can predict the difference for each value of num and add these predictions to a scatter plot.

lo.obj.pr = predict(lo.obj, newdata = data.frame(num = (70:120))) 
##plot(diff ~ num, data)
#lines(x = 70:120, y = lo.obj.pr, col = "#4daf4a", lwd = 2)

lo.obj.pr

### Signal and Distance Relationship ### 

## Looking into the relationship between distance and signal strength, we can smooth the signal strength over 
## the regioun where it is measured to create a contour plot, similar to a topographical map. To do so, 
## we can control for the access point and orientation.

oneAPAngle = subset(offline, mac == subMacs[5] & angle == 0)

## We can then apply a color heatmap using the fields package that can fit to a survace to the signal strength 
## values at the observed locations.

oneAPAngle = subset(offlineSummary,  
                    mac == subMacs[5] & angle == 0)

install.packages('fields')
library(fields)  

smoothSS = Tps(oneAPAngle[, c("posX","posY")],  
               oneAPAngle$avgSignal)

vizSmooth = predictSurface(smoothSS) 

plot.surface(vizSmooth, type = "C") 

points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5)


## We can wrap this plotting routing into its own function that we can parameterize the MAC address and angle, 
## and if desired, other plotting parameters.


parCur = par(mfrow = c(2,2), mar = rep(1, 4)) 

#mapply(surfaceSS, mac = subMacs[ rep(c(5, 1), each = 2)],  
#angle = rep(c(0, 135), 2),  
#data = list(data = offlineSummary))
par(parCur)
head(offlineSummary)
offlineSummary = subset(offlineSummary, mac != subMacs[2])


## According to the documentation, the training data were measured at 1 meter intervals in the building so 
## we can use the grey dots on the plan to estimate the location of the access points. We ﬁnd that two MAC 
## addresses have similar heat maps and these both correspond to the access point near the center of the building
## (i.e., x =7.5 and y =6.3).

## We create a small matrix with the relevant positions for the 6 access points on the ﬂoor plan

AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8, 1, 14, 33.5, 9.3, 33.5, 2.8), 
             ncol = 2, byrow = TRUE, dimnames = list(subMacs[ -2 ], c("x", "y") ))  

AP

## Next, we are going to look into the relationship between signal strength and distance from the access point, 
## we would need to compute the distance distrances from the locations of the devices emitting the signal to the 
## access point recieving the signal. To do this we will need to compute the Euclidean distance from X and Y of 
## the handheld device to the access point.

## To visualize the findings, we would make a series of scatter plots for each access point and device orientation.

diffs = offlineSummary[ , c("posX", "posY")] - AP[ offlineSummary$mac, ]

offlineSummary$dist = sqrt(diffs[ , 1]^2 + diffs[ , 2]^2) 

xyplot(signal ~ dist | factor(mac) + factor(angle), data = offlineSummary, pch = 19, cex = 0.3, xlab ="distance")

## Based on the curvature of the plots, the signals appear to be negative values. Which makes sense as the signal would get worse the further 
## away from the device we are.


####################################
### K-Nearest Neighbors testing ####
####################################

## Since we went through the process of cleaning our offline and online data. 
## We are now able to work through a supervised learning classification procedure (K-Nearest Neighbors) 
## as one of our clustering models, and a weighted K=Nearest Neighbor as our second clustering model.

#offline$posXY = NULL

head(offline)

head(online_data)

dim(offline)

dim(online_data)

#### Unweighted K-Nearest Neighbor ###
install.packages('kknn')
library(kknn)
RTLS <- train.kknn(offline$signal~., data = offline, distance = 1, kernel = "rectangular")
RTLS


prediction <- predict(RTLS, online_data[, -6])
prediction


CM <- table(online_data[, -6], prediction)
CM


library(kknn)
non_weighted = kknn(mac ~ ., offline, online_data, na.action = na.omit(), k = 10, kernel = "rectangular")


offline2 <- offline[which(offline$signal!= 'NA'), ]
online_data2 <- online_data[which(online_data$signal!='NA'), ]

online_data$posXY = paste(online_data$posX, online_data$posY, sep = "-")


head(online_data)

head(offline)

offline2$mac = as.factor(offline$mac)
online_data2$mac = as.factor(online_data$mac)

offline2$posXY = as.factor(offline$posXY)
online_data2$posXY = as.factor(online_data$posXY)

offline2$time = as.factor(offline$time)
online_data2$time = as.factor(online_data$time)

offline2$time = as.factor(offline$posX)
online_data2$time = as.factor(online_data$posX)

offline2$time = as.factor(offline$posY)
online_data2$time = as.factor(online_data$posY)

offline2$time = as.factor(offline$orientation)
online_data2$time = as.factor(online_data$orientation)

offline2$time = as.factor(offline$signal)
online_data2$time = as.factor(online_data$signal)

offline2$time = as.factor(offline$rawTime)
online_data2$time = as.factor(online_data$rawTime)

offline2$time = as.factor(offline$angle)
online_data2$time = as.factor(online_data$angle)

#install.packages('kNN')
library(kNN)
kNN(offline2, online_data2, cl, k=5, l = 0, prob = FALSE, use.all = TRUE)

online_data$posXY = paste(online_data$posX, online_data$posY, sep = "-")
length(unique(online_data$posXY))


online_data$angle = roundOrientation(online_data$orientation)
tabonlineXYA = table(online_data$posXY, online_data$angle)
tabonlineXYA[1:6, ]

keepVars = c("posXY", "posX","posY", "orientation", "angle")
byLoc = with(online_data,
             by(online_data, list(posXY),
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 6)
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)

onlineSummary 

onlineSummary = rename(onlineSummary, c("00:0f:a3:39:e1:c0"="1", "00:14:bf:3b:c7:c6"="2", "00:14:bf:b1:97:81"="3", 
                                        "00:14:bf:b1:97:8a"="4","00:14:bf:b1:97:8d"="5", "00:14:bf:b1:97:90"="6"))

dim(onlineSummary)
names(onlineSummary)


reshapeSS = function(offline, varSignal = "signal",
                     keepVars = c("posXY", "posX","posY")) {
  byLocation =
    with(offline, by(offline, list(posXY),
                     function(x) {
                       ans = x[1, keepVars]
                       avgSS = tapply(x[ , varSignal ], x$mac, mean)
                       y = matrix(avgSS, nrow = 1, ncol = 6)
                       cbind(ans, y)
                     }))
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

selectTrain = function(angleNewObs, signals = NULL, m = 2){
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles) 
  
  offlineSubset = signals[ signals$angle %in% angles, ]
  reshapeSS(offlineSubset, varSignal = "avgSignal")
}

train130 = selectTrain(130, offlineSummary, m = 3)

head(train130)

length(train130[[1]])

findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1,
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainSubset[closest, 1:3 ])
}

predXY = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] =
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  estXY = lapply(closeXY, function(x) sapply(x[ , 2:3],
                                             function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}

estXYk1 = predXY(newSignals = onlineSummary[ , 6:11],
                 newAngles = onlineSummary[ , 4],
                 offlineSummary, numAngles = 3, k = 1)


estXYk3 = predXY(newSignals = onlineSummary[ , 6:11],
                 newAngles = onlineSummary[ , 4],
                 offlineSummary, numAngles = 3, k = 3)

calcError =
  function(estXY, actualXY)
    sum( rowSums( (estXY - actualXY)^2) )


actualXY = onlineSummary[ , c("posX", "posY")]
sapply(list(estXYk1, estXYk3), calcError, actualXY)


v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol = v,
                     nrow = floor(length(permuteLocs)/v))

onlineFold = subset(offlineSummary, posXY %in% permuteLocs[ , 1])


if (sampleAngle)
  
  x = x[x$angle == sample(refs, size = 1),]


keepVars = c("posXY", "posX","posY", "orientation", "angle") 

onlineCVSummary = reshapeSS(offline, keepVars = keepVars)

onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[ , 1])      

offlineFold = subset(offlineSummary,posXY %in% permuteLocs[ , -1])


estFold = predXY(newSignals = onlineFold[ , 6:11],
                 newAngles = onlineFold[ , 4],
                 offlineFold, numAngles = 3, k = 3)

actualFold = onlineFold[ , c("posX", "posY")]
calcError(estFold, actualFold)

K = 20
err = rep(0, K)
for (j in 1:v) {
  onlineFold = subset(onlineCVSummary,
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummary,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
  for (k in 1:K) {
    estFold = predXY(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4],
                     offlineFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}


estXYk5 = predXY(newSignals = onlineSummary[ , 6:11],
                 newAngles = onlineSummary[ , 4],
                 offlineSummary, numAngles = 3, k = 5)

calcError(estXYk5, actualXY)


predXY = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = findNN(newSignal = as.numeric(newSignals[i, ]),
                          trainSS)
  }
  estXY = lapply(closeXY, function(x)
    sapply(x[ , 2:3],
           function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}


predXY(newSignals = onlineSummary[ , 6:11],newAngles = onlineSummary[ , 4], offline)



#########################################
######### Weighted KNN ##################
#########################################
RTLS_weighted <- train.kknn(offline$signal~., data = offline, distance = 1, kernel = "rectangular")
RTLS_weighted

prediction_weighted <- predict(RTLS_weighted, online_data[, -6])
prediction_weighted

CM_weighted <- table(online_data[, -6], prediction_weighted)
CM_weighted





















