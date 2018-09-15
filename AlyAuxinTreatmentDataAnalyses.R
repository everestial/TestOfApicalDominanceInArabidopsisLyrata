
library('rmarkdown')
library("knitr")

#' --- 
#' title: "R-scripts, statistical analyses, results and plots."
#' subtitle: "Results from auxin inhibition study."
#' author: "Bishwa K. Giri"
#' date: "Sept 15th, 2018"
#' output: pdf_document
#' --- 


### Set the required path 
getwd()
# ** Change path if need be. 
setwd("/home/priyanka/Dropbox/DataAnalyses/LyrataPhotoDataAnalyses")
getwd()
list.files()  # read available files and folders 

##### Read the required data  #####

# Read data from September 18th, 2018
DataFromSep <- readxl::read_xlsx("TraitsData/09_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromSep)

DataFromDec <- readxl::read_xlsx("TraitsData/12_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromDec)

DataFromMar <- readxl::read_xlsx("TraitsData/03_28_2018_Data.xlsx", col_names = TRUE)
head(DataFromMar)


######    Create unique names for each columns of each dataframe   ######
## add suffix to each dataframe "header"; keep names like "plant ID", "treatment" and "population" as is
colnames(DataFromSep) <- paste(colnames(DataFromSep), "Sep", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromSep)[colnames(DataFromSep)=="Plant_ID.Sep"] <- "Plant_ID"
colnames(DataFromSep)[colnames(DataFromSep)=="Treatment.Sep"] <- "Treatment"
colnames(DataFromSep)[colnames(DataFromSep)=="Population.Sep"] <- "Population"
colnames(DataFromSep)[colnames(DataFromSep)=="Family.Sep"] <- "Family"
colnames(DataFromSep)[colnames(DataFromSep)=="TreatmentLevel.Sep"] <- "TreatmentLevel"

colnames(DataFromDec) <- paste(colnames(DataFromDec), "Dec", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromDec)[colnames(DataFromDec)=="Plant_ID.Dec"] <- "Plant_ID"
colnames(DataFromDec)[colnames(DataFromDec)=="Treatment.Dec"] <- "Treatment"
colnames(DataFromDec)[colnames(DataFromDec)=="Population.Dec"] <- "Population"
colnames(DataFromDec)[colnames(DataFromDec)=="Family.Dec"] <- "Family"
colnames(DataFromDec)[colnames(DataFromDec)=="TreatmentLevel.Dec"] <- "TreatmentLevel"

colnames(DataFromMar) <- paste(colnames(DataFromMar), "Mar", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromMar)[colnames(DataFromMar)=="Plant_ID.Mar"] <- "Plant_ID"
colnames(DataFromMar)[colnames(DataFromMar)=="Treatment.Mar"] <- "Treatment"
colnames(DataFromMar)[colnames(DataFromMar)=="Population.Mar"] <- "Population"
colnames(DataFromMar)[colnames(DataFromMar)=="Family.Mar"] <- "Family"
colnames(DataFromMar)[colnames(DataFromMar)=="TreatmentLevel.Mar"] <- "TreatmentLevel"

head(DataFromSep)

## **use if need be: this renaming can also be done with library(data.table)
#library(data.table)
#data.table::setnames(DataFromDec, old = c("Plant_ID.Dec", "Treatment.Dec"), 
         #new = c("Plant_ID", "Treatment"))


####### Merge dataframes  ########### 

## ** Use if need be: We can merge only two dataframes one at a time
#merged.Sept.Dec <- merge(DataFromSept, DataFromDec, 
                             #by=c("Plant_ID", "Treatment", "Population"), all = TRUE)
#head(merged.Sept.Dec)

## Or we can merge multiple dataframes using a "Reduce - Merge" function.
merged.Sep.Dec.Mar = Reduce(function(x, y) 
  merge(x, y, by=c("Plant_ID", "Population", "Family", "TreatmentLevel", "Treatment"), 
        all = TRUE), list(DataFromSep, DataFromDec, DataFromMar))
head(merged.Sep.Dec.Mar)

## convert the "NA" strings as empty NA values
merged.Sep.Dec.Mar[merged.Sep.Dec.Mar == "NA"] <- NA


##### remove not-required column names by matching "header names".  #####
# such as "Date", "Remarks", etc.

# find the columns to drop (i.e they are not required any more in this data analyses)
# this code find the column by matching the pattern
dropcols = unique(grep(paste(c("Date", "Remarks"), collapse="|"), 
                       colnames(merged.Sep.Dec.Mar), value=TRUE))
dropcols

# drop the columns
merged.Sep.Dec.Mar = merged.Sep.Dec.Mar[, !(names(merged.Sep.Dec.Mar) %in% dropcols)]
head(merged.Sep.Dec.Mar)

# **use if need be : use subset to select specific columns 
#merged.Sept.Dec.Mar.selected = subset(merged.Sept.Dec.Mar, select = c(dropcols))


##### Assign proper datatype to each column ######

## find the column names for which we want to convert the data types
ChangeToDouble = unique(grep(paste(c("Diameter", "Lat_Shoot", "Inflores", 
                                     "Leaf_Shape", "Trichomes", "Bolting", "Flower"), collapse="|"), 
                       colnames(merged.Sep.Dec.Mar), value=TRUE))

ChangeToFactor = unique(grep(paste(c("Treatment", "Population"), collapse="|"), 
                             colnames(merged.Sep.Dec.Mar), value=TRUE))

## Change datatype of the column
# change all other data to "double" type
merged.Sep.Dec.Mar[ChangeToDouble] <- lapply(merged.Sep.Dec.Mar[ChangeToDouble], 
                                             as.double, na.strings = "NA")
## assign "treatments" and "population" as factors
#merged.Sep.Dec.Mar$Treatment = as.factor(merged.Sep.Dec.Mar$Treatment)
merged.Sep.Dec.Mar[ChangeToFactor] <- lapply(merged.Sep.Dec.Mar[ChangeToFactor], 
                                            as.factor)

### ?? Question ?? - Is keeping datatypes of "Trichome Intensity", "Lateral shoot rating" etc 
  ## .. as "double" appropriate ??


##### Get some summary statistics  #######

## Overall summary 
summary(merged.Sep.Dec.Mar)

## Summary on diameter on each time points 
summary(merged.Sep.Dec.Mar$`Diameter(inches).Sep`, na.rm = TRUE)
summary(merged.Sep.Dec.Mar$`Diameter(inches).Dec`, na.rm = TRUE)
summary(merged.Sep.Dec.Mar$`Diameter(inches).Mar`, na.rm = TRUE)

## Get mean/summary by "treatment" category for each time points. 

SepDiaSummaryByTreatment = with(
  merged.Sep.Dec.Mar, base::
    tapply(merged.Sep.Dec.Mar$`Diameter(inches).Sep`, 
           merged.Sep.Dec.Mar$Treatment, summary, na.rm=TRUE))
SepDiaSummaryByTreatment

DecDiaSummaryByTreatment = with(
  merged.Sep.Dec.Mar, base::
    tapply(merged.Sep.Dec.Mar$`Diameter(inches).Dec`, 
           merged.Sep.Dec.Mar$Treatment, summary, na.rm=TRUE))
DecDiaSummaryByTreatment

MarDiaSummaryByTreatment = with(
  merged.Sep.Dec.Mar, base::
    tapply(merged.Sep.Dec.Mar$`Diameter(inches).Mar`, 
           merged.Sep.Dec.Mar$Treatment, summary, na.rm=TRUE))
MarDiaSummaryByTreatment


#### Start plotting the data #####

## install and import "ggplot2" library
# install.packages("ggplot2")  # install if required
require("ggplot2")
require(dplyr)
require(tidyr)
colnames(merged.Sep.Dec.Mar)

## Plot - Diameter Changes across time (by Treatment)
##### Some data preparation before plotting ########### 
### Let's see how diameter changes across "Treatment" for each time points

# first find all the available "Diameter" data columns 
diameterColNames = unique(grep(paste(c("Diameter"), collapse="|"), 
                       colnames(merged.Sep.Dec.Mar), value=TRUE))
diameterColNames

# Select colums with data for "Diameter" (for each time) and "Treatment"
diameterAndTreatmentData = subset(
  merged.Sep.Dec.Mar, select = c(diameterColNames, "Treatment"))
head(diameterAndTreatmentData)


## now, reshape the data: here, gather is used to store "header" values as "Month" ... 
  # .. and the values within each month are stored as "Diameter". 
gatheredDiameterAndTreatmentData = 
  diameterAndTreatmentData %>%  gather(Month, Diameter, -Treatment) 
head(gatheredDiameterAndTreatmentData)

### ** if need be ** to write data to files 
#write.table(gatheredDiameterAndTreatmentData, file = "DiameterAndTreatmentData.txt", 
 #           sep = "\t", na = "NA", row.names = FALSE, col.names = TRUE)

# Again update the datatype of the column. The datatype of the columns are reverting to "character" .. 
  # after subsetting and/or gathering. So, updating the datatypes again.

# find columns that need their datatypes updated
colnames(gatheredDiameterAndTreatmentData)
typeof(gatheredDiameterAndTreatmentData$Diameter)

gatheredDiameterAndTreatmentData[c("Diameter")] <- 
  as.double(gatheredDiameterAndTreatmentData$Diameter)

# Calculate "sub-mean" value (by "Treatment", by "Month")
subMeans = aggregate(
  as.double(unlist(gatheredDiameterAndTreatmentData$Diameter)),
  by=list(Treatment = gatheredDiameterAndTreatmentData$Treatment, 
          Month = gatheredDiameterAndTreatmentData$Month), FUN = mean, na.rm=TRUE)  

# the above "aggregate" method puts new values as "x". Change it to "Diameter"
colnames(subMeans)[colnames(subMeans)=="x"] <- "Diameter"
subMeans


## Similarly compute "standard error"
# Calculate "sub-mean" value (by "Treatment", by "Month")
subSE = aggregate(
  as.double(unlist(gatheredDiameterAndTreatmentData$Diameter)),
  by=list(Treatment = gatheredDiameterAndTreatmentData$Treatment, 
          Month = gatheredDiameterAndTreatmentData$Month), FUN = sd , na.rm=TRUE)  

# Change column name "x" to "SE"
colnames(subSE)[colnames(subSE)=="x"] <- "SE"
subSE

## now, bind the "subMeans" with "subSE" 
subMeans = Reduce(function(x, y) 
  merge(x, y, by=c("Treatment", "Month"), 
        all = TRUE), list(subMeans, subSE))

##### Now, make some plots ###########


### Plot data for each "Treatment" group by "Month"
gatheredDiameterAndTreatmentData  %>%
  subset(Diameter != "NA") %>%
  ggplot(aes(x = factor(Month), y = Diameter)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeans, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) 

## The above plot can be customized and improved to : 
  # 1) arrange the plot by factors (i.e Month, with "Sept" at the beginning) 
  # 2) add custom color code to the treatment 
gatheredDiameterAndTreatmentData  %>%
  subset(Diameter != "NA") %>%
  ggplot(aes(x = factor(Month), y = Diameter)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeans, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) +
  
  theme_bw() + # remove background 
  
  # add custome color to the "Treatment" levels 
  scale_colour_manual( 
    values = c("Aux_Drop" = "Purple", "Aux_Spray" = "Red", 
               "DMSO" = "Orange", "Water" = "Green")) + 
  
  # rearrange the x-axis
  scale_x_discrete(limits=c(
      "Diameter(inches).Sep", "Diameter(inches).Dec", 
      "Diameter(inches).Mar")) + 
  
  # add an error bar for each "sub-means"
  geom_errorbar(data = subMeans, aes(
    colour = Treatment, ymin=Diameter-SE, ymax=Diameter+SE),
    position = position_dodge(width = 0.2), width=.1, size = 0.5) + 
  
  # to connect the "subMeans - Diameter" values across time points
  geom_line(data = subMeans, aes(
    x = Month, y = Diameter, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) 


### Now, make similar plots for "lateral shoot rating" 
## Plot - Changes in "lateral shoot development" across time (by Treatment)

##### Some data preparation before plotting ########### 
### Let's see how diameter changes across "Treatment" for each time points
laterShootColNames = unique(grep(paste(c("Lat_Shoot"), collapse="|"), 
                               colnames(merged.Sep.Dec.Mar), value=TRUE))
laterShootColNames

# for that we make a new dataframe with diameter (by each time) and treatment
lateralShootAndTreatmentData = subset(
  merged.Sep.Dec.Mar, select = c(laterShootColNames, "Treatment"))
head(lateralShootAndTreatmentData)

## now, reshape the data
gatheredLateralShootAndTreatmentData = 
  lateralShootAndTreatmentData %>%  gather(Month, LateralShootR, -Treatment) 
head(gatheredLateralShootAndTreatmentData)

# Update the datatypes of columns named "LateralShootR"
gatheredLateralShootAndTreatmentData[c("LateralShootR")] <- 
  as.double(gatheredLateralShootAndTreatmentData$LateralShootR)

# Calculate "sub-mean" value (by "Treatment", by "Month")
subMeansLSR = aggregate(gatheredLateralShootAndTreatmentData$LateralShootR,
  by=list(Treatment = gatheredLateralShootAndTreatmentData$Treatment, 
          Month = gatheredLateralShootAndTreatmentData$Month), FUN = mean, na.rm=TRUE)  

# the above "aggregate" method puts new values as "x". Change it to "Diameter"
colnames(subMeansLSR)[colnames(subMeansLSR)=="x"] <- "LateralShootR"
subMeansLSR


## Similarly compute "standard error"
# Calculate "sub-mean" value (by "Treatment", by "Month")
subSErrLSR = aggregate(
  as.double(unlist(gatheredLateralShootAndTreatmentData$LateralShootR)),
  by=list(Treatment = gatheredLateralShootAndTreatmentData$Treatment, 
          Month = gatheredLateralShootAndTreatmentData$Month), FUN = sd , na.rm=TRUE)  

# Change column name "x" to "SE"
colnames(subSErrLSR)[colnames(subSErrLSR)=="x"] <- "SE"
colnames(subSErrLSR)

## now, bind the "subMeans" with "subSE" 
subMeansLSR = Reduce(function(x, y) 
  merge(x, y, by=c("Treatment", "Month"), 
        all = TRUE), list(subMeansLSR, subSErrLSR))

##### Now, make plots ###########

## Plot "Diameter" for each "Treatment" by "Month" 
gatheredLateralShootAndTreatmentData  %>%
  subset(LateralShootR != "NA") %>%
  ggplot(aes(x = factor(Month), y = LateralShootR)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeansLSR, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) + 
  geom_line(data = subMeansLSR, aes(
    x = Month, y = LateralShootR, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) 

## The above plot can be customized and improved to : 
# 1) arrange the plot by factors (i.e Month, with "Sept" at the beginning) 
# 2) add custom color code to the treatment 
gatheredLateralShootAndTreatmentData  %>%
  subset(LateralShootR != "NA") %>%
  ggplot(aes(x = factor(Month), y = LateralShootR)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeansLSR, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) +
  
  theme_bw() + # remove background 
  
  # add custome color to the "Treatment" levels 
  scale_colour_manual( 
    values = c("Aux_Drop" = "Purple", "Aux_Spray" = "Red", 
               "DMSO" = "Orange", "Water" = "Green")) + 
  
  # rearrange the x-axis
  scale_x_discrete(limits=c(
    "Lat_Shoot_R.Sep", "Lat_Shoot_R.Dec", 
    "Lat_Shoot_R.Mar")) + 
  
  # add an error bar for each "sub-means"
  geom_errorbar(data = subMeansLSR, aes(
    colour = Treatment, ymin=LateralShootR-SE, ymax=LateralShootR+SE),
    position = position_dodge(width = 0.2), width=.1, size = 0.5) + 
  
  # to connect the "subMeans - Diameter" values across time points
  geom_line(data = subMeansLSR, aes(
    x = Month, y = LateralShootR, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) 


##### Statistical test of significance #######

## run "lm" model 

## 01) for changes in diameter 




## run "glm" model 



## Run ANOVA 



##### *********** To do ********
## Add auxin vs. control column.
## Add "family" level column






################   End   ##########################

