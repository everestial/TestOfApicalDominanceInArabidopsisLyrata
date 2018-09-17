
#' --- 
#' title: "R-scripts, statistical analyses, results and plots."
#' subtitle: "Results from auxin inhibition study."
#' author: "Bishwa K. Giri"
#' date: "Sept 15th, 2018"
#' output:
#'    html_document:
#'      toc: true
#' --- 


---
#' ### Step 01: Set the required path (only if requred)
---
  
# find the working directory
getwd()
# ** Change path if need be. 
setwd("/home/priyanka/Dropbox/DataAnalyses/LyrataPhotoDataAnalyses")
getwd()
list.files()  # read available files and folders 

--- 
#' \  
---

---
#' ### Step 02: Read the input data
#' **The input files are provided in folder named "TraitsData" ** \
#' **in the link :** https://github.com/everestial/AuxinInhibitionResultsDataAnalyses \
#' \
#' In our research the data was collected biweekly by taking pictures vertically. \
#' The photos of the plant included an inch scale on the side along with it's ID labelled with it's "Treatment". \
#' ![A typical picture of mayodan plant taken during the research.](/home/priyanka/Dropbox/DataAnalyses/LyrataPhotoDataAnalyses/M-14-8-J_9-18-2017.jpg)
---

  
# Now, read the data from month (from Sep 2017 to March 2018).
DataFromSep <- readxl::read_xlsx("TraitsData/09_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromSep)

DataFromDec <- readxl::read_xlsx("TraitsData/12_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromDec)

DataFromMar <- readxl::read_xlsx("TraitsData/03_28_2018_Data.xlsx", col_names = TRUE)
head(DataFromMar)

--- 
#' \  
---

--- 
#' #### Step 02 - A: Rename/Adjust column names
#' Here, we will create unique names of each columns for each dataframe\
#' But, some column names will be kept common which will help in merging the data\
#' **Process :**\
#'   * add unique (month name) suffix to each dataframe "header"\
#'   * reset the names like "plant ID", "treatment" and "population" as they are

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

--- 
#' \  
---

--- 
#' ### Step 03: Merge dataframes
---

## ** Use if need be: We can merge only two dataframes one at a time
#merged.Sept.Dec <- merge(DataFromSept, DataFromDec, 
                             #by=c("Plant_ID", "Treatment", "Population"), all = TRUE)
--- 
#' **Or we can merge multiple dataframes using a "Reduce - Merge" function.**
---
merged.Sep.Dec.Mar = Reduce(function(x, y) 
  merge(x, y, by=c("Plant_ID", "Population", "Family", "TreatmentLevel", "Treatment"), 
        all = TRUE), list(DataFromSep, DataFromDec, DataFromMar))
head(merged.Sep.Dec.Mar)

## convert the "NA" strings as empty NA values
merged.Sep.Dec.Mar[merged.Sep.Dec.Mar == "NA"] <- NA

--- 
#' \
#' **Remove not-required column names by matching "header names".**\
#' **such as "Date", "Remarks", etc.**\
#' This helps to keep the memory low.
---
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

--- 
#' \  
---

---
#' ### Step 04: Assign proper datatype to each column
#' **It is important to assign proper datatype to each column**\
#' **Reading from excel file store most of the datatype as "character"**\
#' **We will convert most datatypes to "double" and columns like "Population", "family", ** 
#' **"TreatmentLevel", "Treatment" to "factor type".**
---

## find the column names for which we want to convert the data types
ChangeToDouble = unique(grep(paste(c("Diameter", "Lat_Shoot", "Inflores", 
                                     "Leaf_Shape", "Trichomes", "Bolting", "Flower"), collapse="|"), 
                       colnames(merged.Sep.Dec.Mar), value=TRUE))
# see which columns will be changed to "double" type
ChangeToDouble

ChangeToFactor = unique(grep(paste(c( "Family", "Population", "Treatment"), collapse="|"), 
                             colnames(merged.Sep.Dec.Mar), value=TRUE))
# see which columns will be changed to "factor" type
ChangeToFactor

## Change datatype of the columns 
# change required columns to "double" type
merged.Sep.Dec.Mar[ChangeToDouble] <- lapply(merged.Sep.Dec.Mar[ChangeToDouble], 
                                             as.double, na.strings = "NA")

# change other required columns to "factor" type 
#merged.Sep.Dec.Mar$Treatment = as.factor(merged.Sep.Dec.Mar$Treatment)
merged.Sep.Dec.Mar[ChangeToFactor] <- lapply(merged.Sep.Dec.Mar[ChangeToFactor], 
                                            as.factor)

### ?? Question ?? - Is keeping datatypes of "Trichome Intensity", "Lateral shoot rating" etc 
  ## .. as "double" appropriate ??

--- 
#' \  
---

--- 
#' ### Step 05: Get some summary statistics 
---

## Overall summary 
summary(merged.Sep.Dec.Mar)

## Summary on diameter from each time points 
summary(merged.Sep.Dec.Mar$`Diameter(inches).Sep`, na.rm = TRUE)
summary(merged.Sep.Dec.Mar$`Diameter(inches).Dec`, na.rm = TRUE)
summary(merged.Sep.Dec.Mar$`Diameter(inches).Mar`, na.rm = TRUE)

--- 
#' \  
---

## Get summary (on mean) by "treatment" for each time points. 
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

--- 
#' \  
---

--- 
#' ### Step 06: Start plotting the data 
---

## install and import "ggplot2" library
# install.packages("ggplot2")  # install if required
require("ggplot2")
require(dplyr)
require(tidyr)

--- 
#' \  
---
--- 
#' #### 06 - A) Plot variation in diameter (by Treatment) across several time periods
#' ##### 06 - A (i) : Run some data preparation before plotting 
---
# first find all the available "Diameter" data columns 
diameterColNames = unique(grep(paste(c("Diameter"), collapse="|"), 
                       colnames(merged.Sep.Dec.Mar), value=TRUE))
diameterColNames

## Subset the data that contain "Diameter" (for each time) and "Treatment"
diameterAndTreatmentData = subset(
  merged.Sep.Dec.Mar, select = c(diameterColNames, "Treatment"))
head(diameterAndTreatmentData)


## now, reshape the data: 
  # use "gather" to store "header" values as "Month"
  # and the diameter values within each month as "Diameter". 
gatheredDiameterAndTreatmentData = 
  diameterAndTreatmentData %>%  gather(Month, Diameter, -Treatment) 
head(gatheredDiameterAndTreatmentData)

## ** if need be ** : to write data to files 
#write.table(gatheredDiameterAndTreatmentData, file = "DiameterAndTreatmentData.txt", 
 #           sep = "\t", na = "NA", row.names = FALSE, col.names = TRUE)

--- 
#' \
#' **Again update the datatype of the column.**\
#' After subsetting and/or gathering the datatype of the columns revert to "character".
---
colnames(gatheredDiameterAndTreatmentData)
typeof(gatheredDiameterAndTreatmentData$Diameter)

--- 
#' \
#' **So, it is important to update the datatypes again.**
---
gatheredDiameterAndTreatmentData[c("Diameter")] <- 
  as.double(gatheredDiameterAndTreatmentData$Diameter)

gatheredDiameterAndTreatmentData[c("Month")] <- 
  as.factor(gatheredDiameterAndTreatmentData$Month)

gatheredDiameterAndTreatmentData[c("Treatment")] <- 
  as.factor(gatheredDiameterAndTreatmentData$Treatment)

# now, check the type of the data 
typeof(gatheredDiameterAndTreatmentData$Diameter)

--- 
#' \  
---

--- 
#' ##### 06 - A (ii) : Calculate "sub-mean" (for each "Treatment", by "Month")
#' **Since, our overall interest is to check how different "Treatment" affect "Diameter"**\
#' **We will compute how the mean "Diameter" varies by "Treatment" for each month**
---
# aggregate the submeans for each treatment (by month)
subMeansDiameter = aggregate(
  as.double(unlist(gatheredDiameterAndTreatmentData$Diameter)),
  by=list(Treatment = gatheredDiameterAndTreatmentData$Treatment, 
          Month = gatheredDiameterAndTreatmentData$Month), FUN = mean, na.rm=TRUE)  

# the above "aggregate" method puts new values as "x". Change it to "Diameter"
colnames(subMeansDiameter)[colnames(subMeansDiameter)=="x"] <- "Diameter"
subMeansDiameter

--- 
#' \
#' **Similarly compute "standard error" of the mean**
---
# Calculate "sub-mean" value (by "Treatment", by "Month")
subSEdiameter = aggregate(
  as.double(unlist(gatheredDiameterAndTreatmentData$Diameter)),
  by=list(Treatment = gatheredDiameterAndTreatmentData$Treatment, 
          Month = gatheredDiameterAndTreatmentData$Month), FUN = sd , na.rm=TRUE)  

# Change column name "x" to "SE"
colnames(subSEdiameter)[colnames(subSEdiameter)=="x"] <- "SEdiam"
subSEdiameter

## now, bind the "subMeansDiameter" with "subSEdiameter" 
subMeansDiameter = Reduce(function(x, y) 
  merge(x, y, by=c("Treatment", "Month"), 
        all = TRUE), list(subMeansDiameter, subSEdiameter))

head(subMeansDiameter)

--- 
#' \  
---

--- 
#' #### 06 - A (iii) : Plot variation in "Diameter" by "Treatment" for each time points
---

--- 
#' \
#' **Use the gathered data for each "Treatment" group by "Month"to plot**
---
gatheredDiameterAndTreatmentData  %>%
  subset(Diameter != "NA") %>%
  ggplot(aes(x = factor(Month), y = Diameter)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeansDiameter, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) 

--- 
#' \
#' **The above plot can be customized and improved by : **\
#'   1) arranging the plot by factors (i.e increasing order of Month ("Sept" 2017 at the beginning)\
#'   2) and by adding custom color code to represent each treatment 
---
gatheredDiameterAndTreatmentData  %>%
  subset(Diameter != "NA") %>%
  ggplot(aes(x = factor(Month), y = Diameter)) + 
  geom_point(aes(colour = Treatment), na.rm = TRUE, 
             position = position_dodge(width = 0.2)) +
  geom_point(data = subMeansDiameter, size = 4, aes(colour = Treatment), 
             na.rm = TRUE, position = position_dodge(width = 0.2)) +
  
  theme_bw() + # remove background 
  
  # add custom color to the "Treatment" levels 
  scale_colour_manual( 
    values = c("Aux_Drop" = "Purple", "Aux_Spray" = "Red", 
               "DMSO" = "Orange", "Water" = "Green")) + 
  
  # rearrange the x-axis
  scale_x_discrete(limits=c(
      "Diameter(inches).Sep", "Diameter(inches).Dec", 
      "Diameter(inches).Mar")) + 
  
  # add an error bar for each "sub-means"
  geom_errorbar(data = subMeansDiameter, aes(
    colour = Treatment, ymin=Diameter-SEdiam, ymax=Diameter+SEdiam),
    position = position_dodge(width = 0.2), width=.1, size = 0.5) + 
  
  # to connect the "subMeans - Diameter" values across time points
  geom_line(data = subMeansDiameter, aes(
    x = Month, y = Diameter, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) 

  # ** to do : add 
    # number of samples in each "Treatment, by Months" - as ticks on x-axis
    # type "mean" and "SE" values, after that sample size value 

  # ** to do : Make plot by each "TreatmentLevel" for each time period (month) 

--- 
#' \
---

--- 
#' #### 06 - B) Now, make similar plots for "lateral shoot rating"
#' **Plot - Changes in "lateral shoot development" across time (by Treatment)**
#' 
#' ##### 06 - B (i) : Run some data preparation before plotting 
#' **Make a new dataframe containing data only on (lateral shoot rating) by "Treatment" for each time points**
---

laterShootColNames = unique(grep(paste(c("Lat_Shoot"), collapse="|"), 
                               colnames(merged.Sep.Dec.Mar), value=TRUE))
laterShootColNames

# for that we make a new dataframe with diameter (by each time) and treatment
lateralShootAndTreatmentData = subset(
  merged.Sep.Dec.Mar, select = c(laterShootColNames, "Treatment"))
head(lateralShootAndTreatmentData)

--- 
#' \
---

## now, reshape the data
gatheredLateralShootAndTreatmentData = 
  lateralShootAndTreatmentData %>%  gather(Month, LateralShootR, -Treatment) 
head(gatheredLateralShootAndTreatmentData)

# Update the datatypes of columns named "LateralShootR"
gatheredLateralShootAndTreatmentData[c("LateralShootR")] <- 
  as.double(gatheredLateralShootAndTreatmentData$LateralShootR)

--- 
#' \
---
# Calculate "sub-mean" (by "Treatment", for each "Month")
subMeansLSR = aggregate(gatheredLateralShootAndTreatmentData$LateralShootR,
  by=list(Treatment = gatheredLateralShootAndTreatmentData$Treatment, 
          Month = gatheredLateralShootAndTreatmentData$Month), FUN = mean, na.rm=TRUE) 

head(subMeansLSR)

# the above "aggregate" method puts new values as "x". Change it to "LateralShootR"
colnames(subMeansLSR)[colnames(subMeansLSR)=="x"] <- "LateralShootR"
subMeansLSR

--- 
#' \
---
## Similarly compute "standard error"
# Calculate "sub-mean" value (by "Treatment", by "Month")
subSErrLSR = aggregate(
  as.double(unlist(gatheredLateralShootAndTreatmentData$LateralShootR)),
  by=list(Treatment = gatheredLateralShootAndTreatmentData$Treatment, 
          Month = gatheredLateralShootAndTreatmentData$Month), FUN = sd , na.rm=TRUE) 

# Change column name "x" to "SE"
colnames(subSErrLSR)[colnames(subSErrLSR)=="x"] <- "SElsr"
head(subSErrLSR)

## now, bind the "subMeansLSR" with "subSErrLSR" 
subMeansLSR = Reduce(function(x, y) 
  merge(x, y, by=c("Treatment", "Month"), 
        all = TRUE), list(subMeansLSR, subSErrLSR))

head(subMeansLSR)

--- 
#' \
---
  
---
#' ##### 06 - B (ii) : Plot variation in "lateral shoot rating" by "Treatment" for each time points
---
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

--- 
#' \
#' **The above plot can be customized and improved to :**\
#'   1) arrange the plot by factors (i.e Month, with "Sept" at the beginning)\
#'   2) add custom color code to the treatment
---

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
    colour = Treatment, ymin=LateralShootR-SElsr, ymax=LateralShootR+SElsr),
    position = position_dodge(width = 0.2), width=.1, size = 0.5) + 
  
  # to connect the "subMeans - Diameter" values across time points
  geom_line(data = subMeansLSR, aes(
    x = Month, y = LateralShootR, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) 


---
#' ### Step 07 : Statistical tests 
---

# ... continuing  ....
  
--- 
#' #### Step 07 - A : Linear regression model\
#' **Some preliminary tests**
---

# modeling the effects of treatment 
####
print('hello')
colnames(merged.Sep.Dec.Mar)

# relevel factors to include "Control" as the reference level
merged.Sep.Dec.Mar = within(
  merged.Sep.Dec.Mar, 
  TreatmentLevel <- relevel(TreatmentLevel, ref = "Control"))

merged.Sep.Dec.Mar = within(
  merged.Sep.Dec.Mar, 
  Treatment <- relevel(Treatment, ref = "Water"))

plantDiamSep <- lm(`Diameter(inches).Sep` ~ TreatmentLevel, merged.Sep.Dec.Mar)
summary(plantDiamSep)

plantDiamSep01 <- lm(`Diameter(inches).Sep` ~ Treatment, merged.Sep.Dec.Mar)
summary(plantDiamSep01)

## 07 - A (i): 
# Testing if there was family level effects on diameter before any treatment started.
plantDiamSepFam <- lm(`Diameter(inches).Sep` ~ TreatmentLevel/Family, merged.Sep.Dec.Mar)
summary(plantDiamSepFam)

plantDiamSepFam01 <- lm(`Diameter(inches).Sep` ~ Treatment/Family, merged.Sep.Dec.Mar)
summary(plantDiamSepFam01)

## Result: We can see that there was no significant differences in diameter before the 
   # treatment was started for any "TreatmentLevel : Control vs. Auxin".
## Result 01: We can see that there was no significant differences in diameter before the 
   # treatment was started for any "Treatment : Control vs. Auxin". "Treatment : Water vs. DMSO vs. Auxin Spray vs. Auxin droplet".

## Result (with Family as covariate): 
  # Family 20 and 21 had some discrepancies in initial diameter before the treatment started. 


## 07 - A (ii):
## Now, check if diameter were different in the month of december 
plantDiamDec <- lm(`Diameter(inches).Dec` ~ TreatmentLevel, merged.Sep.Dec.Mar)
summary(plantDiamDec)

plantDiamDecFam <- lm(`Diameter(inches).Dec` ~ TreatmentLevel/Family,  merged.Sep.Dec.Mar)
summary(plantDiamDecFam)

# run ANOVA to check for "Family" based effects. 
AnovatreatmentEffDecbyFam <- anova(plantDiamDec, plantDiamDecFam)
AnovatreatmentEffDecbyFam

## Result: Treatment level had very significant effect on diameter changes. p-value: 3.982e-06
  # Treatment affect diameter changes negatively (i.e they reduced the diameter)
  # But, the family based effects were almost non existent on the effects of the treatments. 
    # p-value: 0.2591


## 07 - B (i): Test for the effect of Treatment on lateral shoot rating. 
lateralShootRatDec <- glm(Lat_Shoot_R.Dec ~ TreatmentLevel, merged.Sep.Dec.Mar, family = gaussian())
summary(lateralShootRatDec)

lateralShootRatDecFam <- glm(Lat_Shoot_R.Dec ~ TreatmentLevel/Family, merged.Sep.Dec.Mar, family = gaussian())
summary(lateralShootRatDecFam)

anova(lateralShootRatDecFam, lateralShootRatDec)

## Result: There seems to be no effect of inhibition treatments on lateral shoot rating (in December). 

## 07 - B (i): Test for the effect of Treatment on lateral shoot rating. 
lateralShootRatMar <- glm(Lat_Shoot_R.Mar ~ TreatmentLevel, merged.Sep.Dec.Mar, family = gaussian())
summary(lateralShootRatMar)

lateralShootRatMarFam <- glm(Lat_Shoot_R.Mar ~ TreatmentLevel/Family, merged.Sep.Dec.Mar, family = gaussian())
summary(lateralShootRatMarFam)

anova(lateralShootRatMarFam, lateralShootRatMar)

## Result: But, auxin treatment really had some effect on lateral shoot rating on March. p-value: 0.0513 
  # This must be due to the fact that lateral shoot once emerged don't disappear. The observed effects of 
  # .. treatment should have been cumulative over the development period of lyrata. 





#... continue .... 
# Still need to do: 
#  - add diameter as mm. 
#  - take diameter in Sept as base and then add diameter changes. 
#  - set "control" as the base level 


####
#plantPostDiam <- lm(RosetteDiam ~ Pop, data=MorphPlantData)
#summary(plantPostDiam)
#plantPostDiam1 <- lm(RosetteDiam ~ Pop/Family, data=MorphPlantData)
#summary(plantPostDiam1)
#anova(plantPostDiam1, plantPostDiam)

## 01) for changes in diameter 




## run "glm" model 



## Run ANOVA 



##### *********** To do ********
## Add auxin vs. control column.
## Add "family" level column



################   End   ##########################

