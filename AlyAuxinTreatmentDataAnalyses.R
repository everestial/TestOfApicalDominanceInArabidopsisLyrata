
### Set the required path 
getwd()
setwd("/home/priyanka/Dropbox/DataAnalyses/LyrataPhotoDataAnalyses")
getwd()
list.files()  # read available files and folders 

##### Read the required data  #####

# Read data from September 18th, 2018
DataFromSep <- readxl::read_xlsx("lyrataphotodata/09_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromSep)

DataFromDec <- readxl::read_xlsx("lyrataphotodata/12_18_2017_Data.xlsx", col_names = TRUE)
head(DataFromDec)

DataFromMar <- readxl::read_xlsx("lyrataphotodata/03_28_2018_Data.xlsx", col_names = TRUE)
head(DataFromMar)


######    Create unique names for each columns of each dataframe   ######
## add suffix to each dataframe "header"; keep names like "plant ID", "treatment" and "population" as is
colnames(DataFromSep) <- paste(colnames(DataFromSep), "Sep", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromSep)[colnames(DataFromSep)=="Plant_ID.Sep"] <- "Plant_ID"
colnames(DataFromSep)[colnames(DataFromSep)=="Treatment.Sep"] <- "Treatment"
colnames(DataFromSep)[colnames(DataFromSep)=="Population.Sep"] <- "Population"

colnames(DataFromDec) <- paste(colnames(DataFromDec), "Dec", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromDec)[colnames(DataFromDec)=="Plant_ID.Dec"] <- "Plant_ID"
colnames(DataFromDec)[colnames(DataFromDec)=="Treatment.Dec"] <- "Treatment"
colnames(DataFromDec)[colnames(DataFromDec)=="Population.Dec"] <- "Population"

colnames(DataFromMar) <- paste(colnames(DataFromMar), "Mar", sep = ".")
  # set the names of some columns back to original 
colnames(DataFromMar)[colnames(DataFromMar)=="Plant_ID.Mar"] <- "Plant_ID"
colnames(DataFromMar)[colnames(DataFromMar)=="Treatment.Mar"] <- "Treatment"
colnames(DataFromMar)[colnames(DataFromMar)=="Population.Mar"] <- "Population"

head(DataFromSep)


####### Merge dataframes  ########### 

## ** Use if need be: We can merge two dataframes at a time
#merged.Sept.Dec <- merge(DataFromSept, DataFromDec, 
                             #by=c("Plant_ID", "Treatment", "Population"), all = TRUE)
#head(merged.Sept.Dec)

## Or we can merge multiple dataframes using a "Reduce - Merge" function.
merged.Sep.Dec.Mar = Reduce(function(x, y) 
  merge(x, y, by=c("Plant_ID", "Treatment", "Population"), 
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


#####  Add on codes #####
setwd("/home/priyanka/Dropbox/DataAnalyses/LyrataPhotoDataAnalyses/PreviousPhenotypeDataAnalysesResearch")
getwd()

# load plant data file:
MorphPlantData <- read.csv("AlyMorphPostPlant1113.csv", header=TRUE)

# load inflorescence data file:
MorphInflData <- read.csv("AlyMorphPostInfl1113.csv", header=TRUE)

#### This file is missing from Dryad *** ### 
# load full preliminary plant data file:
MorphPreData <- read.csv("AlyMorphPrePlant1113.csv", header=TRUE)

# load REVISED preliminary plant data file (with censoring):
MorphPreData2 <- read.csv("AlyMorphPrePlant1014.csv", header=TRUE)


head(MorphPlantData)

# calculate reproductive season rosette diameter change:
MorphPlantData$dDiam <- MorphPlantData$RosetteDiam-MorphPlantData$PreDiam
MorphPlantData$dDiam

# preliminary tests using lm:
plantPostDiam <- lm(RosetteDiam ~ Pop, data=MorphPlantData)
summary(plantPostDiam)
plantPostDiam1 <- lm(RosetteDiam ~ Pop/Family, data=MorphPlantData)
summary(plantPostDiam1)
anova(plantPostDiam1, plantPostDiam)

plantInfl <- lm(Infl ~ Pop, data=MorphPlantData)
summary(plantInfl)
plantInfl1 <- lm(Infl ~ Pop/Family, data=MorphPlantData)
summary(plantInfl1)
anova(plantInfl1, plantInfl)

plantPreDiam <- lm(PreDiam ~ Pop, data=MorphPlantData)
summary(plantPreDiam)
plantPreDiam1 <- lm(PreDiam ~ Pop/Family, data=MorphPlantData)
summary(plantPreDiam1)
anova(plantPreDiam1, plantPreDiam)

plantDiamChange <- lm(dDiam ~ Pop, data=MorphPlantData)
summary(plantDiamChange)
plantDiamChange1 <- lm(dDiam ~ Pop/Family, data=MorphPlantData)
summary(plantDiamChange1)
anova(plantDiamChange1, plantDiamChange)

plantRating <- lm(LatRating ~ Pop, data=MorphPlantData)
summary(plantRating)
plantRating1 <- lm(LatRating ~ Pop/Family, data=MorphPlantData)
summary(plantRating1)
anova(plantRating,plantRating1)


inflLeaves <- lm(Leaves ~ Pop, data=MorphInflData)
summary (inflLeaves)
inflLeavesN <- lm(Leaves ~ Pop/Plant, data=MorphInflData)
summary(inflLeavesN)

## running aov, but Why?
inflLeavesNaov <- aov(Leaves ~ Pop/Plant, data=MorphInflData)
summary(inflLeavesNaov)

anova(inflLeaves, inflLeavesN)


inflLeafSize <- lm(LeafSize ~ Pop, data=MorphInflData)
summary(inflLeafSize)
inflLeafSizeN <- lm(LeafSize ~ Pop/Plant, data=MorphInflData)
summary(inflLeafSizeN)
inflLeafSizeNaov <- aov(LeafSize ~ Pop/Plant, data=MorphInflData)
summary(inflLeafSizeNaov)

inflOrder <- lm(Order ~ Pop, data=MorphInflData)
summary(inflOrder)

inflNumOrder <- lm(Leaves ~ Pop/Order, data=MorphInflData)
summary(inflNumOrder)

# mixed-model tests of inflorescence data:
inflLeavesMixed0 <- lmer(Leaves ~ Pop + (Pop|Family/Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed0)
inflLeavesMixed0a <- lmer(Leaves ~ 1 + (Pop|Family/Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed0a)
inflLeavesMixed <- lmer(Leaves ~ Pop + (Pop|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed)
inflLeavesMixed1a <- lmer(Leaves ~ Pop + (1|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed1a)
inflLeavesMixed2 <- lmer(Leaves ~ 1 + (Pop|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed2)
inflLeavesMixed3 <- lmer(Leaves ~ 1 + (1|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeavesMixed3)
anova(inflLeavesMixed, inflLeavesMixed2, inflLeavesMixed3)
anova(inflLeavesMixed, inflLeavesMixed1a)
anova(inflLeavesMixed1a, inflLeavesMixed3)
anova(inflLeavesMixed0, inflLeavesMixed)
anova(inflLeavesMixed0, inflLeavesMixed0a)
predict(inflLeavesMixed)
ranef(inflLeavesMixed)
ranef(inflLeavesMixed1a)

inflLeafSizeMixed0 <- lmer(LeafSize ~ Pop + (Pop|Family/Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed0)
inflLeafSizeMixed0a <- lmer(LeafSize ~ 1 + (Pop|Family/Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed0a)
inflLeafSizeMixed <- lmer(LeafSize ~ Pop + (Pop|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed)
inflLeafSizeMixed1a <- lmer(LeafSize ~ Pop + (1|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed1a)
inflLeafSizeMixed2 <- lmer(LeafSize ~ 1 + (Pop|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed2)
inflLeafSizeMixed3 <- lmer(LeafSize ~ 1 + (1|Plant), REML=FALSE,
  data=MorphInflData)
summary(inflLeafSizeMixed3)
anova(inflLeafSizeMixed, inflLeafSizeMixed2, inflLeafSizeMixed3)
anova(inflLeafSizeMixed, inflLeafSizeMixed1a)
anova(inflLeafSizeMixed1a, inflLeafSizeMixed3)
anova(inflLeafSizeMixed0, inflLeafSizeMixed)
anova(inflLeafSizeMixed0, inflLeafSizeMixed0a)
predict(inflLeafSizeMixed1a)
ranef(inflLeafSizeMixed)
ranef(inflLeafSizeMixed1a)
ranef(inflLeafSizeMixed2)
drop1(inflLeafSizeMixed)
drop1(inflLeafSizeMixed1a)

# boxplots:
boxplot(RosetteDiam ~ Pop, ylab="Post-reproductive rosette diam",
  data=MorphPlantData)
boxplot(Infl ~ Pop, ylab="# inflorescences", data=MorphPlantData)
boxplot(Leaves ~ Plant, las=2, cex.axis=0.8, ylab="# leaves/infl",
  data=MorphInflData)
boxplot(LeafSize ~ Plant, las=2, cex.axis=0.8, ylab="Inflorescence Leaf Diam",
  data=MorphInflData)

# functional analyses:
## mean number of leaves (per plant ID)
MeanLvs <- as.vector(with(MorphInflData,
  tapply(Leaves,Plant,mean,na.rm=TRUE)))
MorphPlantData <- cbind(MorphPlantData,MeanLvs)
MeanLvs

MeanLfSize <- as.vector(with(MorphInflData,
  tapply(LeafSize,Plant,mean,na.rm=TRUE)))
MorphPlantData <- cbind(MorphPlantData,MeanLfSize)
MorphPlantData[c(25:37),]

MeanOrder <- as.vector(with(MorphInflData,
  tapply(Order,Plant,mean,na.rm=TRUE)))
MorphPlantData <- cbind(MorphPlantData,MeanOrder)


# Conditional regression models without ScoredDate covariate:
LvsInfl <- lm(Infl ~ MeanLvs, data=MorphPlantData)
summary(LvsInfl)
InflResidLvs <- residuals(LvsInfl)
plot(InflResidLvs)
plantInflCond <- lm(InflResidLvs ~ Pop, data=MorphPlantData)
summary(plantInflCond)
plantInflMult <- lm(Infl ~ Pop + MeanLvs, data=MorphPlantData)
summary(plantInflMult)
plantInflMultR <- lm(Infl ~ MeanLvs + Pop, data=MorphPlantData)
summary(plantInflMultR)

# Test for family effects on MeanLvs, MeanLfSize:
plantLvsPop <- lm(MeanLvs ~ Pop, data=MorphPlantData)
summary(plantLvsPop)
plantLvsPopFam <- lm(MeanLvs ~ Pop/Family, data=MorphPlantData)
summary(plantLvsPopFam)
anova(plantLvsPop, plantLvsPopFam)

plantLfSizePop <- lm(MeanLfSize ~ Pop, data=MorphPlantData)
summary(plantLfSizePop)
plantLfSizePopFam <- lm(MeanLfSize ~ Pop/Family, data=MorphPlantData)
summary(plantLfSizePopFam)
anova(plantLfSizePop, plantLfSizePopFam)

# Analyses for anova model comparison:
plantInflNested <- lm(Infl ~ Pop/MeanLvs, data=MorphPlantData)
summary(plantInflNested)
plantInflAdd <- lm(Infl ~ Pop + MeanLvs, data=MorphPlantData)
summary(plantInflAdd)
plantInflPop <- lm(Infl ~ Pop, data=MorphPlantData)
summary(plantInflPop)
plantInflLvs <- lm(Infl ~ MeanLvs, data=MorphPlantData)
summary(plantInflLvs)

anova(plantInflNested,plantInflAdd) # test separate regression slopes for pops
anova(plantInflAdd,plantInflLvs) # test significance of Pop in additive model
anova(plantInflAdd,plantInflPop) # test significance of MeanLvs in add model

# Effects of lateral rating:
inflRatingNested <- lm(Infl ~ Pop/LatRating, data=MorphPlantData,
  na.action=na.exclude)
summary(inflRatingNested)
inflRatingAdd <- lm(Infl ~ Pop + LatRating, data=MorphPlantData,
  na.action=na.exclude)
summary(inflRatingAdd)
inflRating <- lm(Infl ~ LatRating, data=MorphPlantData,
  na.action=na.exclude)
summary(inflRating)
anova(inflRating,inflRatingAdd,inflRatingNested)

# Test ScoredRelDate as covariate:
plantInflCovNested <- lm(Infl ~ Pop/ScoredRelDate, data=MorphPlantData)
summary(plantInflCovNested)
plantInflCovAdd <- lm(Infl ~ Pop + ScoredRelDate, data=MorphPlantData)
summary(plantInflCovAdd)
anova(plantInflCovNested,plantInflCovAdd,plantInflPop)

plantInflCovLvsNested <- lm(Infl ~ MeanLvs/ScoredRelDate, data=MorphPlantData)
summary(plantInflCovLvsNested)
plantInflCovLvsAdd <- lm(Infl ~ MeanLvs + ScoredRelDate, data=MorphPlantData)
summary(plantInflCovLvsAdd)
plantInflCovCombined <- lm(Infl ~ MeanLvs + ScoredRelDate + Pop,
  data=MorphPlantData)
summary(plantInflCovCombined)
anova(LvsInfl,plantInflCovLvsAdd,plantInflCovCombined)

# Conditional regression model with ScoredRelDate:
InflResidLvsCov <- residuals(plantInflCovLvsAdd)
plot(InflResidLvsCov)
plantInflCovCond <- lm(InflResidLvsCov ~ Pop, data=MorphPlantData)
summary(plantInflCovCond)

# Residuals of regression on ScoredRelDate only:
plantInflDate <- lm(Infl ~ ScoredRelDate, data=MorphPlantData)
summary(plantInflDate)
InflResidDateCov <- residuals(plantInflDate)

inflResidDateLvsMult <- lm(InflResidDateCov ~ Pop/MeanLvs,data=MorphPlantData)
summary(inflResidDateLvs)
inflResidDateLvsAdd <- lm(InflResidDateCov ~ Pop+MeanLvs, data=MorphPlantData)
summary(inflResidDateLvsAdd)
inflResidDateLvs <- lm(InflResidDateCov ~ MeanLvs, data=MorphPlantData)
summary(inflResidDateLvs)
inflResidDatePop <- lm(InflResidDateCov ~ Pop, data=MorphPlantData)
summary(inflResidDatePop)
anova(inflResidDateLvs,inflResidDateLvsAdd,inflResidDateLvsMult)

inflResidDateLvsS <- lm(InflResidDateCov ~ MeanLvs,
  data=MorphPlantData, subset=(Pop=="S"))
summary(inflResidDateLvsS)
inflResidDateLvsM <- lm(InflResidDateCov ~ MeanLvs,
  data=MorphPlantData, subset=(Pop=="M"))
summary(inflResidDateLvsM)


# Effect of ScoredRelDate on Diameter:
plantDiamCovNested <- lm(RosetteDiam ~ Pop/ScoredRelDate, data=MorphPlantData)
summary(plantDiamCovNested)
plantDiamCovAdd <- lm(RosetteDiam ~ Pop + ScoredRelDate, data=MorphPlantData)
summary(plantDiamCovAdd)
anova(plantPostDiam,plantDiamCovAdd,plantDiamCovNested)


MorphPlantDataM <- with(MorphPlantData, MorphPlantData[Pop == "M",])
MorphPlantDataS <- with(MorphPlantData, MorphPlantData[Pop == "S",])

# Effect of Flowering date on number of inflorescences:
# Combined populations:
inflDateNested <- lm(Infl ~ Pop/FlowerRelDate, data=MorphPlantData)
summary(inflDateNested)
inflDateAdd <- lm(Infl ~ Pop + FlowerRelDate, data=MorphPlantData)
summary(inflDateAdd)
anova(inflDateAdd,inflDateNested)
drop1(inflDateAdd,test="F")

# Conditional regression model of inflorescences on flowering date:
inflDate <- lm(Infl ~ FlowerRelDate, data=MorphPlantData,
  na.action=na.exclude)
summary(inflDate)
inflDateResid <- residuals(inflDate)
plot(inflDateResid)
inflDateCond <- lm(inflDateResid ~ Pop, data=MorphPlantData)
summary(inflDateCond)

# Effect of bolting date on number of inflorescences:
# Combined populations:
inflDateBNested <- lm(Infl ~ Pop/BoltRelDate, data=MorphPlantData,
na.action=na.exclude)
summary(inflDateBNested)
inflDateBAdd <- lm(Infl ~ Pop + BoltRelDate, data=MorphPlantData,
  na.action=na.exclude)
summary(inflDateBAdd)
inflDateB <- lm(Infl ~ BoltRelDate, data=MorphPlantData,
  na.action=na.exclude)
summary(inflDateB)
anova(inflDateB,inflDateBAdd,inflDateBNested)

# Conditional regression model of inflorescences on bolting:
# NOTE: Using bolting rather than flowering date makes more sense!
inflDateBResid <- residuals(inflDateB)
plot(inflDateBResid)
inflDateBCond <- lm(inflDateBResid ~ Pop, data=MorphPlantData)
summary(inflDateBCond)

# Joint effect of bolting date and mean leaves on number of inflorescences:
inflBoltLvsAdd <- lm(Infl ~ BoltRelDate + MeanLvs, data=MorphPlantData,
  na.action=na.exclude)
summary(inflBoltLvsAdd)
drop1(inflBoltLvsAdd, test="F")

# Conditional regression model of inflorescences on mean leaves,
#   conditional on bolting date:
inflBoltResid <- residuals(inflDateB)
plot(inflBoltResid)
inflLvsBoltCond <- lm(inflBoltResid ~ MeanLvs, data=MorphPlantData,
  na.action=na.exclude)
summary(inflLvsBoltCond)

# Models using inflorescences conditional on measurement date:
inflResidDateBMult <- lm(InflResidDateCov ~ Pop/BoltRelDate,
  data=MorphPlantData)
summary(inflResidDateBMult)
inflResidDateBAdd <- lm(InflResidDateCov ~ Pop + BoltRelDate,
  data=MorphPlantData)
summary(inflResidDateBAdd)
inflResidDateB <- lm(InflResidDateCov ~ BoltRelDate, data=MorphPlantData)
summary(inflResidDateB)
anova(inflResidDateB,inflResidDateBAdd,inflResidDateBMult)


# Lateral rating effects conditional on evaluation date:
inflRatingCovNested <- lm(InflResidDateCov ~ Pop/LatRating,
  data=MorphPlantData, na.action=na.exclude)
summary(inflRatingCovNested)
inflRatingCovAdd <- lm(InflResidDateCov ~ Pop + LatRating,
  data=MorphPlantData, na.action=na.exclude)
summary(inflRatingCovAdd)
inflRatingCov <- lm(InflResidDateCov ~ LatRating, data=MorphPlantData,
  na.action=na.exclude)
summary(inflRatingCov)
anova(inflRatingCov,inflRatingCovAdd,inflRatingCovNested)




# Leaf Size regressions:
lfSizePop <- lm(MeanLfSize ~ Pop, data=MorphPlantData)
summary(lfSizePop)


cor.test(MeanLvs,Infl, alternative="greater", data=MorphPlantData,
  subset=(Pop == "M"))
cor.test(MeanLvs,Infl, alternative="greater", data=MorphPlantData,
  subset=(Pop == "S"))


# plots:
with(MorphPlantData, plot(MeanLvs,Infl))
abline(reg=LvsInfl)
with(MorphPlantData, plot(FlowerRelDate,Infl))
abline(reg=inflDate)
# Plots correct relative date on X axis:
BoltRelDate2 <- MorphPlantData$BoltRelDate+141
inflDateB2 <- lm(Infl ~ BoltRelDate2, data=MorphPlantData,
  na.action=na.exclude)
summary(inflDateB2)
with(MorphPlantData, plot(BoltRelDate2,Infl))
abline(reg=inflDateB2)
with(MorphPlantData, plot(MeanLvs,InflResidDateCov))
abline(reg=inflResidDateLvs)
with(MorphPlantDataM, cor(MeanLvs,Infl))
with(MorphPlantDataS, cor(MeanLvs,Infl))

# Reversed causal order:
plantLvs <- lm(MeanLvs ~ Pop, data=MorphPlantData)
summary(plantLvs)
InflLvs <- lm(MeanLvs ~ Infl, data=MorphPlantData)
summary(InflLvs)
LvsResidInfl <- residuals(InflLvs)
plot(LvsResidInfl)
plantLvsCond <- lm(LvsResidInfl ~ Pop, data=MorphPlantData)
summary(plantLvsCond)

# Effect of mean leaves on inflorescences for Sp only:

LvsInflS <- lm(Infl ~ MeanLvs, data=MorphPlantDataS)
summary(LvsInflS)

# Rhizomatous shoot probability:
rhizShootProb <- glm(RhizShoots ~ Pop, family=binomial(link=logit),
  data=MorphPlantData)
summary(rhizShootProb)

rhizContMat <- c(sum(MorphPlantDataM$RhizShoots),
  sum(MorphPlantDataM$RhizShoots)/mean(MorphPlantDataM$RhizShoots),
  sum(MorphPlantDataS$RhizShoots),
  sum(MorphPlantDataS$RhizShoots)/mean(MorphPlantDataS$RhizShoots))
dim(rhizContMat) <- c(2,2)
# change 2nd row from sum to non-rhiz total:
rhizContMat[2,] <- rhizContMat[2,]-rhizContMat[1,]
rhizShootC <- chisq.test(rhizContMat)
rhizShootC2 <- chisq.test(rhizContMat, correct=FALSE)
rhizShootF <- fisher.test(rhizContMat)

rhizEffectS <- lm(MeanLvs ~ RhizShoots, data=MorphPlantDataS)
summary(rhizEffectS)

MorphPlantDataS[,c(17,20)]

#-----------------------------------------

# Analyses of full preliminary plant data set:

# Pop effects on LatShootRelDate:
LatShootFull <- lm(LatShootRelDate ~ Pop/Family, data=MorphPreData)
summary(LatShootFull)
LatShootPop <- lm(LatShootRelDate ~ Pop, data=MorphPreData)
summary(LatShootPop)
anova(LatShootFull,LatShootPop)
LatShootKW <- kruskal.test(LatShootRelDate ~ Pop, data=MorphPreData)
# Using Cox proportional hazard modeling (requires Survival pkg.):
LatShootCox <- coxph(Surv(LatShootRelDate,LatShootStat) ~ Pop,
  data=MorphPreData2)
summary(LatShootCox)


# Pop effects on BoltRelDate:
BoltFull <- lm(BoltRelDate ~ Pop/Family, data=MorphPreData)
summary(BoltFull)
BoltPop <- lm(BoltRelDate ~ Pop, data=MorphPreData)
summary(BoltPop)
anova(BoltFull,BoltPop)
BoltKW <- kruskal.test(BoltRelDate ~ Pop, data=MorphPreData)
# Using Cox proportional hazard modeling (requires Survival pkg.):
BoltCox <- coxph(Surv(BoltRelDate,BoltStat) ~ Pop, data=MorphPreData2)
summary(BoltCox)

# Pop effects on FlowerRelDate:
FlowerFull <- lm(FlowerRelDate ~ Pop/Family, data=MorphPreData)
summary(FlowerFull)
FlowerPop <- lm(FlowerRelDate ~ Pop, data=MorphPreData)
summary(FlowerPop)
anova(FlowerFull,FlowerPop)
FlowerKW <- kruskal.test(FlowerRelDate ~ Pop, data=MorphPreData)
# Using Cox proportional hazard modeling (requires Survival pkg.):
FlowerCox <- coxph(Surv(FlowerRelDate,FlowerStat) ~ Pop, data=MorphPreData2)
summary(FlowerCox)


# Pop effects on RosetteDiam:
RosDiamFull <- lm(RosetteDiam ~ Pop/Family, data=MorphPreData)
summary(RosDiamFull)
RosDiamPop <- lm(RosetteDiam ~ Pop, data=MorphPreData)
summary(RosDiamPop)
anova(RosDiamFull,RosDiamPop)
# Using log to make more homoscedastic:
RosDiamPopL <- lm(log(RosetteDiam) ~ Pop, data=MorphPreData)
summary(RosDiamPopL)
RosDiamKW <- kruskal.test(RosetteDiam ~ Pop, data=MorphPreData)

# Pop effects on LatRating:
RatingFull <- lm(LatRating ~ Pop/Family, data=MorphPreData)
summary(RatingFull)
RatingPop <- lm(LatRating ~ Pop, data=MorphPreData)
summary(RatingPop)
anova(RatingFull,RatingPop)
RatingKW <- kruskal.test(LatRating ~ Pop, data=MorphPreData)

---------------------------------------

# Descriptive statistics:

as.vector(with(MorphPreData,tapply(LatShootRelDate,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(LatShootRelDate,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPreData,tapply(BoltRelDate,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(BoltRelDate,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPreData,tapply(FlowerRelDate,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(FlowerRelDate,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPreData,tapply(RosetteDiam,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(RosetteDiam,Pop,sd,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(log(RosetteDiam),Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPreData,tapply(LatRating,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPreData,tapply(LatRating,Pop,sd,na.rm=TRUE)))


as.vector(with(MorphPlantData,tapply(RosetteDiam,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPlantData,tapply(RosetteDiam,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPlantData,tapply(Infl,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPlantData,tapply(Infl,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPlantData,tapply(MeanLfSize,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPlantData,tapply(MeanLfSize,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPlantData,tapply(MeanLvs,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPlantData,tapply(MeanLvs,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphPlantData,tapply(MeanOrder,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphPlantData,tapply(MeanOrder,Pop,sd,na.rm=TRUE)))


as.vector(with(MorphInflData,tapply(Order,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphInflData,tapply(Order,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphInflData,tapply(Leaves,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphInflData,tapply(Leaves,Pop,sd,na.rm=TRUE)))

as.vector(with(MorphInflData,tapply(LeafSize,Pop,mean,na.rm=TRUE)))
as.vector(with(MorphInflData,tapply(LeafSize,Pop,sd,na.rm=TRUE)))


