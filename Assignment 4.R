library(dplyr)
library(ggplot2)
library(gplots)
library(GGally)


#Importing data into correct format - need to update file path
redwine <- read.csv("C:/Users/seren/OneDrive/Desktop/JCU/2022SP5 - Foundations for Data Science/Assessments/Assessment 4/Data files/Data 7/winequality-red.csv", sep = ";", dec = ".", header = FALSE, na.strings = '?', skip = 1)


#Name columns in appopriate format
names(redwine) <- c("Fixed_acidity","Volatile_acidity","Citric_acid","Residual_sugar","Chlorides","Free_sulphur_dioxide","Total_sulphur_dioxide","Density","pH","Sulphates","Alcohol","Quality")


#Check for missing data
sum(is.na(redwine))  # no NAs in dataset
str(redwine)
summary(redwine)


#HEATMAP
wine_sample <- sample_frac(redwine, size=0.1, replace=FALSE) # take representative data
distM <- wine_sample %>% 
  dist(method="euclidean", p=2) %>% 
  as.matrix() 
heatmap.2(distM, keysize = 1.1, col = bluered(100), 
          trace = "none", density.info = "none")


#Correlation with quality
corr <- redwine %>% 
  summarise(across(, ~ cor(Quality,.))) %>% 
  t() %>% 
  as.data.frame()
names(corr) <- c("Correlation")


#Sub-setting quality into 3 groups
redwine$Wine_quality <- cut(redwine$Quality, breaks=c(0,4,6,10), labels=c("Low","Medium","High"))  # '<= 4''-low, '5,6'-med, '>= 7'-high 
summary(redwine$Wine_quality)

#ggpairs stratified by Wine_quality
ggpairs(redwine,
        aes(color=Wine_quality, alpha=0.5), 
        upper = list(continuous = wrap("cor", size = 3.5)))


#Sub-selecting variables for ggpairs
redwine_subset <- redwine %>% select(c("Fixed_acidity","Volatile_acidity","Citric_acid","Sulphates","Alcohol","Quality","Wine_quality"))


#ggpairs of subset
ggpairs(redwine_subset,
        aes(color=Wine_quality, alpha=0.5))  


#3 scatterplots subset by wine quality
ggplot(data=redwine, aes(x=Citric_acid, y=Fixed_acidity, colour=Wine_quality)) + geom_point() + facet_wrap(("Wine_quality"), ncol=3) + geom_smooth(method=lm, se=FALSE, colour="black") + labs(title="Citric acid vs Fixed acidity across wine qualities", x="Citric acid", y="Fixed acidity") + theme(plot.title = element_text(hjust=0.5))


## Citric acid stratified on high quality wines

#Filtering high quality wine
redwine_high <- filter(redwine, Wine_quality == "High")


#Determining cut sections
ggplot(data = redwine_high, aes(x=Citric_acid)) + geom_density(fill="firebrick4") + labs(title="Distribution of Citric acid in high quality group", x="Citric acid", y="Density") + theme(plot.title = element_text(hjust=0.5))  # density plot


#Sub-setting high wine quality by citric acid value
redwine_high$Citric_subset <- cut(redwine_high$Citric_acid, breaks=c(-0.05,0.2,1.0), labels=c("citric <0.2", "citric >0.2"))  


#ggpair of high quality wine stratified by citric acid
ggpairs(redwine_high, columns=1:11,
        aes(color=Citric_subset, alpha=0.8)) 

