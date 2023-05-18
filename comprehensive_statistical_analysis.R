#################################
# Libraries and Importing Dataset
#################################



install.packages("Hmisc")
install.packages("ggplot2")
install.packages('skimr') # Expands on summary() by providing a larger set of statistics

#import

library(Hmisc) 
library(skimr)
library(ggplot2)

# Importing the CSV into a Data Frame

df_1 <- read.csv("Data.csv", header = TRUE)



###############################
# Displaying Summary Statistics
###############################



df_1

head(df_1)

tail(df_1)

summary(df_1)

skim(df_1) # Perform skim to display summary statistics
warnings(df_1) # to view warnings

# Checking for missing data

sum(is.na(df_1))
colSums(is.na(df_1)) 

# dealing with missing Data

clean.data <- na.omit(df_1)

sum(is.na(clean.data))

# Converting each column into  numeric

clean.data$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD. <-
  as.numeric(clean.data$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.)

clean.data$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS. <-
  as.numeric(clean.data$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)

clean.data$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.<- 
  as.numeric(clean.data$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.)

clean.data$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.<- 
  as.numeric(clean.data$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.)

clean.data$Domestic.general.government.health.expenditure.per.capita..current.US....SH.XPD.GHED.PC.CD.<-
  as.numeric(clean.data$Domestic.general.government.health.expenditure.per.capita..current.US....SH.XPD.GHED.PC.CD.)

clean.data$Domestic.private.health.expenditure....of.current.health.expenditure...SH.XPD.PVTD.CH.ZS.<-
  as.numeric(clean.data$Domestic.private.health.expenditure....of.current.health.expenditure...SH.XPD.PVTD.CH.ZS.)

clean.data$Educational.attainment..at.least.completed.post.secondary..population.25...total......cumulative...SE.SEC.CUAT.PO.ZS.<-
  as.numeric(clean.data$Educational.attainment..at.least.completed.post.secondary..population.25...total......cumulative...SE.SEC.CUAT.PO.ZS.)

clean.data$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.<-
  as.numeric(clean.data$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.)

clean.data$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.<-
  as.numeric(clean.data$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)

clean.data$Mortality.rate..infant..female..per.1.000.live.births...SP.DYN.IMRT.FE.IN.<-
  as.numeric(clean.data$Mortality.rate..infant..female..per.1.000.live.births...SP.DYN.IMRT.FE.IN.)

clean.data$Number.of.maternal.deaths..SH.MMR.DTHS.<- 
  as.numeric(clean.data$Number.of.maternal.deaths..SH.MMR.DTHS.)

clean.data$Number.of.neonatal.deaths..SH.DTH.NMRT.<-
  as.numeric(clean.data$Number.of.neonatal.deaths..SH.DTH.NMRT.)

clean.data$Number.of.surgical.procedures..per.100.000.population...SH.SGR.PROC.P5.<-
  as.numeric(clean.data$Number.of.surgical.procedures..per.100.000.population...SH.SGR.PROC.P5.)

clean.data$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.<-
  as.numeric(clean.data$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.)

clean.data$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.<-
  as.numeric(clean.data$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.)

clean.data$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.<-
  as.numeric(clean.data$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.)

clean.data$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.<-
  as.numeric(clean.data$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)

clean.data$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.<-
  as.numeric(clean.data$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.)

clean.data$Proportion.of.population.spending.more.than.10..of.household.consumption.or.income.on.out.of.pocket.health.care.expenditure......SH.UHC.OOPC.10.ZS.<-
  as.numeric(clean.data$Proportion.of.population.spending.more.than.10..of.household.consumption.or.income.on.out.of.pocket.health.care.expenditure......SH.UHC.OOPC.10.ZS.)

clean.data$Women.making.their.own.informed.decisions.regarding.sexual.relations..contraceptive.use.and.reproductive.health.care.....of.women.age.15.49...SG.DMK.SRCR.FN.ZS.<-
  as.numeric(clean.data$Women.making.their.own.informed.decisions.regarding.sexual.relations..contraceptive.use.and.reproductive.health.care.....of.women.age.15.49...SG.DMK.SRCR.FN.ZS.)

# Further inspecting the Dataset

str(clean.data)

names(clean.data)

summary(clean.data)  

summary(clean.data$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.)
summary(clean.data$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)
summary(clean.data$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.)
summary(clean.data$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.)

describe(clean.data)
skim(clean.data)
# check again for missing data

sum(is.na(clean.data))

colSums(is.na(clean.data)) # check which columns contain n/a

View(colSums(is.na(clean.data)))

# list rows with missing data

missingdata <- clean.data[!complete.cases(clean.data),]

View(missingdata)

# dealing with missing Data. There are a number of options
# 1st option is to clean the Dataset; i.e, omit /delete every n/a from the Dataset

clean.data_1 <- na.omit(clean.data)

sum(is.na(clean.data_1))

View(sum(is.na(clean.data_1))) 

# clean.data_1 <- na.omit(clean.data) does not work for this dataset, rather it
# has omitted and deleted every value in the Dataset. So we try the other method.

# the 2nd option is to perform imputation. Imputation is where the missing 
# values in the Dataset are replaced with other values such as the column mean 
# or median

# MEAN
# what to do: create a new data object: "clean_data.impute" and dump into my
# "clean.data" dataset. More like creating a duplicate Dataset or cloning the 
# original Dataset. This is so that we can compare between the original Dataset
# and the new Dataset.

clean_data.impute <- clean.data

for (i in which(sapply(clean_data.impute, is.numeric))) {
  clean_data.impute[is.na(clean_data.impute[, i]), i] <- mean(clean_data.impute[, i], na.rm = TRUE)
}

sum(is.na(clean_data.impute))

# there are still 300 missing values after cleaning

missingdata_1 <- clean_data.impute[!complete.cases(clean_data.impute),]
View(missingdata_1) ## list rows with missing data

colSums(is.na(clean_data.impute)) # check which columns contain n/a

View(colSums(is.na(clean_data.impute)))

# removing the column with no dataset (300 n/a)

#install.packages("dplyr")
library(dplyr)

df_final <- clean_data.impute %>% select (-Women.making.their.own.informed.decisions.regarding.sexual.relations..contraceptive.use.and.reproductive.health.care.....of.women.age.15.49...SG.DMK.SRCR.FN.ZS.)

View (df_final)

# checking the cleaned dataset one last time

str(df_final)
sum(is.na(df_final))
colSums(is.na(df_final))
skim(df_final)

# Filtering Data Frame to create new Data Frames

yr_16 <- df_final[df_final$Time == 2016,]

yr_21 <- df_final[df_final$Time == 2021,]

yr_11 <- df_final[df_final$Time == 2011,]

total_yrs <- df_final[df_final$Time >= 2007,]
total_yrs

five_yrs <- df_final[df_final$Time == 2007 | df_final$Time == 2008 | df_final$Time == 2009 | df_final$Time == 2010 | df_final$Time ==2011,]
five_yrs

five_yrs_a <- df_final[df_final$Time == 2012 | df_final$Time == 2013 | df_final$Time == 2014 |df_final$Time == 2015 | df_final$Time == 2016,]
five_yrs_a

five_yrs_b <- df_final[df_final$Time == 2017 | df_final$Time == 2018 | df_final$Time == 2019 |df_final$Time == 2020 | df_final$Time == 2021,]
five_yrs_b

AUS <- df_final[df_final$Country.Name=="Australia",]
AUS

UK <- df_final[df_final$Country.Name == "United Kingdom",]
print(UK)

ISR <- df_final[df_final$Country.Name == "Israel",]

USA <- df_final[df_final$Country.Name == "United States",]

NOR <- df_final[df_final$Country.Name == "Norway",]
NOR

five_countries <- df_final[df_final$Country.Name == "Australia"|df_final$Country.Name == "Austria"|
                       df_final$Country.Name == "Canada"|df_final$Country.Name == "New Zealand"|
                       df_final$Country.Name == "Netherlands",]

five_countries

five_countries_a <- df_final[df_final$Country.Name == "Unites States"|df_final$Country.Name == "United Kingdom"|
                         df_final$Country.Name == "Norway"|df_final$Country.Name == "Israel"|
                         df_final$Country.Name == "Ireland",]

five_countries_a

five_countries_b <- df_final[df_final$Country.Name == "Iceland"|df_final$Country.Name == "Switzerland"|
                         df_final$Country.Name == "Germany"|df_final$Country.Name == "Singapore"|
                         df_final$Country.Name == "Sweeden",]

five_countries_b

five_countries_c <- df_final[df_final$Country.Name == "Japan"|df_final$Country.Name == "Belgium"|
                         df_final$Country.Name == "Denmark"|df_final$Country.Name == "Finland"|
                         df_final$Country.Name == "Hong Kong SAR, China",]

five_countries_c



###########################
# Quick Data Visualisation
###########################



# Kernel Density Plots

plot(density(yr_16$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.))
plot(df_final) # figure margins too large
plot(density(five_countries$Number.of.neonatal.deaths..SH.DTH.NMRT.))
plot(density(five_countries_a$Number.of.neonatal.deaths..SH.DTH.NMRT.))
plot(density(five_countries_b$Number.of.neonatal.deaths..SH.DTH.NMRT.))
plot(density(five_countries_c$Number.of.neonatal.deaths..SH.DTH.NMRT.))

plot(density(five_yrs$Number.of.neonatal.deaths..SH.DTH.NMRT.))
plot(density(five_yrs_a$Number.of.neonatal.deaths..SH.DTH.NMRT.))
plot(density(five_yrs_b$Number.of.neonatal.deaths..SH.DTH.NMRT.))



# Scatter plots

# plot(UK$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD., 
     # df$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS., col = "purple",
     # xlab = "Net National (UK) Income", ylab = "Health Expenditure of GDP" )

# plot(five_yrs$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN., df$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.,
     # col = "purple", xlab = "Crude Death Rate", ylab = "Health Expenditure")

plot(yr_16$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.,
     col = "purple")

plot(yr_21$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.,
     col = "red")

plot(yr_11$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.,
     col = "blue")

# Barplots

barplot(AUS$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
barplot(AUS$Number.of.maternal.deaths..SH.MMR.DTHS.)
barplot(ISR$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.)

 # checking for people who have basic access to drinking water and the prevalence of Anaemia in Israel

barplot(ISR$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.)
barplot(ISR$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.)
barplot(ISR$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.)
barplot(ISR$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)
barplot(ISR$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.)

# checking for people who have basic access to drinking water and the prevalence of Anaemia in the UK

barplot(UK$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.)
barplot(UK$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.)
barplot(UK$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.)
barplot(UK$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)
barplot(UK$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.)

barplot(UK$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)
barplot(ISR$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)
barplot(AUS$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)
barplot(US$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)

barplot(UK$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
barplot(US$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
barplot(AUS$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
barplot(ISR$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
barplot(NOR$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)

# Histograms

hist(yr_16$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.)
hist(yr_16$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.)
hist(yr_16$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.)
hist(yr_16$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)
hist(yr_16$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.)
hist(yr_21$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.)
hist(yr_11$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.)

hist(total_yrs$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.)
hist(total_yrs$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.)
hist(total_yrs$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.)


# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
# library(caret)

# Feature plots
# featurePlot(x = df[,5:24],
            # y = df$Time,
            # plot = "box",
            # strip = strip.custom(par.strip.text=list(cex=.7)),
            # scales = list(x = list(relation="free"),
                          # y = list(relation="free")))

# Box plots

UK <- droplevels(subset(df_final, Country.Name = "United Kingdom"))

boxplot(UK$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS., data=UK)
boxplot(UK$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM., data = UK)

US <- droplevels(subset(df_final, Country.Code = "USA"))
boxplot(Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.~Number.of.maternal.deaths..SH.MMR.DTHS., data = US, col=rainbow(8))



###########################################
# Exporting the DataFrame to a CSV file
##########################################



write.csv(df_final, "dF.csv")
?write.csv

# Code Explanation

# df: name of the data frame in the environment ; Dataset to save. Need to be 
#     the same name of the data frame in the environment.
# “dF.csv”: Name the file dF and store it as csv; A string. Set the destination 
#           path. Path + filename + extension i.e. 
#           "/Users/USERNAME/Downloads/mydata.csv" or the filename + extension 
#           if the folder is the same as the working directory
# https://www.guru99.com/r-exporting-data.html

# Opening the directory folder

# create a function called open_folder() to open the directory folder to see 
# where the csv file is stored.
# # https://www.guru99.com/r-exporting-data.html

open_folder <-function(dir){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)  
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}
# Call the function to open the folder
open_folder(directory)



###########################################
# Hypothesis Testing
##########################################




install.packages("datarium")
install.packages("qqplotr")
#install.packages("ggplot2")
#install.packages("dplyr")
install.packages("tidyverse")
library(ggplot2)
library(datarium)
library(qqplotr)
library(dplyr)
library(tidyverse)


# T-test


US <- droplevels(subset(df_final, Country.Code = "USA"))

boxplot(US$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE., data=US)

boxplot(Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.~Number.of.maternal.deaths..SH.MMR.DTHS., data = US, col=rainbow(8))

qplot(df_final$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE., geom = "histogram")
qplot(X, geom = "histogram")


X <- df_final$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.

mean(X)
sd(X)

# OBJECTIVE 1:
# Null Hypothesis: The true mean of the Maternal Mortality Ratio = 5
# Alternative Hypothesis: The true mean of the Maternal Mortality Ratio =/ 5

t.test(X, mu = 5,)

# We DO NOT REJECT the Null Hypothesis


# Chi-square test


# This data includes two variables to be tested, both numerical, but have been 
# converted to Factors for the purpose of running the hypothesis testing. The 
# two variables are Adult Female Mortality Rate and Net National Income.

df_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD. <-
  as.factor(df_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.)

df_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.<-
  as.factor(df_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)

MR <- df$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.
NI <- df$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.

contingency_table <- table(MR, NI)
contingency_table

# Plotting the Data as a stacked bar chart

ggplot(df_final) +
  aes(x = Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE., fill = Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.) +
  geom_bar() +
  labs(title = "Adult Female Mortality Rate and Net National Income",
       x = "Adult Female M.R", y = "Net Income")

# My Null and Alternative Hypothesis are:

#   H0: Adult Female Mortality Rate and Net National Income are not associated
#   H1: Adult Female Mortality Rate and Net National Income are associated


chisq.test(contingency_table)

# Here the p-value is < 2.2e-16, which is 0.00000000000000022! So, the 
# Null Hypothesis is rejected and the Alternative Hypothesis is accepted.

# The Net National Income Of Adult Females is associated/related to Adult 
# Female Mortality Rate.



# This data includes two variables to be tested, both numerical, but have been 
# converted to Factors for the purpose of running the hypothesis testing. The 
# two variables are A Prevalence of Anaemia among Pregnant Women and 
# Maternal Deaths.

df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.<-
  as.factor(df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)

df_final$Number.of.maternal.deaths..SH.MMR.DTHS.<-
  as.factor(df_final$Number.of.maternal.deaths..SH.MMR.DTHS.)

APW <- df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.
MD <- df_final$Number.of.maternal.deaths..SH.MMR.DTHS.

contingency_table_1 <- table(APW, MD)
contingency_table_1

# My Null and Alternative Hypothesis are:

#   H0: Prevalence of Anaemia among Pregnant Women and Number of Maternal Deaths
#       are not associated
#   H1: Prevalence of Anaemia among Pregnant Women and Number of Maternal Deaths
#       are associated

chisq.test(contingency_table_1)

# # Here the p-value is < 2.2e-16, which is 0.00000000000000022! So, the 
# Null Hypothesis is rejected and the Alternative Hypothesis is accepted.

# The Prevalence of Anaemia among pregnant women is associated/related to 
# Maternal Deaths.




###########################################
# Correlation Analysis
##########################################




install.packages("corrplot")
library(corrplot)

## Correlation Coefficient Between Two Variables#########


df_final
skim(df_final)

# the Variables: Adjusted Net National Income, Female Mortality Rate, Number
# of Maternal Deaths and Prevalence of Anaemia among Pregnant Women are Factors.
# They must be converted to Numeric Variables

df_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD. <-
  as.numeric(df_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.)

df_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.<-
  as.numeric(df_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)

df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.<-
  as.numeric(df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.)

df_final$Number.of.maternal.deaths..SH.MMR.DTHS.<-
  as.numeric(df_final$Number.of.maternal.deaths..SH.MMR.DTHS.)

# The Variables: Country.Code, Country.Name, and Time.Code are categorical
# variables. They have to be removed for the correlation Analysis.

newdf_final <- df_final %>% select(-Country.Code, -Country.Name, -Time.Code, -Time)
newdf_final
skim(newdf_final)


# Pearson correlation between 2 Variables

?cor
cor(df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM., df_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.)

cor(df_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM., df_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
#cor(newdf_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM., newdf_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.)
cor(newdf_final$PAWP, newdf_final$FAMR)

cor(df_final$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS., df_final$Number.of.neonatal.deaths..SH.DTH.NMRT.)
#cor(newdf_final$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS., newdf_final$Number.of.neonatal.deaths..SH.DTH.NMRT.)
cor(newdf_final$PAC, newdf_final$NND)

# Since the correlation between variables X and Y is equal to the correlation 
# between variables Y and X, the order of the variables in the cor() function 
# does not matter.

cor(newdf_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD., newdf_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE., method = "spearman")
cor(newdf_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM., newdf_final$Number.of.neonatal.deaths..SH.DTH.NMRT., method = "spearman")
cor(newdf_final$FIMR, newdf_final$PAWP, method = "spearman")
cor(newdf_final$PAWP, newdf_final$FAMR, method = "spearman")


#Since the correlation between variables X and Y is equal to the correlation 
# between variables Y and X, the order of the variables in the cor() function 
# does not matter.

# a positive correlation implies that the two variables under consideration 
# vary in the same direction, i.e., if a variable increases the other one 
# increases and if one decreases the other one decreases as well.

# Regarding the strength of the relationship: The more extreme the correlation 
# coefficient (the closer to -1 or 1), the stronger the relationship. This also 
# means that a correlation close to 0 indicates that the two variables are 
# independent, that is, as one variable increases, there is no tendency in the 
# other variable to either decrease or increase.

## Correlations for All Variables ###########################

round(cor(newdf_final), digits = 2)

# To have a slightly improved and better version -than the Correlation Matrix 
# which just gives an overview of the correlations for all combinations of 
# two variables correlation matrix - of the Correlation MAtrix which presents 
# the correlation coefficients by colouring the coefficients based on their 
# signs, the library(corrplot) is used.

# However, due to the quite lengthy column names, the visualisation of the 
# Correlation Matrix was returning the error/warning message:

# "In corrplot(cor(newdf_final)) :
#    Not been able to calculate text margin, please try again with a clean new 
#    empty window using {plot.new(); dev.off()} or reduce tl.cex"

# # So the Data Frame: "newdf_final" Columns had to be renamed before the 
# visualisation of the correlation matrix was done again.

names(newdf_final)
names(newdf_final) <- c('PAWP', 'NMD', 'ANNI', 'NCD', 'CHE', 'CDR', 'GHE',
                        'PHE', 'EA', 'MMR', 'FAMR', 'FIMR', 'NND', 'NSP',
                        'BDWS', 'PAC', 'PANP', 'PAWR', 'PSM')

corrplot(cor(newdf_final),
         method = "number", 
         type = "upper") 



###########################################
# Regression Analysis
##########################################



install.packages("car")
install.packages("caret")
#install.packages("stats")
library(car)
library(corrplot)
library(caret)
library(stats)

# The data had already been previously loaded as "newdf_final"
# checking the data

head(as.data.frame(newdf_final))

# The variables and the data types for each variable had also previouly
# been adjusted under the correlation matrix section,
# the data.frame has300 obs. of  19 numeric variables

str(newdf_final)

# To find out which Independent Variable (x) is possibly better and can 
# explain visually can explain a selected Dependent Variable (y), a 
# correlation matrix has to be built and visualised  using the corrplot package.

# However, due to the quite lengthy column names, the visualisation of the 
# Correlation Matrix was returning the error/warning message:

# "In corrplot(cor(newdf_final)) :
#    Not been able to calculate text margin, please try again with a clean new 
#    empty window using {plot.new(); dev.off()} or reduce tl.cex"

# So the Columns had to be renamed


# Renaming the Data Frame Columns
# Ref: https://www.statology.org/how-to-rename-data-frame-columns-in-r/

names(newdf_final)
names(newdf_final) <- c('PAWP', 'NMD', 'ANNI', 'NCD', 'CHE', 'CDR', 'GHE',
                        'PHE', 'EA', 'MMR', 'FAMR', 'FIMR', 'NND', 'NSP',
                        'BDWS', 'PAC', 'PANP', 'PAWR', 'PSM')

# PAPW <- newdf_final$Prevalence.of.anemia.among.pregnant.women......SH.PRG.ANEM.
# NMD  <- newdf_final$Number.of.maternal.deaths..SH.MMR.DTHS.
# ANNI <- newdf_final$Adjusted.net.national.income..current.US....NY.ADJ.NNTY.CD.
# NCD  <- newdf_final$Cause.of.death..by.non.communicable.diseases....of.total...SH.DTH.NCOM.ZS.
# CHE  <- newdf_final$Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.
# CDR  <- newdf_final$Death.rate..crude..per.1.000.people...SP.DYN.CDRT.IN.
# GHE  <- newdf_final$Domestic.general.government.health.expenditure.per.capita..current.US....SH.XPD.GHED.PC.CD.
# PHE  <- newdf_final$Domestic.private.health.expenditure....of.current.health.expenditure...SH.XPD.PVTD.CH.ZS.
# EA   <- newdf_final$Educational.attainment..at.least.completed.post.secondary..population.25...total......cumulative...SE.SEC.CUAT.PO.ZS.
# MMR  <- newdf_final$Maternal.mortality.ratio..national.estimate..per.100.000.live.births...SH.STA.MMRT.NE.
# FAMR <- newdf_final$Mortality.rate..adult..female..per.1.000.female.adults...SP.DYN.AMRT.FE.
# FIMR <- newdf_final$Mortality.rate..infant..female..per.1.000.live.births...SP.DYN.IMRT.FE.IN.
# NND  <- newdf_final$Number.of.neonatal.deaths..SH.DTH.NMRT.
# NSP  <- newdf_final$Number.of.surgical.procedures..per.100.000.population...SH.SGR.PROC.P5.
# BDWS <- newdf_final$People.using.at.least.basic.drinking.water.services....of.population...SH.H2O.BASW.ZS.
# PAC  <- newdf_final$Prevalence.of.anemia.among.children....of.children.ages.6.59.months...SH.ANM.CHLD.ZS.
# PANP <- newdf_final$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49...SH.ANM.NPRG.ZS.
# PAWR <- newdf_final$Prevalence.of.anemia.among.women.of.reproductive.age....of.women.ages.15.49...SH.ANM.ALLW.ZS.
# PSM  <- newdf_final$Proportion.of.population.spending.more.than.10..of.household.consumption.or.income.on.out.of.pocket.health.care.expenditure......SH.UHC.OOPC.10.ZS.


## SLR ##


cor(newdf_final)
corrplot(cor(newdf_final)) 

# The correlation between PAWP (Prevalence of Anaemia among Pregnant Women) and 
# FIMR (Infant Female Mortality Rate) is/seems high. 

# So, I'll try to fit a SLR model between them; (PAWP (Y) and FIMR (X)).

?lm

Y = PAWP
X = FIMR

model_1 <- lm(PAWP ~ FIMR, newdf_final)
summary.lm(model_1)

# R-squared is 0.8985 meaning, FIMR (Infant Female Mortality Rate) can predict 
# 89% of the entire variability in the 
# PAWP (Prevalence of Anaemia among Pregnant Women)


## Assumptions #####################


## LINEARITY ##

# The relationship between X and Y must be linear. This assumption is confirmed 
# by examining a scatterplot of x and y.

# Visualising the fitted regression line: 

# First: drawing the scatter plot:

plot(PAWP ~ FIMR, newdf_final,
col = "purple",
main = "Regression: Anaemia in Pregnant Women & Infant Female Mortality",
xlab = "Infant Female Mortality",
ylab = "Anaemia in Pregnant Women")

# Second: adding the regression line to the plot

abline(model_1, col = 'blue')


## RESIDUALS' INDEPENDENCE ##

# This assumption is confirmed by examinining a scatterplot of 
# “residuals versus fits”; the correlation should be estimatedly "0". Meaning, 
# there shouldn't appear any form of relationship. Ideally, the plot would not 
# have a definite pattern and the red line plot would be approaching the 
# horizontal at zero ("0")     

plot(model_1, 1)


## NORMALITY OF RESIDUALS ##

# The residuals must be approximately normally distributed. This assumption is 
# validated by examining a normal probability plot; the observations should be 
# near the plot (line).

plot(model_1, 2)


## EQUAL VARIANCES OF THE RESIDUALS (HOMOSCEDASTICITY) ##

# The Scale-Location plot can be used to validate the Homoscedasticity 
# assumption which says variance of the residuals are constant and not related 
# to the fitted value (or even x). That is, there should be no visibly clear 
# pattern among the residuals, and the residuals should be haphazardly 
# scattered around the red plot with approximately equal variability at all 
# fitted values.

plot(model_1, 3)

# The Simple Linear Regression is quite appropriate for the selected variables 
# PAWP (Prevalence of Anaemia among Pregnant Women) and 
# FIMR (Female Infant Mortality Rate), seeing as all 4 assumptions were approved. 
# Also, this was confirmed in the Correlation Analysis Section were a 
# Strong Positive Correlation Relationship was noticed. As relating to one of my 
# objectives, the SLR model only goes to further support and prove my objective 
# that the Female Infant Mortality Rate (FIMR) is directly caused by and 
# proportional to the Prevalence of Anaemia in Pregnant Women (PAWP) and 
# vice versa. 



corrplot(cor(newdf_final)) 

# The correlation between FAMR (Adult Female Mortality Rate) and 
# PAWP (Prevalence of Anaemia among Pregnant Women) and is/seems high. 

# So, I'll try to fit an SLR model between them; (FAMR (Y) and PAWP (X)).

Y = FAMR
x = PAWP

model_2 <- lm(FAMR ~ PAWP, newdf_final)
summary.lm(model_2)

# R-squared is 0.6627 meaning, 
# PAWP (Prevalence of Anaemia among Pregnant Women)  can predict 66% of the 
# entire variability in the FAMR (Adult Female Mortality Rate)


## Assumptions #####################


## LINEARITY ##

# The relationship between X and Y must be linear. This assumption is confirmed 
# by examining a scatterplot of x and y.

# Visualising the fitted regression line: 

# First: drawing the scatter plot:

plot(FAMR ~ PAWP, newdf_final,
     col = "blue",
     main = "Regression: Adult Female Mortality & Anaemia in Pregnant Women",
     ylab = "Adult Female Mortality",
     xlab = "Anaemia in Pregnant Women")

# Second: adding the regression line to the plot

abline(model_2, col = 'red')


## RESIDUALS' INDEPENDENCE ##

# This assumption is confirmed by examinining a scatterplot of 
# “residuals versus fits”; the correlation should be estimatedly "0". Meaning, 
# there shouldn't appear any form of relationship. Ideally, the plot would not 
# have a definite pattern where the red line plot would be approaching the 
# horizontal at zero ("0")     

plot(model_2, 1)


## NORMALITY OF RESIDUALS ##

# The residuals must be approximately normally distributed. This assumption is 
# validated by examining a normal probability plot; the observations should be 
# near the plot (line).

plot(model_2, 2)


## EQUAL VARIANCES OF THE RESIDUALS (HOMOSCEDASTICITY) ##

# The Scale-Location plot can be used to validate the Homoscedasticity 
# assumption which says variance of the residuals are constant and not related 
# to the fitted value (or even x). That is, there should be no visibly clear 
# pattern among the residuals, and the residuals should be haphazardly 
# scattered around the red plot with approximately equal variability at all 
# fitted values.

plot(model_2, 3)

# The Simple Linear Regression is also quite appropriate for the selected variables: 
# FAMR (Female Adult Mortality Rate) and 
# PAWP (Prevalence of Anaemia among Pregnant Women), since all 4 assumptions 
# were proved. 
# This correlation relationship was also confirmed in the Correlation 
# Analysis Section were a strong Positive Correlation Relationship was noticed. 

# As relating to one of my objectives, the SLR model goes to further support, 
# credit, and prove my objective that the 
# Prevalence of Anaemia in Pregnant Women (PAWP) is directly responsible and
# one of the main causes of Female Adult Mortality Rate (FAMR).


## MLR ##


# OBJECTIVE: To examine the possible linear relationship between 
# PANP (Y) (Prevalence of Anaemia among Non_Pregnant Women) and a number of
# Independent Variables (X!, X2, X3, ...).

# To find out which Independent Variables (X1, X2, X3, ...) are possibly better 
# and can explain the revalence of Anaemia among Non-Pregnant Women, a 
# correlation matrix has to be built and visualised  using the corrplot package.

# For the MLR, the same "newdf_final" dataset is still being used

corrplot(cor(newdf_final)) 

# EA (Educational Attainment), MMR (Maternal Mortality Ratio), 
# BDWS (People access to Basic Drinking Water Services), and 
# PAC (Prevalence of Anaemia among Children) seem to have a somewhat 
# big correlation with PANP (Prevalence of Anaemia among Non_Pregnant People).

# Performing the Linear Regression Analysis

# To write the formula option:

Y = PANP
X1 = EA
X2 = MMR
X3 = BDWS

model_3 <- lm(PANP ~ EA + MMR + BDWS, newdf_final )
summary.lm(model_3)

# Checking the Pr(>|t|) column, the intercept and 2 of the coefficients are 
# significant. The MLR equation will be:
# PANP = 11.47343 + (-0.85741) *  MMR + 0.61258 * BDWS

# Adjusted R-squared = 0.57. This means Maternal Mortality Ratio (MMR) and 
# People access to Basic Drinking Water Services (BDWS) can predict 57% of 
# the total variability in the Prevalence of Anaemia among Non-Pregnant People
# (PANP).

# Trying to improve the model

# To write the formula option:

Y = PANP
X1 = EA
X2 = MMR
X3 = BDWS
X4 = PAC

model_4 <- lm(PANP ~ EA + MMR + BDWS + PAC, newdf_final )
summary.lm(model_4)

# Checking the Pr(>|t|) column, the intercept and 4 of the coefficients are 
# significant. The MLR equation will be:
# PANP = 8.191979 + (-0.040349) *  EA + (-0.270784) * MMR 
#         + 0.302449 * BDWS + 0.629565 * PAC

# Adjusted R-squared = 0.84. This shows a good fit; This means 
# Educational Attainment (EA), Maternal Mortality Ratio (MMR), 
# People access to Basic Drinking Water Services (BDWS), and 
# Prevalence of Anaemia among Children (PAC) can predict 84% of 
# the total variability in the Prevalence of Anaemia among Non-Pregnant People
# (PANP).

# By adding one more variable: Prevalence of Anaemia among Non-Pregnant People
# (PANP), and fitting a Multiple Linear Regression, a better model with a more 
# powerful predicting power was developed. 

# The negative sign of the Educational Attainment (EA) and Maternal Mortality 
# Ratio (MMR) means the variables have a negative relation with 
# Anaemia among Non-Pregnant People (PANP) as seen in the Correlation Matrix. 


## Making sure the fitted model meets MLR Assumptions #####################


## LINEARITY ##

# The relationship between X and Y must be linear. This assumption is confirmed 
# by examining a scatterplot of x and y.

# First:  Find each column's index

data.frame(colnames(newdf_final))

# The dataset has 19 columns with indices from 1 to 19. The model has 
# 5 variables (4 IVs and 1 DV)

# To draw the scatter plot matrix, the indices for the 5 variables are 
# identified and put in the c() vector, while putting the DV first.

pairs(newdf_final[,c(17, 9, 10, 15, 16)], lower.panel = NULL, pch =19, cex = 0.2)


## RESIDUALS' INDEPENDENCE ##

# This assumption is confirmed by examinining a scatterplot of 
# “residuals versus fits”; the correlation should be estimatedly "0". Meaning, 
# there shouldn't appear any form of relationship. Ideally, the plot would not 
# have a definite pattern where the red line plot would be approaching the 
# horizontal at zero ("0")

plot(model_4, 1)


## NORMALITY OF RESIDUALS ##

# The residuals must be approximately normally distributed. This assumption is 
# validated by examining a normal probability plot; the observations should be 
# near the plot (line).

plot(model_4, 2)


## EQUAL VARIANCES OF THE RESIDUALS (HOMOSCEDASTICITY) ##

# The Scale-Location plot can be used to validate the Homoscedasticity 
# assumption which says variance of the residuals are constant and not related 
# to the fitted value (or even x). That is, there should be no visibly clear 
# pattern among the residuals, and the residuals should be haphazardly 
# scattered around the red plot with approximately equal variability at all 
# fitted values.

plot(model_4, 3)


## NO MULTICOLLINEARITY ##

# Multiple Linear Regression assumes that none of the Independent (Predictor) 
# Variables are unusually correlated with each other. This is because when one 
# or more Independent (Predictor) Variables are unusually correlated, the 
# regression model suffers from multicollinearity; this causes the coefficient
# estimates in the model to become unreliable. 

# The most widespread form of detecting multicollinearity is by using the 
# Variance Inflation Factor (VIF); it measures the correlation and strength of 
# correlation between the independent/predictor variables in a regression model. 
# The value for VIF starts at 1 and has no upper limit.
vif(model_4)

# All the values are less than (<) 5, therefore we can conclude there is no 
# collinearity between the Independent Variables

# All 5 assumptions were approved, and I can confirm my fitted Regression Line 
# as follows:
# PANP = 8.191979 + (-0.040349) *  EA + (-0.270784) * MMR 
#         + 0.302449 * BDWS + 0.629565 * PAC



###########################################
# Time Series Analysis
##########################################



install.packages("TTR")
library(TTR)

# I start by using the Dataset I've used all along for this project: "df_final"

df_final
names(df_final)

# Removing some columns (Country by code, Country by Name, and Time by code) 
# that are irrelevant to running a Time Series Analysis. Also, renaming the 
# columns so that the Time series Data is a little bit visually appealing.

tsdf_final<- df_final %>% select(-Country.Code, -Country.Name, -Time.Code)
tsdf_final
str(tsdf_final)
names(tsdf_final)
names(tsdf_final) <- c('TIME','PAWP', 'NMD', 'ANNI', 'NCD', 'CHE', 'CDR', 'GHE',
                       'PHE', 'EA', 'MMR', 'FAMR', 'FIMR', 'NND', 'NSP',
                       'BDWS', 'PAC', 'PANP', 'PAWR', 'PSM')


# Inspecting "tsdf_final"

names(tsdf_final)
row(tsdf_final)
data.frame(colnames(tsdf_final))
skim(tsdf_final)
str(tsdf_final)
summary(tsdf_final)
head(as.data.frame(tsdf_final))


# Write "tsdf_final' data as txt file to directory

write.table(tsdf_final,                                     
            file = "tsdf.txt",
            row.names = FALSE,
            col.names = FALSE)

open_folder <-function(dir){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)  
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}
# Call the function to open the folder

open_folder(directory)


## Reading Time Series Data #####################


# Apply scan function to txt file
# Reading the time series data into R

tsdf <- scan("tsdf.txt", what = "character")         
tsdf

# Storing the data in a time series object in R, so as to enable R's many 
# functions in analysing the time series data.

tsdftimeseries <- ts(tsdf, frequency = 365, start = c(2007,1))
tsdftimeseries
class(tsdftimeseries)

# 2007 is just for specifying the first year the data was collected; the 1 
# stands for the 1st interval the data was collected; in this case, it's 
# the first quarter


## Plotting Time Series #####################



# Making a plot of the time series data

plot.ts(tsdftimeseries)

# From the time plot, this time series shows a spike between years 2012 and 2014, 
# but other than that, the random fluctuations in the data are roughly constant 
# in size over the 15-year period. So, this time series plot could probably be
# described using an additive model.


##  Decomposing Time Series #####################


# When decomposing a time series data, it first has to be decided upon whether 
# the data is a seasonal or a non-seasonal data, after which the time series is 
# seperated into its distinct components. A non-seasonal time series consists 
# of a trend, and an irregular component, while a seasonal time series consists 
# of a trend, seasonal, and  an irregular component.

# The R function "decompose()" estimates the trend, seasonal, and irregular 
# components of a seasonal time series that can be described using an additive 
# model, while the Simple Moving Average function "SMA()" is used to estimate 
# the trend component of a non-seasonal time series that can be described 
# using an additive model.

# My time series data is a "Non-seasonal" data since it doesn't fluctuate with 
# time; the random fluctuations in the data are roughly constant over time.

# I'll be using the SMA() function to smoothen my time series data. the Span 
# of the moving average  will be specified using the parameter "n".

# the number "n" takes is not really definite, and has to be a trial-and-error 
# iteration until the right amount of smoothing is found. The estimation of the 
# accuracy of the trend component is sometimes based on a higher order (number) 
# of the Simple Moving Average (SMA).
tsdftimeseriesSMA8 <- SMA (tsdftimeseries, n=8)
plot.ts(tsdftimeseriesSMA8)

# After trying an SMA of"n=3" and "n=5", "n=8" finally gave me just about the 
# right amount of smoothing, and a clearer picture of the trend component. 

# Trying to estimate my time series data as a seasonal ime series using an 
# additive mode.

tsdftimeseriescomponents <- decompose(tsdftimeseries)

# It returns the below error:
# Error in `-.default`(x, trend) : non-numeric argument to binary operator

# going further to show my time series data is non-seasonal.


## Carrying out Simple Exponential Smoothing ##################

# Exponential smoothing is used for making short-term forecasts for time series.

# A simple Exponential smoothing is used for making short-term forecasts on a 
# time series that can be described using an additive model with constant level 
# and no seasonality; which makes my Time Series Data perfectly suitable for 
# Simple Exponential Smoothing (SME) since it uses an additive model and is 
# non-seasonal.

# The SME method devises a way of calculating  the steady level at the current 
# time point since it's non-seasonal and the level doesn't fluctuate with time 
# rather, it remains fairly constant. The smoothing is controlled by the "alpha"
# parameter with a value that lies betweer 0 and 1. Alpha values near 0 mean 
# that very slight weight is placed on the most recent observations when making 
# future values forecasts. 

tsdftimeseriesforecasts <- HoltWinters(tsdftimeseries, beta=FALSE, gamma=FALSE)
tsdftimeseriesforecasts

# The output of HoltWinters() suggests to me that the estimated value of the 
# alpha parameter is around '0.011'; which is quite close to '0', thereby 
# implying thatthe forecasts are based on both recent and less recent 
# observations.



