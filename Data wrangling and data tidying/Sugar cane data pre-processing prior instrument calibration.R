#Assessment 4A -Simonovikj Biljana
#Table 1 Use this R code to set-up the RStudio environment
# Libraries
#~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(tidyr)
# Functions
#~~~~~~~~~~

rescale_01 <-function(x) (x-min(x))/(max(x)-min(x)) -1/2
z_stand<-function(x) (x-mean(x))/sd(x)
ExpectedBrix <- function(x) (x*0.21084778699754 + 4.28455310831511)
# Thresholds
#~~~~~~~~~~~
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105
ExpectedBrix.delta <- 1

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25
Thresh.Fibre.delta <- .25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

#Fibre Data: The Fibre data file (Lab_Fibre_Weights.csv) is a CSV file that: (a) contains the variable names 
#in the first row; #(b) uses comma (“,”) as field separator character; and (c) uses dot (“.”) as decimal point 
#character. #Table 2 Enter your R code you used to import the Fibre data into a data table called “Lab_Fibre_Data”
getwd()
Lab_Fibre_Data <- read.table("/Users/Biljana/Lab_Fibre_Weights.csv", header = TRUE, sep = ",", quote = "\"'", dec = ".")

View(Lab_Fibre_Data)
str(Lab_Fibre_Data)
dim(Lab_Fibre_Data) # check the dimensions of the data frame
head(Lab_Fibre_Data) # first 6 rows
sapply(Lab_Fibre_Data, class) # high-order function returns vector as an output and it is a version of lapply()
#function
summary(Lab_Fibre_Data)
# Calculate Percentage Fibre Variables: Table 3 Calculate the fibre percentage of the first set of measurements 
#(columns 2 to 4, named SampleWeight_1, InitialSampleCanWeight_1,and FinalSampleCanWeight_1). Name the resulting variable 
#“Fibre1” and add this new variable to the “Lab_Fibre_Data” data table, using direct assignment 
#(i.e. Lab_Fibre_Data$Fibre1 <- …).Enter your R code you used:
SampleWeight_1 <- Lab_Fibre_Data$SampleWeight_1
head(SampleWeight_1)
InitialSampleCanWeight_1 <- Lab_Fibre_Data$InitialSampleCanWeight_1
head(InitialSampleCanWeight_1)
FinalSampleCanWeight_1 <- Lab_Fibre_Data$FinalSampleCanWeight_1
head(FinalSampleCanWeight_1)
Lab_Fibre_Data$Fibre1 <- 100 * (InitialSampleCanWeight_1 - FinalSampleCanWeight_1) / SampleWeight_1
Fibre1 <- Lab_Fibre_Data$Fibre1
head(Fibre1)
dim(Lab_Fibre_Data)
View(Lab_Fibre_Data)
head(Lab_Fibre_Data)

#Table 4 Repeat the above procedure for the second set of measurements (columns 5 to 7, named SampleWeight_2, 
#InitialSampleCanWeight_2, and FinalSampleCanWeight_2), but now using the mutate() function from the dplyr package 
#(rather than direct assignment) to calculate the corresponding fibre percentage, “Fibre2”, 
#and add it as a new variable to the “Lab_Fibre_Data” data table. Enter your R code you used:
library(dplyr)
SampleWeight_2 <- Lab_Fibre_Data$SampleWeight_2
head(SampleWeight_2)
InitialSampleCanWeight_2 <- Lab_Fibre_Data$InitialSampleCanWeight_2
head(InitialSampleCanWeight_2)
FinalSampleCanWeight_2 <- Lab_Fibre_Data$FinalSampleCanWeight_2
head(FinalSampleCanWeight_2)
#Lab_Fibre_Data<-Lab_Fibre_Data %>% mutate(Fibre2 = 100 * (InitialSampleCanWeight_2 - FinalSampleCanWeight_2) / SampleWeight_2)
Lab_Fibre_Data <- mutate(Lab_Fibre_Data, Fibre2 = 100 * (InitialSampleCanWeight_2 - FinalSampleCanWeight_2) / SampleWeight_2)#df = mutate(df, newcol = x + 2)
Fibre2 <- Lab_Fibre_Data$Fibre2
head(Fibre2)
View(Lab_Fibre_Data)
dim(Lab_Fibre_Data)
head(Lab_Fibre_Data) # first 6 rows display

#Filtering Fibre Variables: The Fibre data contains missing values, which are recorded as zeros in the data. 
#Table 5 Use the filter() function to remove samples (rows) that contain a missing value in any of the weight measurements. 
#Since weights cannot be negative, do that by keeping only the rows that have positive values (> 0) for all the six raw weight measurements. 
#Save the filtered data to a new data table called Lab_Fibre_Filtered.  Enter your R code you used:

Columns <- sapply(Lab_Fibre_Data, function(Lab_Fibre_Data) sum(Lab_Fibre_Data <= 0))
print(Columns)
sum(Lab_Fibre_Data <= 0)
Rows <- unique(unlist(lapply(Lab_Fibre_Data, function(Lab_Fibre_Data) which(Lab_Fibre_Data <= 0))))
print(Rows)
length(Rows)
dim(Lab_Fibre_Data)

Lab_Fibre_Filtered <- filter(Lab_Fibre_Data, SampleWeight_1 > 0, InitialSampleCanWeight_1 > 0, FinalSampleCanWeight_1 > 0, SampleWeight_2 > 0, 
                           InitialSampleCanWeight_2 > 0, FinalSampleCanWeight_2 > 0)

dim(Lab_Fibre_Filtered)
View(Lab_Fibre_Filtered)
Columns <- sapply(Lab_Fibre_Filtered, function(Lab_Fibre_Filtered) sum(Lab_Fibre_Filtered <= 0))
print(Columns)
sum(Lab_Fibre_Filtered <= 0)
Rows <- unique(unlist(lapply(Lab_Fibre_Filtered, function(Lab_Fibre_Filtered) which(Lab_Fibre_Filtered <= 0))))
print(Rows)

#For the replicate fibre measurements, if there is an absolute difference between the two estimates (computed variables “Fibre1” and “Fibre2”) 
#equal to or greater than 0.25 units, the sample is discarded.
#Table 6 UPDATE your code in table 5 to include this maximum fibre difference limit as an additional filtering criteria. Enter your R code you used:
Fibre1 <- Lab_Fibre_Data$Fibre1
Fibre2 <- Lab_Fibre_Data$Fibre2
Lab_Fibre_Data <- mutate(Lab_Fibre_Data, Max_Fibre_Diff_Lim = abs(Fibre1 - Fibre2))

Max_Fibre_Diff_Lim <- Lab_Fibre_Filtered$Max_Fibre_Diff_Lim
head(Max_Fibre_Diff_Lim)


Lab_Fibre_Filtered <- filter(Lab_Fibre_Data, SampleWeight_1 > 0,InitialSampleCanWeight_1 > 0, FinalSampleCanWeight_1 > 0,SampleWeight_2 > 0, 
                           InitialSampleCanWeight_2 > 0, FinalSampleCanWeight_2 > 0, Max_Fibre_Diff_Lim < 0.25) 
dim(Lab_Fibre_Filtered)
View(Lab_Fibre_Filtered)

#Table 7 Calculate the final fibre estimates by averaging the replicate fibre measurements, “Fibre1” and “Fibre2”.  
#Use mutate() to calculate the average fibre and add it as a new variable named “Fibre” 
#to the Lab_Fibre_Filtered data table. Enter your R code you used:

Lab_Fibre_Filtered <- mutate(Lab_Fibre_Filtered, Fibre = (Fibre1 + Fibre2)/2)
Fibre <- Lab_Fibre_Filtered$Fibre
dim(Lab_Fibre_Filtered)
View(Lab_Fibre_Filtered)

#Finally, we need to filter out any out-of-range measurements.  The minimum and maximum values are specified in the environmental 
#threshold variables Table 8 Use a PIPE to sequentially filter the measurements in Lab_Fibre_Filtered to remove the out-of-range fibre values, first keeping only the rows of the data table for which “Fibre” 
#is greater than Thresh.Fibre.min, and then keeping only the resulting rows for which “Fibre” is less than Thresh.Fibre.max.  
#Save the resulting data into the Lab_Fibre_Filtered table.  Enter your R code you used:

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25
Lab_Fibre_Filtered <- Lab_Fibre_Filtered %>% filter(Fibre > 4 & Fibre < 25) 

View(Lab_Fibre_Filtered)

dim(Lab_Fibre_Filtered)
 
 #The resulting Lab_Fibre_Filtered data table contains many temporary variables which are no longer needed.Table 9 Use select() 
 #to save the LabID and Fibre variables (1st and last columns) from Lab_Fibre_Filtered to a new data table called Lab_Fibre.  
 #Enter your R code you used:
LabID <- Lab_Fibre_Filtered$LabID 
Lab_Fibre <- Lab_Fibre_Filtered %>% select(LabID, Fibre)
 View(Lab_Fibre)
 dim(Lab_Fibre)
#Table 10 Enter your R code you used to import the Ash data into a data table called “Lab_Ash_Data”
getwd()
Lab_Ash_Data <- read.table("/Users/Biljana/Lab_Ash_Weights.csv", header = TRUE, sep = ",", quote = "\"'", dec = ".", na.strings = "0", 
                         stringsAsFactors = TRUE, comment.char = "")
View(Lab_Ash_Data)
str(Lab_Ash_Data)
typeof(Lab_Ash_Data) # determines what type of object is crx
is.list(Lab_Ash_Data) # check if it is list
class(Lab_Ash_Data) # check class of the object crx
dim(Lab_Ash_Data) # check the dimensions of the data frame
length(Lab_Ash_Data) # check the length of lists, vectors and factors
ncol(Lab_Ash_Data) # check number of columns
nrow(Lab_Ash_Data) # check number of rows
sapply(Lab_Ash_Data,class)
head(Lab_Ash_Data)
summary(Lab_Ash_Data)
#Table 11 Use a PIPE to (a) first filter out the missing values using filter(), then sequentially calculate (b) “InitialWeight”, 
#(c) “FinalWeight” and (d) “Ash” as new variables using mutate().Save the pipe result to a new data table Lab_Ash_Calculated. 
#Enter your R code you used:
Rows <- unique(unlist(lapply(Lab_Ash_Data, function(Lab_Ash_Data) which(is.na(Lab_Ash_Data)))))
Rows
length(Rows)

Colomns <- sapply(Lab_Ash_Data, function(Lab_Ash_Data) sum(is.na(Lab_Ash_Data))) # number of missing values by coloumn
Colomns
sum(is.na(Lab_Ash_Data))


TinWeight <- Lab_Ash_Data$TinWeight
InitialSampleInTinWeight <- Lab_Ash_Data$InitialSampleInTinWeight
FinalSampleInTinWeight <- Lab_Ash_Data$FinalSampleInTinWeight

Lab_Ash_Data <- Lab_Ash_Data %>% filter(complete.cases(.)) %>% mutate(InitialWeight = InitialSampleInTinWeight - TinWeight, 
                                                                          FinalWeight  = FinalSampleInTinWeight - TinWeight)
View(Lab_Ash_Data) 
InitialWeight <- Lab_Ash_Data$InitialWeight
FinalWeight <- Lab_Ash_Data$FinalWeight
Lab_Ash_Calculated <- Lab_Ash_Data %>% mutate(Ash = 100 * FinalWeight / InitialWeight)
                                                                          
View(Lab_Ash_Calculated)
Rows <- unique(unlist(lapply(Lab_Ash_Calculated, function(Lab_Ash_Calculated) which(is.na(Lab_Ash_Calculated)))))
Rows
Columns <- sapply(Lab_Ash_Calculated, function(Lab_Ash_Calculated) sum(is.na(Lab_Ash_Calculated))) # number of missing values by coloumn
Columns
sum(is.na(Lab_Ash_Calculated))
View((Lab_Ash_Calculated))
dim(Lab_Ash_Calculated)

#We need to filter out any out-of-range measurements.  The minimum and maximum value are specified in the environmental variables 
#we initially set-up.  The threshold values for ash are: Thresh.Ash.min and Thresh.Ash.max.Table 12 Update your PIPE 
#from table 11 to filter out any out-of-range Ash values.  Hint: Add the additional filter at the end of the pipe.  
#Enter your R code you used:

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8
Ash <- Lab_Ash_Calculated$Ash
Lab_Ash_Calculated <- Lab_Ash_Data %>% filter(complete.cases(.)) %>% mutate(InitialWeight = InitialSampleInTinWeight - TinWeight, 
                                                                          FinalWeight  = FinalSampleInTinWeight - TinWeight,
                                                                          Ash = 100 * FinalWeight / InitialWeight) %>% filter(Ash > 0 & Ash < 8)
View(Lab_Ash_Calculated)
dim(Lab_Ash_Calculated)
 #Summarising Ash Variables: To ensure accuracy, ash is also measured twice, and the two measurements are supposed 
#to be averaged to provide the final result.However, unlike the fibre data, replicates occur as two separate rows (rather than columns) 
#in the data, with the same Lab ID. We need to summarise the replicate values to a single, averaged value.
#Table 13 Use a PIPE with the grouped_by() and summarize() functions to produce a data table called Lab_Ash that is grouped by 
#LabID and summarises variable Ash by taking its grouped mean values. The resulting table, Lab_Ash, must then have two variables, 
#LabID and Ash, where LabID now contains unique values (no replicates). Enter your R code you used:
Ash <- Lab_Ash_Calculated$Ash
LabID <- Lab_Ash_Calculated$LabID
View(Lab_Ash_Calculated)
Lab_Ash <- Lab_Ash_Calculated %>% select(LabID,Ash) %>% group_by(LabID) %>% summarize(Ash = mean(Ash))
View(Lab_Ash)

dim(Lab_Ash)
head(Lab_Ash)

#Table 14 Enter your R code you used to import the Pol and Brix data into a data table called “Lab_PB_Data”
getwd()
Lab_PB_Data <- read.table("/Users/Biljana/Lab_Pol_Brix.csv", header = TRUE, sep = ",", quote = "\"'", dec = ".")

View(Lab_PB_Data)
str(Lab_PB_Data)
dim(Lab_PB_Data) # check the dimensions of the data frame
sapply(Lab_PB_Data,class)
summary(Lab_PB_Data)
head(Lab_PB_Data)
#The Pol and Brix dataset does not contain any missing values because the database that exported the records had already handled missing values.  
#However, the dataset does contain anomalous measurements.  There is a function “ExpectedBrix” which uses the Pol value as the input.  
#This function calculates the expected Brix given a Pol value.  
#A difference between the measured Brix and expected Brix greater than one indicates that there was a problem in collecting either the Brix or Pol measurements.
#Table 15 Use the mutate() function to add to the data table Lab_PB_Data a new variable, PredBrix, which uses the ExpectedBrix function with Pol as input.  
#Enter your R code you used:
ExpectedBrix.delta <- 1
Pol <- Lab_PB_Data$Pol
Brix <- Lab_PB_Data$Brix
ExpectedBrix <- function(Pol) (Pol*0.21084778699754 + 4.28455310831511)

Lab_PB_Data <- Lab_PB_Data %>% mutate(PredBrix = Pol*0.21084778699754 + 4.28455310831511)
View(Lab_PB_Data)
PredBrix <- Lab_PB_Data$PredBrix
Lab_PB_Data <- Lab_PB_Data %>% mutate(Dif = abs(Brix - PredBrix))
View(Lab_PB_Data)
head(Lab_PB_Data)
Dif <- Lab_PB_Data$Dif
Lab_PB_Data %>% count(Dif < 1)
Lab_PB_Data %>% count(Dif > 1)

#Table 16 Use ggplot() to visually display a scatter (i.e. point) plot of variables Brix and Pol. Colour code the points according 
#to if the absolute difference between the measured Brix (variable Brix, 3rd column) and the predicted Brix (variable PredBrix, 4th column)
#is greater than one.  Enter your R code and the jpeg of the plot:

ggplot(data = Lab_PB_Data, mapping = aes(x = Pol, y = Brix, colour = Dif > 1)) + 
  geom_point(size = 1)

ggplot(data = Lab_PB_Data) + aes(x = Brix, y = Pol, color = Dif > 1) + 
  geom_point(size = 1) + geom_smooth(method = lm, se = FALSE)
ggplot(data = Lab_PB_Data) + aes(x = Pol, y = Brix) + 
  geom_point(size = 1) + geom_smooth(method = lm, se = FALSE) + aes(color = Dif > 1)

#bp + expand_limits(x = c(10,30), y = c(40,120))
#Table 17 Use a PIPE to sequentially filter out undesirable rows from Lab_PB_Data, and then select only a subset of its variables, as follows: 
#first, filter out samples (rows) where (a) the absolute difference between the measured Brix and predicted Brix is greater than one, and/or (b)
#any value for Pol or Brix are out of range (the min. and max. values are specified in the threshold variables we initially set-up, namely, Thresh.
#Brix.min, Thresh.Brix.max, Thresh.Pol.min, and Thresh.Pol.max).
#Then, (c) select only the variables LabID, Pol and Brix to constitute a new data table, called Lab_PB. Enter your R code you used: 

#a)filter out samples (rows) where (a) the absolute difference between the measured Brix and predicted Brix is greater than one
#sset <- filter(Lab_PB_Data, Dif & Dif < 1)
#sset
# OR
#subset<-Lab_PB_Data %>% filter(Dif < 1)
#subset
#subset<-Lab_PB_Data %>% select(Dif) %>% filter(Dif < 1)
#dim(subset)
Lab_PB <- Lab_PB_Data %>% filter(Dif < 1, Pol > 50 & Pol < 105, Brix > 15 & Brix < 30) %>% select(LabID, Pol, Brix)
dim(Lab_PB)
View(Lab_PB)

Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105
ExpectedBrix.delta <- 1
# A Single Lab File
#We have tidied and cleaned laboratory data for the Fibre, Ash and Pol and Brix samples in the three data tables Lab_Fibre, 
#Lab_Ash and Lab_PB respectively.  The common variable that links all the samples together is LabID.  
#To join tables together using a common key variable, we can use the full_join() function from the dplyr library.
#Table 18 Use the following R code to join the Fibre and Ash tables together


Lab <- full_join(Lab_Ash, Lab_Fibre, by = c("LabID" = "LabID"))
View(Lab)
dim(Lab)

#Table 19 Join the existing Lab data table to the Lab_PB data table.  
#Enter you R code and the first eleven rows of the combined Lab data table. Hint: Use Lab[1:11,] to display the top 11 rows: 
Lab <- full_join(Lab_PB, Lab, by = c("LabID" = "LabID"))
View(Lab)
# Table 19 Join the existing Lab data table to the Lab_PB data table.  
#Enter you R code and the first eleven rows of the combined Lab data table. Hint: Use Lab[1:11,] to display the top 11 rows: 
Lab[1:11,]

#To save the Lab data table, we can use the write.table() function in the R base library.
#Table 20 Use the following R code to save the Lab data table to disk 

write.table(Lab, file = "Lab_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

getwd() # get working directory


Lab_Out <- read.csv("Lab_Out.csv") # load the data into R

str(Lab_Out) # data structure compact display

summary(Lab_Out) # summary statistics of the data

#Table 21 Use transform() to transform the Fibre measurements from the Lab_Fibre table using the provided z_stand() function.  
#Then save the resulting table (containing variables LabID and Fibre) to a file on disk.  Enter the R code you use to perform both actions. 
#View(Lab_Fibre)
Fibre <- Lab_Fibre$Fibre
LabID <- Lab_Fibre$LabID
z_stand <- function(Fibre) (Fibre - mean(Fibre))/sd(Fibre)

Lab_Fibre <- transform(Lab_Fibre, Fibre = (Fibre - mean(Fibre))/sd(Fibre))
head(Lab_Fibre)
Lab_Fibre_Out <- select(Lab_Fibre, LabID, Fibre)
View(Lab_Fibre_Out)
dim(Lab_Fibre_Out)
write.table(Lab_Fibre_Out, file = "Lab_Fibre_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
Lab_Fibre_Out <- read.csv("Lab_Fibre_Out.csv")
str(Lab_Fibre_Out)
summary(Lab_Fibre_Out)
#Table 22 Use a pipe with two subsequent transform() operations to transform the Ash measurements from the Lab_Ash table, first using log10(), 
#and then using z_stand(). Save the resulting table (containing variables LabID and Ash) to a file on disk.  
#Enter the R code you use to perform both actions.
LabID <- Lab_Ash$LabID
Ash <- Lab_Ash$LabID
z_stand <- function(Ash) (Ash - mean(Ash))/sd(Ash)

                                
Lab_Ash_Out <- Lab_Ash %>% select(LabID,Ash) %>% transform(Ash = log10(Ash)) %>% transform(Ash = (Ash - mean(Ash))/sd(Ash))
dim(Lab_Ash_Out)
View(Lab_Ash_Out)                                      
write.table(Lab_Ash_Out, file = "Lab_Ash_Out.csv", append = FALSE, quote = TRUE, sep = ",",
             eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "") # data output saved as Lab_Fibre_Out.csv in working directory 

getwd() # get working directory
Lab_Ash_Out <- read.csv("Lab_Ash_Out.csv") # load the data into R

str(Lab_Ash_Out)
summary(Lab_Ash_Out)
#Table 23 Use the following R code to create a variable which can be subsequently used for stratified sub-sampling: 
Lab_PB$Bbin <- cut(Lab_PB$Brix, 40, labels = FALSE)
Bbin <- Lab_PB$Bbin
length(Bbin)
head(Bbin)
# Table 24 Use the following R code to re-cast the Bbin variable as ordinal, so it can be used for stratified sub-sampling: 
Lab_PB$Bbin <- as.factor(Lab_PB$Bbin) # encoding variable as a factor 
levels(Lab_PB$Bbin) # levels of the ordinal factor variable Bbin


#Table 25 Use the group_by() function then the sample_n() function to perform stratified sampling on the 
#Brix meansurements, using Bbin as the grouping variable and size=50 for the number of samples in each stratification
# Name the resulting data table as Lab_B_Stratified_Balanced. Note: sample_n() will need to use attribute 
#replace = TRUE,
#as not all groups have fifty samples.  Enter the R code you use to perform both actions. 

Lab_B_Stratified_Balanced <- Lab_PB %>% group_by(Bbin) %>% sample_n(50, replace = TRUE)
Lab_B_Stratified_Balanced
View(Lab_B_Stratified_Balanced)
Brix <- Lab_B_Stratified_Balanced$Brix
length(Brix)
head(Brix)
#Table 26 Use transform() with rescale_01() to rescale the Brix measurements in Lab_B_Stratified_Balanced. 
#Then, use select() to select only the LabID and Brix variables, and write the resulting data table to a csv file. 
#Enter the R code you use to perform the three actions. 

Lab_Brix_Out <- Lab_B_Stratified_Balanced %>% transform(Brix = (Brix - min(Brix))/(max(Brix) - min(Brix)) - 1/2) %>% select(LabID, Brix)

rescale_01 <- function(x) (x - min(x))/(max(x) - min(x)) - 1/2
View(Lab_Brix_Out)
write.table(Lab_Brix_Out, file = "Lab_Brix_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
getwd() # get working directory
Lab_Brix_Out <- read.csv("Lab_Brix_Out.csv") # load the data into R

str(Lab_Brix_Out)
summary(Lab_Brix_Out)
dim(Lab_Brix_Out)
# 27

Lab_B_Stratified_Balanced <- Lab_PB %>% group_by(Bbin) %>% sample_n(50, replace = TRUE)
Lab_B_Stratified_Balanced

Pol <- Lab_B_Stratified_Balanced$Pol
length(Pol)
head(Pol)

Lab_Pol_Out <- Lab_B_Stratified_Balanced %>% transform(Pol = (Pol - min(Pol))/(max(Pol) - min(Pol)) - 1/2) %>% select(LabID, Pol)
View(Lab_Pol_Out)
write.table(Lab_Pol_Out, file = "Lab_Pol_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
getwd() # get working directory
Lab_Pol_Out <- read.csv("Lab_Pol_Out.csv") # load the data into R

str(Lab_Pol_Out)
summary(Lab_Pol_Out)
dim(Lab_Pol_Out)
