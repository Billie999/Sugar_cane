# Tutorial

#~~~~~~~~~~~
# Libraries
#~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(tidyr)

#~~~~~~~~~~~~
# Thresholds
#~~~~~~~~~~~
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

# Enter your R code you used to import the NIR data into a data table called “NIRData”

getwd()
NIRData <- read.table("path", header = TRUE, sep = ",", quote = "\"'", dec = ".",
                      fill = TRUE, comment.char = "", na.strings =  "?") # reads a file in table format and creates 
# data frame from it named “NIRData”

str(NIRData) # structure of data frame
dim(NIRData) # check the dimensions of the data frame
NIRData[1:15,] # first 15 rows of data frame
summary(NIRData) # summary statistics of data frame

# Use the following R code to correctly assign the DateTime variable to the POSIXct data type.
# Note that the POSIXct uses input arguments that specify the format which our DateTime uses.

NIRData$DateTime <- as.POSIXct(NIRData$DateTime, format = "%Y-%m-%d %H:%M:%S") # re-casting the “DateTime” variable 
#to the “POSIXc”t data type

DateTime <- NIRData$DateTime # assigning value to the“DateTime” variable
class(DateTime) # checking data type of “DateTime” variable
head(DateTime) # first 6 rows
NIRData$DateTime <- format(as.POSIXct(NIRData$DateTime, format = "%Y-%m-%d %H:%M:%S"), "%d/%m/%Y %H:%M") # re-formatting of the expression with format () function:“Year-month-day  
# Hours:Minutes:Seconds” is replaced with“day/month/Year Hours:Minutes” as it is in the suggested output file

DateTime <- NIRData$DateTime # assigning value to the variable “DateTime”
class(DateTime) # checking data type of “DateTime” variable after applying format() function
head(DateTime) # first 6 rows of the column “DateTime

# Use transform() with the floor() function applied to ScanID to create a new variable called LabID in the NIRData table. Enter your R code you used to create the new variable,
# then enter the first fifteen rows of the updated NIRData table.
DateTime <- NIRData$DateTime # assigning value to a name
ScanID <- NIRData$ScanID # assigning value to a name

NIRData <- NIRData %>% transform(ScanID = (floor(ScanID))) %>% rename(LabID = ScanID) # rounding “ScanID” down to the lowest integer with floor () function and transforming it into a new variable called 
# “LabID” with transform () function and rename () function 
NIRData[1:15,] # first 15 columns of “NIRData
dim(NIRData) # dimensions of the data table
str(NIRData) # data structure compact display
sapply(NIRData, class) # check the vector types of each column in the data table

# Use a PIPE to sequentially filter the NIR data by filtering out any (a) GH values greater than 3.5, (b)
# NH values greater than 2, (c) any out-of-range values for Pol, Brix, Fibre and Ash and (d) any sample that has a ScanID equal to -1.
# Save the filtered data to a new data table called NIRData_Filtered. 
GH <- NIRData$GH
NH <- NIRData$NH
NIR_Pol <- NIRData$NIR_Pol
NIR_Brix <- NIRData$NIR_Brix
NIR_Fibre <- NIRData$NIR_Fibre
NIR_Ash <- NIRData$NIR_Ash
LabID <- NIRData$LabID
DateTime <- NIRData$DateTime
NIRData_Filtered <- NIRData %>% filter(GH < 3.5)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(NH < 2)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(NIR_Pol > 50) %>% filter(NIR_Pol < 105)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(NIR_Brix > 15) %>% filter(NIR_Brix < 30)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(NIR_Fibre > 4) %>% filter(NIR_Fibre < 25)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(NIR_Ash > 0) %>% filter(NIR_Ash < 8)
dim(NIRData_Filtered)
NIRData_Filtered <- NIRData_Filtered %>% filter(LabID != -1) 
dim(NIRData_Filtered)

# OR filter out all the 11 conditions in one sequence:
NIRData_Filtered <- NIRData %>% filter(GH < 3.5) %>% filter(NH < 2) %>% 
  filter(NIR_Pol > 50) %>% filter(NIR_Pol < 105) %>% 
  filter(NIR_Brix > 15) %>% filter(NIR_Brix < 30) %>% 
  filter(NIR_Fibre > 4) %>% filter(NIR_Fibre < 25) %>% 
  filter(NIR_Ash > 0) %>% filter(NIR_Ash < 8) %>% filter(LabID != -1) 
dim(NIRData_Filtered) # the same result obtained as with sequential filtering
NIRData_Filtered[1:15,]

# Use a PIPE with the grouped_by() and summarize() functions on the “NIRData_Filtered” table to produce a 
# data table called “NIR_Fina”l which is grouped by”LabID” and contains, in addition to the grouped variable, 
# the first “DateTime” for each group as well as the corresponding mean values for “Pol”, “Brix”, “Fibre” and 
#“Ash” (i.e. the group means).
NIR_Final <- NIRData_Filtered %>% group_by(LabID) %>%
  summarise(DateTime = min(DateTime), NIR_Pol = mean(NIR_Pol), NIR_Brix = mean(NIR_Brix), 
            NIR_Fibre = mean(NIR_Fibre), NIR_Ash = mean(NIR_Ash))
# generate “NIR_Final” table that is grouped by “LabID” and summarized under min values for “DateTime”
# variable and mean values of “NIR_Pol”, “NIR_Brix”, “NIR_Fibre” and NIR_Ash” variables; the code sequence 
# produces transformed data frame corresponding to the suggested output file by dimensions of the data frame
# (rows and columns) and by variable names and order 
dim(NIR_Final) # dimensions of the new "NIR_Fila" data table
NIR_Final[1:15,] # first 15 rows of "NIR_Final"

# Finally, save the NIR_Final data table to a csv file.
# Enter your R code to save the NIR_Final data table to a csv file on disk. 
write.table(NIR_Final, file = "NIR_Final_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
                     qmethod = c("escape", "double"), fileEncoding = "") #  data output saved as 
# NIR_Final_Out.csv” in the working directory 

getwd() # get working directory
NIR_Final_Out <- read.csv("NIR_Final_Out.csv") # load the data into R

str(NIR_Final_Out) #structure of "NIR_Final_Out"
summary(NIR_Final_Out) # summary of "NIR_Final_Out"
dim(NIR_Final_Out) # dimensions of "NIR_FInal_Out"
View(NIR_Final_Out) # check "NIR_Final_Out"
