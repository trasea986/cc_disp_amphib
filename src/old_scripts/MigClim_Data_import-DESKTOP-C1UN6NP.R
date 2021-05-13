#R code for importing multiple text files, so that I can then break up and calculate the proportional shifts easier
library(plyr)
library(tidyverse)

#Going to create full dataframe for 8.5 and 4.5 and then split the excel documents up to species specific to calculate out the sensitivity
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/85_cc")
#create list of files
list_85 = list.files(pattern = "*summary.txt", 
                full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_85_RALU = ldply(list_85, read.table, sep = "\t")


#remove odd rows to get rid of repeating headers

data_85_all <-  data_85_RALU[ !c(TRUE,FALSE), ]  # rows

#add column names back in

names(data_85_all) = c("Run", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column. note that the M in "MigClim" is lost, but need it here to prevent the addition "_" from getting in the way

data_85_all_sep <- separate(data = data_85_all, col = Run, into = c("Species", "Rest"), sep = "_M")

#write full species data set
write.table(data_85_all_sep, file = "data_85_all_sep.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

#now to create one dataframe for each species
RALU_85 <- subset(data_85_all_sep, Species == "RALU")
RASY_85 <- subset(data_85_all_sep, Species == "RASY")
ANBO_85 <- subset(data_85_all_sep, Species == "ANBO")
ABMA_85 <- subset(data_85_all_sep, Species == "ABMA")
ANHE_85 <- subset(data_85_all_sep, Species == "ANHE")
PSMA_85 <- subset(data_85_all_sep, Species == "PSMA")

#write these to their own csv files
write.table(RALU_85, file = "RALU_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(RASY_85, file = "RASY_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ANBO_85, file = "ANBO_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ABMA_85, file = "ABMA_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ANHE_85, file = "ANHE_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(PSMA_85, file = "PSMA_85.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)




#now for 4.5
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/45_cc")

list_45 = list.files(pattern = "*summary.txt", 
                     full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_45_RALU = ldply(list_45, read.table, sep = "\t")


#remove odd rows to get rid of repeating headers

data_45_all <-  data_45_RALU[ !c(TRUE,FALSE), ]  # rows

#add column names back in

names(data_45_all) = c("Run", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column. note that the M in "MigClim" is lost, but need it here to prevent the addition "_" from getting in the way

data_45_all_sep <- separate(data = data_45_all, col = Run, into = c("Species", "Rest"), sep = "_M")

#write full species data set
write.table(data_45_all_sep, file = "data_45_all_sep.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

#now to create one dataframe for each species
RALU_45 <- subset(data_45_all_sep, Species == "RALU")
RASY_45 <- subset(data_45_all_sep, Species == "RASY")
ANBO_45 <- subset(data_45_all_sep, Species == "ANBO")
ABMA_45 <- subset(data_45_all_sep, Species == "ABMA")
ANHE_45 <- subset(data_45_all_sep, Species == "ANHE")
PSMA_45 <- subset(data_45_all_sep, Species == "PSMA")

#write these to their own csv files
write.table(RALU_45, file = "RALU_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(RASY_45, file = "RASY_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ANBO_45, file = "ANBO_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ABMA_45, file = "ABMA_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(ANHE_45, file = "ANHE_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)

write.table(PSMA_45, file = "PSMA_45.csv", sep = ",", col.names = TRUE, row.names=FALSE, append = FALSE)