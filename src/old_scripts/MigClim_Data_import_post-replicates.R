#R code for importing multiple text files, so that I can then break up and calculate the proportional shifts easier
#note that writing the output files has been ## out for now

library(plyr)
library(tidyverse)

#Going to create full dataframe for 8.5 and 4.5 and then split the excel documents up to species specific to calculate out the sensitivity
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/85_cc")
#create list of files
list_85 = list.files(pattern = "*summary.txt", 
                full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_85 <- ldply(list_85, read.table, stringsAsFactors = FALSE, sep = "\t")


#remove  rows to get rid of repeating headers

data_85_cleaned <- subset(data_85, data_85$V1 != "simulName")

#looks like ldply resulted in duplication?

data_85_cleaned2 <- unique(data_85_cleaned)


#add column names back in

names(data_85_cleaned2) = c("Name", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column, then split the rest of the name to give the category of the run type

data_85_cleaned_sep <- separate(data = data_85_cleaned2, col = Name, into = c('Species', 'Rest'), sep = "_")

data_85_final <- data_85_cleaned_sep %>% separate(Rest, into = c('Type', 'Run_Num'), sep = 4)

data_85_final$Scenario <- c("8.5")

summary_85 <- data_85_final %>%
  group_by(Species, Type) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))





##############4.5
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/45_cc")
#create list of files
list_45 = list.files(pattern = "*summary.txt", 
                     full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_45 <- ldply(list_45, read.table, stringsAsFactors = FALSE, sep = "\t")


#remove  rows to get rid of repeating headers

data_45_cleaned <- subset(data_45, data_45$V1 != "simulName")

#looks like ldply resulted in duplication?

data_45_cleaned2 <- unique(data_45_cleaned)



#add column names back in

names(data_45_cleaned2) = c("Name", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column, then split the rest of the name to give the category of the run type

data_45_cleaned_sep <- separate(data = data_45_cleaned2, col = Name, into = c('Species', 'Rest'), sep = "_")

data_45_final <- data_45_cleaned_sep %>% separate(Rest, into = c('Type', 'Run_Num'), sep = 4)

data_45_final$Scenario <- c("4.5")

summary_45 <- data_45_final %>%
  group_by(Species, Type) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))


###########Compare 8.5 to 4.5 means

data_all <- bind_rows(data_45_final, data_85_final)


summary_all <- data_all %>%
  group_by(Species, Type, Scenario) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))

ggplot(data=data_all, aes(x=Type, y=as.numeric(occupiedCount))) +
  geom_boxplot(aes(color = Species)) +
  theme_classic()


ggplot(data=data_all, aes(x=Species, y=as.numeric(occupiedCount))) +
  geom_boxplot(aes(color = Scenario)) +
  theme_classic()

ggplot(data=data_all, aes(x=Scenario, y=occupiedCount)) +
  geom_boxplot(aes(color = Species)) +
  theme_classic()


####okay enough visualization, time to calculate the sensitivty of each one
#so, need the parameter values down the column
#write a csv just so that I can see what the values need to be for entering in
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/R_Outputs")
#write.csv(data_all, "data_all.csv")

data_all_param <- data_all

data_all_param$Param <- c('0','0','0','0','0','0', #base
                    
                    '2','2','2','2','2','2', #age
                    '3','3','3','3','3','3',
                    '4','4','4','4','4','4',
                    '5','5','5','5','5','5',
                    '6','6','6','6','6','6',
                    
                    '12','12','12','12','12','12', #distance
                    '2','2','2','2','2','2', 
                    '22','22','22','22','22','22',
                    '3','3','3','3','3','3',
                    '6','6','6','6','6','6',
                    '9','9','9','9','9','9',

                    
                    '10','10','10','10','10','10',
                    '2','2','2','2','2','2', #ldddist
                    '3','3','3','3','3','3',
                    '5','5','5','5','5','5',

                    
                    '.05','.05','.05','.05','.05','.05', #ldd rate
                    '.10','.10','.10','.10','.10','.10',
                    '.15','.15','.15','.15','.15','.15',
                    '.20','.20','.20','.20','.20','.20',
                    '.25','.25','.25','.25','.25','.25',
                    
                    '1','1','1','1','1','1', #rate
                    '.2','.2','.2','.2','.2','.2',
                    '.4','.4','.4','.4','.4','.4',
                    '.6','.6','.6','.6','.6','.6',
                    '.8','.8','.8','.8','.8','.8',
                    
                    '.5','.5','.5','.5','.5','.5', #shape, note .6 was dropped as there was an error in code...
                    '.7','.7','.7','.7','.7','.7',
                    '.8','.8','.8','.8','.8','.8'
                    )

###now that parameter value is in there, can use group_by to calculate the sensitivity

data_all_param$Param <- as.numeric(data_all_param$Param)

data_all_param$occupiedCount <- as.numeric(data_all_param$occupiedCount)  

sens1 <- data_all_param %>%
  group_by(Species, Type, Scenario) %>%
  mutate(minP = min(Param)) %>%
  mutate(minOcc = min(occupiedCount))
  
  
#now that the minimum is present for each based on the species, type, and scenario, we can create columns to do the parts of the calculation, which we will then summarize based on the species, type, and scenario

#change in occupiedCount

sens1$occChange <- sens1$occupiedCount - sens1$minOcc

#change in parameter
sens1$paramChange <- sens1$Param - sens1$minP

#proportional change of occupied compared to parameter, absoulte value

sens1$sens <- sens1$occChange / sens1$paramChange

# multiple to get rid of the units and calculate sensitivity

sens1$elast <- sens1$sens * (sens1$Param / sens1$minOcc)

#need to remove inf, NAs and NAN, keeping intermediate files as this is proving to be a bit challenging trying to piece together

sens2 <- sens1[is.finite(sens1$elast),]

sens3 <- sens2 %>%
  group_by(Species,Scenario,Type) %>% #deleted type, so will compare across
  summarize(avgE = mean(elast), stdE = sd(elast))

sens4 <- sens3 %>% 
  arrange(desc(avgE)) %>% 
  group_by(Species,Scenario)%>% 
  mutate(rank=row_number())

sens5 <- sens4[order(sens4$Scenario, sens4$Species, sens4$rank),]

#spread to make a happy table to read

sens6 <- subset(sens5, sens5$rank <= 3)

#what was the worse?
sens_lowest <- subset(sens5, sens5$rank == 5)

#remove data values

sens7 <- dplyr::select(sens6,-c(avgE,stdE))

sens8 <- sens7 %>%
  spread(Type, rank) 


#write.csv(sens8, "Rank_table_full.csv")

#next, need average propoportional change for all dispersal models

average_disp_change <- summary_all %>% 
  group_by(Species, Scenario) %>% 
  summarize(average_average = mean(average))

#adding ini manually based on the data_all_param object
average_disp_change$ini <- c(1865278,1865278,4611047,4611047,1693749,1693749,8364215,8364215,2231054,2231054,6670342,6670342) 

average_disp_change$prop <- (average_disp_change$average_average-average_disp_change$ini)/average_disp_change$ini
