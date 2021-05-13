#same as import script except pointing to the southern edge only folders
#note that writing the output files has been ## out for now

library(plyr)
library(tidyverse)

#Going to create full dataframe for 8.5 and 4.5 and then split the excel documents up to species specific to calculate out the sensitivity
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/New SRange/New Buffer Work/8.5")
#create list of files
list_85_S = list.files(pattern = "*summary.txt", 
                     full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_S_85 <- ldply(list_85_S, read.table, stringsAsFactors = FALSE, sep = "\t")


#remove  rows to get rid of repeating headers

data_S_85_cleaned <- subset(data_S_85, data_S_85$V1 != "simulName")

#looks like ldply resulted in duplication?

data_S_85_cleaned2 <- unique(data_S_85_cleaned)


#add column names back in

names(data_S_85_cleaned2) = c("Name", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column, then split the rest of the name to give the category of the run type

data_S_85_cleaned_sep <- separate(data = data_S_85_cleaned2, col = Name, into = c('Species', 'Rest'), sep = "_")

data_S_85_final <- data_S_85_cleaned_sep %>% separate(Rest, into = c('Type', 'Run_Num'), sep = 4)

data_S_85_final$Scenario <- c("8.5")

summary_85_S <- data_S_85_final %>%
  group_by(Species, Type) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))





##############4.5
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/New SRange/New Buffer Work/4.5/Aligned/ASC")
#create list of files
list_45_S = list.files(pattern = "*summary.txt", 
                     full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

#create data frame
data_S_45 <- ldply(list_45_S, read.table, stringsAsFactors = FALSE, sep = "\t")


#remove  rows to get rid of repeating headers

data_S_45_cleaned <- subset(data_S_45, data_S_45$V1 != "simulName")

#looks like ldply resulted in duplication?

data_S_45_cleaned2 <- unique(data_S_45_cleaned)



#add column names back in

names(data_S_45_cleaned2) = c("Name", "iniCount",	"noDispCount",	"univDispCount",	"occupiedCount",	"absentCount",	"totColonized",	"totDecolonized",	"totLDDsuccess",	"runTime")

#split species to its own column, then split the rest of the name to give the category of the run type

data_S_45_cleaned_sep <- separate(data = data_S_45_cleaned2, col = Name, into = c('Species', 'Rest'), sep = "_")

data_S_45_final <- data_S_45_cleaned_sep %>% separate(Rest, into = c('Type', 'Run_Num'), sep = 4)

data_S_45_final$Scenario <- c("4.5")

summary_45_S <- data_S_45_final %>%
  group_by(Species, Type) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))


###########Compare 8.5 to 4.5 means

data_S_all <- bind_rows(data_S_45_final, data_S_85_final)


summary_all <- data_S_all %>%
  group_by(Species, Type, Scenario) %>%
  summarize(average = mean(as.numeric(occupiedCount)), stdev = sd(as.numeric(occupiedCount)))

#ggplot(data=data_S_all, aes(x=Type, y=as.numeric(occupiedCount))) +
#  geom_boxplot(aes(color = Species)) +
#  theme_classic()


#ggplot(data=data_S_all, aes(x=Species, y=as.numeric(occupiedCount))) +
#  geom_boxplot(aes(color = Scenario)) +
#  theme_classic()

#ggplot(data=data_S_all, aes(x=Scenario, y=occupiedCount)) +
#  geom_boxplot(aes(color = Species)) +
#  theme_classic()


####okay enough visualization, time to calculate the sensitivty of each one
#so, need the parameter values down the column
#write a csv just so that I can see what the values need to be for entering in
setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/R_Outputs")
#write.csv(data_S_all, "data_all_south.csv")

data_S_all_param <- data_S_all

data_S_all_param$Param <- c('0','0','0','0','0','0', #base
                          
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

data_S_all_param$Param <- as.numeric(data_S_all_param$Param)

data_S_all_param$occupiedCount <- as.numeric(data_S_all_param$occupiedCount)

sens1_S <- data_S_all_param %>%
  group_by(Species, Type, Scenario) %>%
  mutate(minP = min(Param)) %>%
  mutate(minOcc = min(occupiedCount))


#now that the minimum is present for each based on the species, type, and scenario, we can create columns to do the parts of the calculation, which we will then summarize based on the species, type, and scenario

#change in occupiedCount
sens1_S$occChange <- sens1_S$occupiedCount - sens1_S$minOcc

#change in parameter
sens1_S$paramChange <- sens1_S$Param - sens1_S$minP

#proportional change of occupied compared to parameter, absoulte value

sens1_S$sens <- sens1_S$occChange / sens1_S$paramChange

# multiple to get rid of the units and calculate sensitivity

sens1_S$elast <- sens1_S$sens * (sens1_S$Param / sens1_S$minOcc)

#need to remove inf, NAs and NAN, keeping intermediate files as this is proving to be a bit challenging trying to piece together

sens2_S <- sens1_S[is.finite(sens1_S$elast),]

sens3_S <- sens2_S %>%
  group_by(Species,Scenario,Type) %>% #deleted type, so will compare across
  summarize(avgE = mean(elast), stdE = sd(elast))

sens4_S <- sens3_S %>% 
  arrange(desc(avgE)) %>% 
  group_by(Species,Scenario)%>% 
  mutate(rank=row_number())

sens5_S <- sens4_S[order(sens4_S$Scenario, sens4_S$Species, sens4_S$rank),]

#spread to make a happy table to read

sens6_S <- subset(sens5_S, sens5_S$rank <= 3)

#remove data values

sens7_S <- dplyr::select(sens6_S,-c(avgE,stdE)) #sometimes need to specify dplyer to avoid a namespace glitch

sens8_S <- sens7_S %>%
  spread(Type, rank) 


#write.csv(sens8_S, "Rank_table_south.csv")

