#this script will import all of the summary files, and then calculate changes in the range for each species
library(tidyverse)

#note that the outputs of migclim show up in the working directory they are run, so they need to be manually (or use bash/cmd) moved into a new directory, called "migclim".

setwd("./outputs/migclim")

#create dataframe from list of files
#n_max is used to avoid bringing in the line averaging across replicates. adjusted value to match replicate number

df_migclim = list.files(pattern = paste("*summary.txt", sep=''), 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE) %>% 
  map_df(function(x) read_tsv(x, n_max = 5) %>% mutate(filename=paste(dirname((x)),basename(x),sep="/")))

#use filename to get the ssp that the simulation came from
#then break up simulation name so each row has an appropriate species and category for sensivity analysis
df_migclim <- separate(data = df_migclim, col = filename, into = c('junk1', 'SSP', 'junk2', 'junk3'), sep = "/")

df_migclim$junk1 <- NULL
df_migclim$junk2 <- NULL
df_migclim$junk3 <- NULL

#now we break up the simulation name to full vs. south range, species, and runinformation
df_migclim_test <- df_migclim %>%
  separate(col = simulName, 
           into = c('range', 'species', 'runinfo'), sep = "_")

#need to break up run information by the text name and the value used
df_migclim_test <- df_migclim_test %>%
  separate(col = runinfo, 
           into = c("category", "run"), 
           sep = "(?<=[A-Za-z])(?=[0-9])")

#last, split run by value and replicate. this would be trickier if doing more than 9 replicates...
df_migclim_test <- df_migclim_test %>%
  separate(col = run, 
           into = c("value", "replicate"), 
           sep = -1)


#next we will calculate the proportional change across all dispersal models and the number of meters that are occupied at the end of the simulation. this also ranks species by most and least affected.
#to get the area instead of number of cells, multiple by 576.9299 which is number of meters per cell
#this is for table 2
#note that initial count, unlimited dispersal count, and no dispersal counts do not vary with run, but calculating mean and things here so that grouping in final table remains
prop <- df_migclim_test %>%
  group_by(species, SSP, range) %>%
  summarize(ini_km = (mean(iniCount) * 576.9299/1000),
            zero_prop = ((min(noDispCount)-mean(iniCount))/mean(iniCount)),
            unlim_prop = ((max(univDispCount)-mean(iniCount))/mean(iniCount)),
            avg_disp_prop = ((mean(occupiedCount)-mean(iniCount))/mean(iniCount)),
            min_disp_prop = ((min(occupiedCount)-mean(iniCount))/mean(iniCount)),
            max_disp_prop = ((max(occupiedCount)-mean(iniCount))/mean(iniCount))
  ) %>%
  ungroup() %>% 
  arrange(range, desc(SSP))

#these full table
write.csv(prop, "table3_raw.csv")

#next is to calculate the average by SSP for all species, which is added to the table in excel when doing final formatting

all_sp <- prop %>%
  group_by(SSP, range) %>%
  summarise(across(ini_km:max_disp_prop, ~ mean(.x, na.rm = TRUE)))

write.csv(prop_average, "table3_all_sp_avg.csv")