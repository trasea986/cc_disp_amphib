#this script will import all of the summary files, and then calculate changes in the range for each species
library(tidyverse)

#note that the outputs of migclim show up in the working directory they are run, so they need to be manually (or use bash/cmd) moved into a new directory, called "migclim".

setwd("./outputs/migclim")

#create dataframe from list of files
#n_max is used to avoid bringing in the line averaging across replicates. adjusted value to match replicate number

#give list of column types so that every time read_tsv finishes, it doesn't drop a message in the console summarizing those types unless there is a problem
col_types <- cols(
  simulName = col_character(),
  iniCount = col_double(),
  noDispCount = col_double(),
  univDispCount = col_double(),
  occupiedCount = col_double(),
  absentCount = col_double(),
  totColonized = col_double(),
  totDecolonized = col_double(),
  totLDDsuccess = col_double(),
  runTime = col_double()
)

#for making the dataframe, first read in all summary.txt files. Then, need to also remove larger files, which are those under 450 bytes. The reason for this is that each directory will have a summary text that has one row for each of the replicate summary outputs in addition to the summary file for individual replicate run. We don't want to read in those 6 line files and only want each individual 2 line file (header + data) which are automatically made in each directory.
df_migclim = list.files(pattern = paste("*summary.txt", sep=''), 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE)  %>%
  discard(function(x) file.size(x)>400) %>%
  map_df(function(x) read_tsv(x, col_types = col_types) %>% mutate(filename=paste(dirname((x)),basename(x),sep="/")))


#use filename to get the ssp that the simulation came from
#then break up simulation name so each row has an appropriate species and category for sensivity analysis
#note that this will work even if renaming files after running migclim (such as adding prefix)... all wrangling uses filename and not migclim simulation name
df_migclim <- df_migclim %>%
  separate(col = filename, 
           into = c('junk1', 'SSP', 'junk2', 'run'), sep = "/")

df_migclim$junk1 <- NULL
df_migclim$junk2 <- NULL

#now we break up the simulation name to full vs. south range, species, and runinformation
df_migclim <- df_migclim %>%
  separate(col = run, 
           into = c('range', 'species', 'runinfo', "junk1"), sep = "_")

df_migclim_test$junk1 <- NULL

#need to break up simulation name by the text name and the value used after pulling species (and deleting species, which was created above)
df_migclim <- df_migclim %>%
  separate(col = runinfo, 
           into = c("category", "run"), 
           sep = "(?<=[A-Za-z])(?=[0-9])")
  
#last, split run by value and replicate. this would be trickier if doing more than 9 replicates...
df_migclim <- df_migclim %>%
  separate(col = run, 
           into = c("value", "replicate"), 
           sep = -1)


#next we will calculate the proportional change across all dispersal models and the number of meters that are occupied at the end of the simulation. this also ranks species by most and least affected.
#to get the area instead of number of cells, multiple by 576.9299 which is number of meters per cell
#this is for table 2
#note that initial count, unlimited dispersal count, and no dispersal counts do not vary with run, but calculating mean and things here so that grouping in final table remains
prop <- df_migclim %>%
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