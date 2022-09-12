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

df_migclim$junk1 <- NULL

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
  summarize(ini_km = (mean(iniCount) * (576.9299/1000)),
            zero_prop = ((min(noDispCount)-mean(iniCount))/mean(iniCount)),
            unlim_prop = ((max(univDispCount)-mean(iniCount))/mean(iniCount)),
            avg_disp_prop = ((mean(occupiedCount)-mean(iniCount))/mean(iniCount)),
            min_disp_prop = ((min(occupiedCount)-mean(iniCount))/mean(iniCount)),
            max_disp_prop = ((max(occupiedCount)-mean(iniCount))/mean(iniCount))
  ) %>%
  ungroup() %>% 
  arrange(range, desc(SSP))

#these full table
#write.csv(prop, "table3_raw.csv")

#next is to calculate the average by SSP for all species, which is added to the table in excel when doing final formatting

all_sp <- prop %>%
  group_by(SSP, range) %>%
  summarise(across(ini_km:max_disp_prop, ~ mean(.x, na.rm = TRUE)))

#write.csv(all_sp, "table3_all_sp_avg.csv")

#one visualization bit, the occupied means and stdev across the three climate change scenarios

df_migclim_plot <- df_migclim

df_migclim_plot$SSP <- as.factor(df_migclim_plot$SSP)
df_migclim_plot$SSP <- recode_factor(df_migclim_plot$SSP, ssp245 = "SSP245",
                                ssp370 = 'SSP370',
                                ssp585 = "SSP585")

df_migclim_plot$range <- as.factor(df_migclim_plot$range)
df_migclim_plot$range <- recode_factor(df_migclim_plot$range, full = "Full Range", 
                                     south = "Only Southern Range")

df_migclim_plot$species <- as.factor(df_migclim_plot$species)
df_migclim_plot$species <- recode_factor(df_migclim_plot$species, ABMA = 'Ambystoma macrodactylum', ANBO = 'Anaxyrus boreas', ANHE = 'Anaxyrus hemiophrys', PSMA = 'Pseudacris maculata', LISY = 'Lithobates sylvaticus', RALU = 'Rana luteiventris')


spp_ranges <- 
  ggplot(data=df_migclim_plot, aes(x=species, y=as.numeric(occupiedCount * 576.9299/1000))) +
  geom_boxplot(aes(color = SSP)) +
  scale_colour_manual(values = c("blue3", "green4", "grey35")) +
  facet_wrap(~range)+ #, scales = "free") + if wanting different scales
  xlab("Species")+
  ylab("Occupied Kilometers") +
  scale_y_continuous(labels = scales::comma)+
  theme_classic(base_size = 14)+
  theme(axis.text.x = element_text(angle = 90, face = 'italic', hjust=0.95,vjust=0.2))

ggsave("./SSP_range_size.png", plot = spp_ranges, 
       width = 12, height = 9, dpi = 600)

#next up is the actual sensitivity analysis
#need to create the rows that will represent the base model
sens1 <- df_migclim %>%
  filter(category == "base")

#next, replicate the sens1 df multiple times, will a set of five rows (the replicates) for each of the categories.
#then, replace category and value with the base information
#as a reminder, this is the migclim base model:

#dispersal kernal, rate = 0.1
#dispersa kernal, distance = 5 (100 m increments, e.g. "30" value is 6 cells, "10" is 2 cells)
#iniMatAge=1, 
#lddFreq=0.05, commented out/0 in base model
#lddMaxDist=4, commented out/0 in base model

#will need to do this for each category: age     base distance  ldddist  lddrate     rate 
age <- sens1
age$category <- c('age')
age$value <- c('1')

distance <- sens1
distance$category <- c('distance')
distance$value <- c('5')

ldddist <- sens1
ldddist$category <- c('ldddist')
ldddist$value <- c('0')

lddrate <- sens1
lddrate$category <- c('lddrate')
lddrate$value <- c('0')

rate <- sens1
rate$category <- c('rate')
rate$value <- c('0.1')

#create df without the base bits, then row bind all together
sens2 <- df_migclim %>%
  filter(category != "base")
sensitivity_df <- rbind(sens2, age, distance, ldddist, lddrate, rate)

#define structure of chr columns
sensitivity_df$SSP <- as.factor(sensitivity_df$SSP)
sensitivity_df$category <- as.factor(sensitivity_df$category)
sensitivity_df$range <- as.factor(sensitivity_df$range)
sensitivity_df$value <- as.numeric(sensitivity_df$value)

#next, calculate the mean across replicates
sensitivity_df <- sensitivity_df %>%
  group_by(species, SSP, category, range, value) %>%
  summarise(mean_occ = mean(occupiedCount, na.rm = T))

#determine minimum value for the category
#note that all of parameters decrease the occuped cells except age, so age needs to be calculated sep and brought back
sensitivity_df1 <- sensitivity_df %>%
  filter(category != 'age') %>%
  group_by(species, SSP, category, range) %>%
  mutate(min_value = min(value)) %>% #determine the minimum value
  mutate(min_occ = min(mean_occ))

sensitivity_df2 <- sensitivity_df %>%
  filter(category == 'age') %>%
  group_by(species, SSP, category, range) %>%
  mutate(min_value = max(value)) %>% #determine the minimum value
  mutate(min_occ = min(mean_occ))

#change in occupiedCount
sensitivity_df1$occ_change <- sensitivity_df1$mean_occ - sensitivity_df1$min_occ
sensitivity_df2$occ_change <- sensitivity_df2$mean_occ - sensitivity_df2$min_occ

#change in parameter
sensitivity_df1$param_change <- as.numeric(sensitivity_df1$value) - as.numeric(sensitivity_df1$min_value)
sensitivity_df2$param_change <- as.numeric(sensitivity_df2$value) - as.numeric(sensitivity_df2$min_value)

#proportional change of occupied compared to parameter, absolute value
sensitivity_df1$sens <- abs(sensitivity_df1$occ_change / sensitivity_df1$param_change)
sensitivity_df2$sens <- abs(sensitivity_df2$occ_change / sensitivity_df2$param_change)

# multiply to get rid of the units and calculate proportional change. original param value / original occ value.
sensitivity_df1$elast <- abs(sensitivity_df1$sens * (sensitivity_df1$value / sensitivity_df1$min_occ))
sensitivity_df2$elast <- abs(sensitivity_df2$sens * (sensitivity_df2$value / sensitivity_df2$min_occ))

#need to remove NAN after combining age and other calculations, keeping intermediate files as this is proving to be a bit challenging trying to piece together. this is the elasticity value of the min parameter
sensitivity_df <- rbind(sensitivity_df1, sensitivity_df2)
sensitivity_df <- sensitivity_df[is.finite(sensitivity_df$sens),] #removes "base" model (NaN)

sensitivity_summary <- sensitivity_df %>%
  group_by(species, SSP, category, range) %>%
  summarize(avg_elast = mean(elast), std_elast = sd(elast))

sensitivity_ranked <- sensitivity_summary %>% 
  arrange(desc(avg_elast)) %>% 
  group_by(species, SSP, range)%>% 
  mutate(rank=row_number()) %>%
  arrange(range, SSP, species)

#order by ranking for full table
sensitivity_ordered <- sensitivity_ranked[order(sensitivity_ranked$SSP, sensitivity_ranked$species, sensitivity_ranked$rank),]

#write.csv(sensitivity_ordered, '../sens_ranked_ordered_supp_table4.csv')

#determine top 3
sensitivity_table <- sensitivity_ordered %>%
  select(-c(avg_elast, std_elast)) %>%
  spread(category, rank) %>%
  arrange(range, SSP)
  
#write.csv(sensitivity_table, '../table4_sens_rank_fixed_lisy.csv')

#helpful code for troubleshooting the missing LISY 585 south, probably due to zeroes. below returns just these data
sensitivity_df %>%
  filter(species == 'LISY' & SSP == 'ssp585' & range == 'south')

sensitivity_df %>%
  filter(species == 'PSMA' & SSP == 'ssp585' & range == 'south')
