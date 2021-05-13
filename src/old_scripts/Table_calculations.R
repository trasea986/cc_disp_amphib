#The other code get ranks, but this script will be for the table calculations

#Proportional area change for full and southern range, and then rank the species

data_all$occupiedCount <- as.numeric(data_all$occupiedCount)
data_all$iniCount <- as.numeric(data_all$iniCount)
data_all$noDispCount <- as.numeric(data_all$noDispCount)
data_all$univDispCount <- as.numeric(data_all$univDispCount)

data_S_all$occupiedCount <- as.numeric(data_S_all$occupiedCount)
data_S_all$iniCount <- as.numeric(data_S_all$iniCount)
data_S_all$noDispCount <- as.numeric(data_S_all$noDispCount)
data_S_all$univDispCount <- as.numeric(data_S_all$univDispCount)

prop_table <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((mean(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))


prop_table_S <- data_S_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((mean(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number()) %>%
  arrange(desc(Scenario))

#write.csv(prop_table_S, "Southern_Prop_T3.csv")
#write.csv(prop_table, "Prop_T3.csv")

#note that table 3 is in table 2, except it also has ranks

#Proportional area change between initial and final when...

#average of all dispersal models which is from above

#minimum and max from dispersal models

min_table <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

max_table <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((max(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

min_south <- data_S_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

max_south <- data_S_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(occupiedCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

#write.csv(max_table, "max_table.csv")
#write.csv(min_table, "min_table.csv")

#write.csv(max_south, "Souther_max_table.csv")
#write.csv(min_south, "Souther_min_table.csv")

#although didn't change, calculating the proportional change for...

#zero dispersal and unlimited

zero <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(noDispCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

unlim <- unlim_disp <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(univDispCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

zero_S <- data_S_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(noDispCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

unlim_S <- unlim_S_disp <- data_all %>%
  group_by(Species, Scenario) %>%
  summarize(prop_area = ((min(univDispCount)-mean(iniCount))/mean(iniCount)))%>% 
  ungroup() %>% 
  arrange(desc(prop_area)) %>%
  group_by(Scenario)%>% 
  mutate(rank=row_number())%>%
  arrange(desc(Scenario))

#write.csv(zero, "zero_table.csv")
#write.csv(unlim, "unlim_table.csv")
#write.csv(zero_S, "Souther_zero_table.csv")
#write.csv(unlim_S, "Souther_unlim_table.csv")
