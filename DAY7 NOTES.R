library(tidyverse)
library(palmerpenguins)
library(dplyr)

#Starting from penguins, write a single piped sequence in which you:
#Only include penguins at Dream and Biscoe Islands

# penguins %>% filter(island == "Dream" | island == "Biscoe") write in a better way
penguins %>% filter(island %in% c("Dream","Biscoe")) %>% 
  select(-year,-sex) %>% 
  mutate(body_mass_kg = body_mass_g/1000) %>% 

#2. Starting from penguins, write a single piped sequence to:
  #limit to adelie penguins
  #remove any observations where flipper_length_mm is NA(!is.na()) or drop_na
  # group the data by sex
  #create a summary table that containe the mean, sd, and sample size of flipper length for male and female adelie penguins
  
penguins %>% filter(species == "Adelie", !is.na(flipper_length_mm)) %>% 
  group_by(sex) %>% 
  summarize(mean_length = mean(flipper_length_mm),
            sd_length = sd(flipper_length_mm))


penguins %>% filter(species == "Adelie") %>% 
  drop_na(flipper_length_mm, sex) %>% 
  group_by(sex) %>% 
  summarize(mean_length = mean(flipper_length_mm),
            sd_length = sd(flipper_length_mm),
            sample_size =n())


### These two are equivelant
penguins %>% group_by(species, island, sex) %>% 
  summarize(sample_size = n())

# Count is just wrapping togetehr group_by + summarize +n() to get counts of observation. Use this when you just want to know how many there are and not really grouping
penguins %>% count(species,island,sex)
