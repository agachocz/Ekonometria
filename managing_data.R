library(tidyverse)
library(plyr)

data_files <- list.files(path = "data", full.names = TRUE)

data_list <- lapply(data_files, function(file){
  read.csv(file)
})

# Some data are noted daily, some annually

daily_data <- join_all(data_list[1:3], by = c("Entity", "Code", "Day"), type = 'full')
annual_data <- join_all(data_list[c(4:5, 7:14)], by = c("Entity", "Code", "Year"), type = 'full')

covid_data <- data_list[[6]] %>% mutate(Year = str_sub(date, 1, 4)) %>%
  select(Entity = location, Year, Deaths = total_deaths) %>%
  filter(Year == "2020" | Year == "2021") %>%
  mutate(Entity_Year = paste(Entity, Year)) %>%
  group_by(Entity_Year) %>%
  filter(!is.na(Deaths)) %>%
  arrange(Deaths) %>% do(tail(., 1))

annual_data <- annual_data %>% select(-Code) %>% filter(Year <= 2021)

colnames(annual_data) <- c('Entity', 'Year', 'Gini', 'Median_age', 'PISA_science', 
                           'Total_pop', 'Pop_fertility', 'Trust_others',
                           'Health_public_exp', 'Tertiary_edu', 'Trust_gov', 'Health_exp',
                           'Deaths1', 'Deaths2', 'Deaths3')


# find the most recent available data for each country
Gini <- annual_data %>% select(Entity, Year, Gini) %>% group_by(Entity, Year) %>%
  filter(!is.na(Gini)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Median_age <- annual_data %>% select(Entity, Year, Median_age) %>% group_by(Entity, Year) %>%
  filter(!is.na(Median_age)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

PISA_science <- annual_data %>% select(Entity, Year, PISA_science) %>% group_by(Entity, Year) %>%
  filter(!is.na(PISA_science)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Trust_others <- annual_data %>% select(Entity, Year, Trust_others) %>% group_by(Entity, Year) %>%
  filter(!is.na(Trust_others)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Health_public_exp <- annual_data %>% select(Entity, Year, Health_public_exp) %>% group_by(Entity, Year) %>%
  filter(!is.na(Health_public_exp)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Tertiary_edu <- annual_data %>% select(Entity, Year, Tertiary_edu) %>% group_by(Entity, Year) %>%
  filter(!is.na(Tertiary_edu)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Trust_gov <- annual_data %>% select(Entity, Year, Trust_gov) %>% group_by(Entity, Year) %>%
  filter(!is.na(Trust_gov)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Health_exp <- annual_data %>% select(Entity, Year, Health_exp) %>% group_by(Entity, Year) %>%
  filter(!is.na(Health_exp)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Total_pop <- annual_data %>% select(Entity, Year, Total_pop) %>% group_by(Entity, Year) %>%
  filter(!is.na(Total_pop)) %>% arrange(Year) %>% group_by(Entity) %>% do(tail(., 1)) %>% select(-Year)

Deaths <- annual_data %>% select(Entity, Year, Deaths1, Deaths2, Deaths3) %>% 
  mutate(Deaths = Deaths1 + Deaths2 + Deaths3) %>% select(Entity, Year, Deaths) %>%
  filter(Year > 2015) %>% filter(!is.na(Deaths)) %>% group_by(Entity) %>%
  dplyr::summarise(Hist_Deaths = mean(Deaths)) %>% select(Entity, Hist_Deaths)


# merge annual data

annual_data_merge <- join_all(list(Gini, Health_exp, Health_public_exp, Median_age, PISA_science, Tertiary_edu,
                                   Trust_gov, Trust_others), by = c("Entity")) %>%
  filter_all(all_vars(!is.na(.)))


# daily data

daily_data <- daily_data %>% select(-Code) %>% rename(Excess_mortality = cum_excess_proj_all_ages) 
plot(daily_data$Excess_mortality)

daily_tranformed <- daily_data %>%
  mutate(Year = str_sub(Day, 1, 4)) %>% filter(Year == "2020" | Year == "2021") %>%
  group_by(Entity, Year) %>%
  summarise(
    Containment_mean = mean(containment_index, na.rm = T),
    Containment_sd = sd(containment_index, na.rm = T),
    Stringency_mean = mean(stringency_index, na.rm = T),
    Stringency_sd = sd(stringency_index, na.rm = T),
    Excess_mortality = sum(Excess_mortality, na.rm = T)
  )

data_all <- full_join(annual_data_merge, daily_tranformed, by = c('Entity'))
