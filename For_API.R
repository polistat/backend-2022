library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


### Variables

#variables for combining BPI and polls 
#NOT USED HERE
#These variables should also be in kz's simulation code
a <- 1.9
b <- 1.75
c <- 0.05

#Variables for combining undecided variance and goofi variance
und_a <- 1.9
und_b <- 1.75
und_c <- 0.05

#Weight chosen by class for the polls weighed by date
w <- 0.05

#Day the polls were collected
polls_downloaded_date <- "10-7-2022"

#For looping through to get a separate file for the simulation for each day
start_day <- "10/13/2022"
end_date <- "9/1/2022"


####
#Reads csv files for the polls 
senate_polls_down <- read_csv(eval(paste("~/R Stuff/PoliStat/Polls/", polls_downloaded_date, "/senate_polls.csv", sep = ""))) %>% 
    filter(stage == "general" | stage == "jungle primary")
governor_polls_down <- read_csv(eval(paste("~/R Stuff/PoliStat/Polls/", polls_downloaded_date, "/governor_polls.csv", sep = "")))  %>% 
  filter(stage == "general")
generic_polls_down <- read_csv(eval(paste("~/R Stuff/PoliStat/polls/", polls_downloaded_date, "/generic_ballot_polls.csv", sep = "")))

#read csv here
Goofi_down <- read_csv("~/R Stuff/PoliStat/Class Model/goofi.csv")
races_down <- read_csv("~/R Stuff/PoliStat/Class Model/races.csv")
state_pos_down <- read_csv("~/R Stuff/PoliStat/Class Model/state_po.csv")
Past_Results_down <- read_csv("~/R Stuff/PoliStat/Class Model/Past_Results_For_BPI.csv")

#################################################

run_day_date <-  as.Date.character(start_day,"%m/%d/%Y")
end_date <- as.Date.character(end_date,"%m/%d/%Y")
today_date <- as.Date.character(start_day,"%m/%d/%Y")


while(today_date >= end_date){
  
print(today_date)

  
################################################################################################
  
  today_char <- as.character(today_date)
  
  today_dash <- gsub("/", "-", today_char)
  
  senate_polls <- senate_polls_down
  governor_polls <- governor_polls_down
  
  #Combines polls into one df
  polls <- rbind(senate_polls, governor_polls) %>% 
    select(state, office_type, end_date, sample_size, population, fte_grade, methodology, candidate_name, party, pct, race_id, poll_id) %>% 
    filter(!is.na(fte_grade) | fte_grade != "F" | fte_grade != "D") %>% 
    filter(poll_id != "78283") %>% 
    mutate(party = ifelse(candidate_name == "Evan McMullin", "DEM", party))
  
  Oklahoma_senate_polls <- polls %>% 
    filter(state == "Oklahoma") %>% 
    filter(office_type == "U.S. Senate")
  
  Alaska_polls <- polls %>% 
    filter(state == "Alaska")
  
  Alaska_senate <- Alaska_polls %>% 
    filter(office_type == "U.S. Senate")
  
  Alaska_governor <- Alaska_polls %>% 
    filter(office_type == "Governor")
  
  Louisiana_polls <- polls %>% 
    filter(state == "Louisiana")
  
  polls <- polls %>% 
    filter(!(state == "Oklahoma" & office_type == "U.S. Senate")) %>% 
    filter(state != "Alaska") %>% 
    filter(state != "Louisiana")
  
  two_party_polls <- polls%>% 
    filter(party == "DEM" | party == "REP") 
  
  pct_sum_per_poll <- two_party_polls %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              two_party_tot = sum(pct)) 
  
  polls <- polls %>% 
    filter(party == "DEM") %>% 
    full_join(pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
    mutate(two_party_sample = (two_party_tot/100)*sample_size)
  
  polls <- polls %>% 
    filter(count == 2)
  
  third_party_pct <- polls %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              undecided = 100 - two_party_tot) 
  
  polls <- polls %>% 
    full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 
  
  
  polls <- polls %>% 
    mutate(two_party_pct = pct/two_party_tot) %>%
    filter(party == "DEM") %>% 
    mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
           standard_deviation = sqrt(variance))
  
  polls <- polls %>% 
    mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
    mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
    filter(days_before > -1) %>% 
    mutate(last_30 = ifelse(days_before < 31, 1, 0))
  
  summary <- polls %>%
    group_by(race_id) %>%
    summarize(number_of_polls = n(),
              sum = sum(abs(date_weight)))
  
  polls <- polls %>% 
    full_join(summary, by = c("race_id")) %>% 
    mutate(real_date_weight = date_weight/sum,
           weighted_poll = real_date_weight*two_party_pct,
           weighted_sd = real_date_weight*standard_deviation,
           weighted_var = real_date_weight*variance,
           weighted_undecided = real_date_weight*undecided)
  
  date_weighted_polls_by_race <- polls %>% 
    group_by(race_id, state, office_type) %>% 
    summarize(weighted_polls = sum(weighted_poll),
              weighted_var = sum(weighted_var),
              weighted_sd = sqrt(weighted_var),
              weighted_undecided = sum(weighted_undecided),
              number_of_polls = sum(number_of_polls)/n(),
              last_30_polls = sum(last_30)) %>% 
    ungroup() %>%
    select(c(-1)) %>% 
    mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))
  
  ### Oklahoma ###
  
  #Oklahoma is weird, its code is here
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    mutate(office_type = ifelse(race_id == 8943 , "Senate 1", "Senate 2"))
  
  Oklahoma_senate_two_party_polls <- Oklahoma_senate_polls %>% 
    filter(party == "DEM" | party == "REP") %>% 
    mutate(office_type = ifelse(race_id == 8943 , "Senate 1", "Senate 2"))
  
  Oklahoma_senate_pct_sum_per_poll <- Oklahoma_senate_two_party_polls %>% 
    group_by(poll_id, state, office_type, race_id) %>% 
    summarize(count = n(),
              two_party_tot = sum(pct)) 
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    filter(party == "DEM") %>% 
    full_join(Oklahoma_senate_pct_sum_per_poll, by = c("poll_id", "state", "office_type", "race_id")) %>% 
    mutate(two_party_sample = (two_party_tot/100)*sample_size)
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    filter(count == 2)
  
  Oklahoma_senate_third_party_pct <- Oklahoma_senate_polls %>% 
    group_by(poll_id, state, office_type, race_id) %>% 
    summarize(count = n(),
              undecided = 100 - two_party_tot) 
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    full_join(Oklahoma_senate_third_party_pct, by = c("poll_id", "state", "office_type", "race_id")) 
  
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    mutate(two_party_pct = pct/two_party_tot) %>%
    filter(party == "DEM") %>% 
    mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
           standard_deviation = sqrt(variance))
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
    mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
    filter(days_before > -1) %>% 
    mutate(last_30 = ifelse(days_before < 31, 1, 0))
  
  Oklahoma_senate_summary <- Oklahoma_senate_polls %>%
    group_by(race_id) %>%
    summarize(number_of_polls = n(),
              sum = sum(abs(date_weight)))
  
  Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
    full_join(Oklahoma_senate_summary, by = c("race_id")) %>% 
    mutate(real_date_weight = date_weight/sum,
           weighted_poll = real_date_weight*two_party_pct,
           weighted_sd = real_date_weight*standard_deviation,
           weighted_var = real_date_weight*variance,
           weighted_undecided = real_date_weight*undecided)
  
  Oklahoma_senate_date_weighted_polls_by_race <- Oklahoma_senate_polls %>% 
    group_by(race_id, state, office_type) %>% 
    summarize(weighted_polls = sum(weighted_poll),
              weighted_var = sum(weighted_var),
              weighted_sd = sqrt(weighted_var),
              weighted_undecided = sum(weighted_undecided),
              number_of_polls = sum(number_of_polls)/n(),
              last_30_polls = sum(last_30)) %>% 
    ungroup() %>%
    select(c(-1)) %>% 
    mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))
  
  ####
  
  ### TEMPORARY SOLUTION
  #Alaska Senate between two REP
  #Alaska Governor any REP vs DEM + IND
  #Louisiana REP vs any DEM
  
  
  ### Alaska Senate ###
  
  Alaska_senate <- Alaska_senate %>% 
    filter(party == "REP") 
  
  Alaska_senate_pct_sum_per_poll <- Alaska_senate %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              rep_tot = sum(pct)/(count/2)) 
  
  Alaska_senate <- Alaska_senate %>% 
    filter(candidate_name == "Lisa Murkowski") %>% 
    full_join(Alaska_senate_pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
    mutate(rep_party_sample = (rep_tot/100)*sample_size/(count/2))
  
  third_party_pct <- Alaska_senate %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              undecided = 100 - rep_tot) 
  
  Alaska_senate <- Alaska_senate %>% 
    full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 
  
  
  Alaska_senate <- Alaska_senate %>% 
    mutate(two_party_pct = pct/rep_tot) %>%
    mutate(variance = two_party_pct*(1 - two_party_pct)/rep_party_sample)
  
  Alaska_senate <- Alaska_senate %>% 
    mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
    mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
    filter(days_before > -1) %>% 
    mutate(last_30 = ifelse(days_before < 31, 1, 0))
  
  summary <- Alaska_senate %>%
    group_by(race_id) %>%
    summarize(number_of_polls = n(),
              sum = sum(abs(date_weight)))
  
  Alaska_senate <- Alaska_senate %>% 
    full_join(summary, by = c("race_id")) %>% 
    mutate(real_date_weight = date_weight/sum,
           weighted_poll = real_date_weight*two_party_pct,
           weighted_var = real_date_weight*variance,
           weighted_undecided = real_date_weight*undecided)
  
  Alaska_senate_date_weighted_polls_by_race <- Alaska_senate %>% 
    group_by(race_id, state, office_type) %>% 
    summarize(weighted_polls = sum(weighted_poll),
              weighted_var = sum(weighted_var),
              weighted_sd = sqrt(weighted_var),
              weighted_undecided = sum(weighted_undecided),
              number_of_polls = sum(number_of_polls)/n(),
              last_30_polls = sum(last_30)) %>% 
    ungroup() %>%
    select(c(-1)) %>% 
    mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

  
  ### Alaska Governor ###
  
  
  
  ### Louisiana ###
  
  ### TEMPORARY SOLUTION
  #Louisiana Incumbent vs any DEM
  
  pct_sum_per_poll <- Louisiana_polls %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              two_party_tot = sum(pct)) 
  
  Louisiana_polls <- Louisiana_polls %>% 
    filter(party == "REP") %>% 
    full_join(pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
    mutate(two_party_sample = (two_party_tot/100)*sample_size)
  
  third_party_pct <- Louisiana_polls %>% 
    group_by(poll_id, state, office_type) %>% 
    summarize(count = n(),
              undecided = 100 - two_party_tot) 
  
  Louisiana_polls <- Louisiana_polls %>% 
    full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 
  
  
  Louisiana_polls <- Louisiana_polls %>% 
    mutate(two_party_pct = pct/two_party_tot) %>%
    mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
           standard_deviation = sqrt(variance))
  
  Louisiana_polls <- Louisiana_polls %>% 
    mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
    mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
    filter(days_before > -1) %>% 
    mutate(last_30 = ifelse(days_before < 31, 1, 0))
  
  summary <- Louisiana_polls %>%
    group_by(race_id) %>%
    summarize(number_of_polls = n(),
              sum = sum(abs(date_weight)))
  
  Louisiana_polls <- Louisiana_polls %>% 
    full_join(summary, by = c("race_id")) %>% 
    mutate(real_date_weight = date_weight/sum,
           weighted_poll = real_date_weight*two_party_pct,
           weighted_sd = real_date_weight*standard_deviation,
           weighted_var = real_date_weight*variance,
           weighted_undecided = real_date_weight*undecided)
  
  Louisiana_date_weighted_polls_by_race <- Louisiana_polls %>% 
    group_by(race_id, state, office_type) %>% 
    summarize(weighted_polls = 1 - sum(weighted_poll),
              weighted_var = sum(weighted_var),
              weighted_sd = sqrt(weighted_var),
              weighted_undecided = sum(weighted_undecided),
              number_of_polls = sum(number_of_polls)/n(),
              last_30_polls = sum(last_30)) %>% 
    ungroup() %>%
    select(c(-1)) %>% 
    mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))
  
  
  
  
  ####
  
  #Weighted polls by date per race
  date_weighted_polls_by_race <- rbind(date_weighted_polls_by_race, Oklahoma_senate_date_weighted_polls_by_race, Alaska_senate_date_weighted_polls_by_race, Louisiana_date_weighted_polls_by_race)
  
  
  #Writes a csv file with the weighted polls
  write_csv(date_weighted_polls_by_race, eval(paste("~/R Stuff/PoliStat/Class Model/Weighted_polls/", today_dash, ".csv", sep = "")))
  
################################################################################################

Weighted_polls <- date_weighted_polls_by_race
Goofi <- Goofi_down
races <- races_down
state_pos <- state_pos_down
Past_Results <- Past_Results_down 

names(state_pos)[names(state_pos) == 'Code'] <- 'state_po'

races <- full_join(races, state_pos, by = c("state_po")) %>% 
  filter(!is.na(office)) %>% 
  select(State, office) %>% 
  mutate(race = "yes")

Weighted_polls <- Weighted_polls %>% 
  full_join(races, by = c("state" = "State", "office_type" = "office")) %>% 
  filter(race == "yes") %>% 
  mutate(last_30_polls = ifelse(is.na(last_30_polls), 0, last_30_polls),
         number_of_polls = ifelse(is.na(number_of_polls), 0, number_of_polls))

### FIX, TEMPORARY SOLUTION
#### SD with no polls
Weighted_polls <- Weighted_polls %>% 
  mutate(weighted_sd = ifelse(is.na(weighted_sd), 0.03, weighted_sd),
         weighted_var = ifelse(is.na(weighted_var), (weighted_sd)^2, weighted_var))

BPI <- Past_Results %>% 
  mutate(BPI = Past_Results) %>% 
  select(state, BPI)

### State_Lean ###
State_lean <- full_join(BPI, Weighted_polls, by = c("state")) %>% 
  filter(race == "yes")

State_lean <- State_lean[, c(1, 3, 2, 4:9)]


### Additional Variance ###
State_lean <-  State_lean %>% 
  left_join(Goofi, by = c("state")) 

State_lean <-  State_lean %>% 
  mutate(BPI = BPI*100,
         weighted_sd = weighted_sd*100,
         weighted_var = weighted_sd^2,
         sd  = sqrt(variance),
         variance = sd^2
  )

State_lean <- State_lean %>% 
  mutate(percent_var_und = (und_a/pi)*atan(und_b*last_30_polls + und_c*number_of_polls),
         und_variance = (2*log(weighted_undecided))^2,
         extra_variance = ifelse(percent_var_und == 0, variance, percent_var_und*und_variance + (1-percent_var_und)*variance),
         extra_sd = sqrt(extra_variance))

State_lean <-  State_lean %>% 
  mutate(new_variance = ifelse(is.na(weighted_var), extra_variance, weighted_var+extra_variance),
         new_sd = sqrt(new_variance)) 

for_python <- State_lean %>% 
  select(state, office_type, BPI, weighted_polls, new_variance, new_sd, number_of_polls, last_30_polls)

names(for_python)[2] <- 'office'
names(for_python)[4] <- 'polls'
names(for_python)[5] <- 'variance'
names(for_python)[6] <- 'standard_deviation'




################################################################################################


generic_polls <- generic_polls_down %>% 
  select(end_date, sample_size, population, fte_grade, methodology, dem, rep, poll_id) %>% 
  filter(!is.na(fte_grade) | fte_grade != "F" | fte_grade != "D") %>% 
  filter(!is.na(sample_size)) %>% 
  mutate(two_party_tot = dem + rep,
         dem_two_party = dem/two_party_tot)

generic_polls <- generic_polls %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(variance = (dem_two_party*(1-dem_two_party))/sample_size,
         standard_deviation = sqrt(variance))

generic_polls <- generic_polls %>% 
  mutate(real_date_weight = date_weight/sum(generic_polls$date_weight),
         weighted_poll = real_date_weight*dem_two_party,
         weighted_var = real_date_weight*variance,
         yes = "yes")

center_and_variance <- generic_polls %>% 
  group_by(yes) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var)) %>% 
  ungroup() %>%
  select(c(-1))

################################################################################################


#Output

#Writes a csv file for each day
write_csv(for_python, eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/", today_dash, ".csv", sep = "")))

#Writes a csv file with the center and variance for national mood
write_csv(center_and_variance, eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/National_Mood/", today_dash, "_National_Mood.csv", sep = "")))

today_date <- today_date - as.difftime(1, unit="days")

}
