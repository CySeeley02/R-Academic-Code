
nyc_squirrels <- as.data.frame(readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv"))
View(nyc_squirrels)
colnames(nyc_squirrels)
###############################################################################
############################# Necessary Packages ##############################
###############################################################################

install.packages("ggplot2")
library(ggplot2)
install.packages("g.data")
library(g.data)
install.packages("reshape2")
library(reshape2)
install.packages("tidyverse")
library(tidyverse)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("ggmap")
library(ggmap)
install.packages("openintro")
library(openintro)
install.packages("gdata")
library(gdata)
install.packages("readr")
library(readr)
install.packages("zipcodeR")
library(zipcodeR) 
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("moments")
library(moments)
install.packages("readxl")
library(readxl)
install.packages("RODBC")
library(RODBC)
install.packages("sqldf")
library(sqldf)
install.packages("RCurl")
library(RCurl)
install.packages("jsonlite")
library(jsonlite)


###############################################################################
############################# Business Questions ##############################
###############################################################################
# What code is used to get answers from the data? #############################
# What industry or populus benefits from this data and how? ###################
###############################################################################

###  1)	What are the most prominent areas where squirrels are sighted?
sightings_by_location <- nyc_squirrels %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(sightings_by_location, aes(x = reorder(location, -count), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Squirrel Sightings by Location",
       x = "Location",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#     a.	Which grids are most prominent and what may be attributed to popularity?

#     b.	Are these areas more frequented in the AM or PM?
TotalAMViews <- sum(nyc_squirrels$shift == 'AM')
TotalPMViews <- sum(nyc_squirrels$shift == 'PM')

sightings_by_location_shift <- nyc_squirrels %>%
  group_by(location, shift) %>%
  summarise(count = n()) %>%
  arrange(location, desc(count))

ggplot(sightings_by_location_shift, aes(x = reorder(location, -count), y = count, fill = shift)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Squirrel Sightings by Location and Shift",
       x = "Location",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###  2)	Are squirrels with the same unique squirrel ID sighted multiple times in multiple areas?
library(dplyr)

sightings_multiple_areas <- nyc_squirrels %>%
  group_by(unique_squirrel_id) %>%
  summarize(
    count = n(),
    distinct_hectares = n_distinct(hectare),
    distinct_lat_long = n_distinct(lat, long)
  ) %>%
  filter(distinct_hectares > 1 | distinct_lat_long > 1)

print(sightings_multiple_areas)
#     a.	What activities are attributed to the most active squirrels?
squirrel_activity_summary <- nyc_squirrels %>%
  rowwise() %>%
  mutate(activity_count = sum(c_across(running:foraging), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(unique_squirrel_id) %>%
  summarize(
    total_activities = sum(activity_count, na.rm = TRUE),
    running = sum(running, na.rm = TRUE),
    chasing = sum(chasing, na.rm = TRUE),
    climbing = sum(climbing, na.rm = TRUE),
    eating = sum(eating, na.rm = TRUE),
    foraging = sum(foraging, na.rm = TRUE)
  ) %>%
  arrange(desc(total_activities))

top_active_squirrels <- squirrel_activity_summary %>%
  top_n(10, total_activities)

top_squirrel_activities <- top_active_squirrels %>%
  summarize(
    running = sum(running),
    chasing = sum(chasing),
    climbing = sum(climbing),
    eating = sum(eating),
    foraging = sum(foraging)
  )

print(top_squirrel_activities)
#     b.	What is the correlation between age and sightings and activity?

squirrel_sightings <- nyc_squirrels %>%
  group_by(unique_squirrel_id, age) %>%
  summarize(sightings = n(), .groups = 'drop')

squirrel_activities <- nyc_squirrels %>%
  rowwise() %>%
  mutate(activity_count = sum(c_across(running:foraging), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(unique_squirrel_id) %>%
  summarize(total_activity = sum(activity_count, na.rm = TRUE), .groups = 'drop')

squirrel_summary <- squirrel_sightings %>%
  left_join(squirrel_activities, by = "unique_squirrel_id")

print(squirrel_summary)

#install.packages("psych")
library(psych)

#notice sd is zero
correlation_results <- squirrel_summary %>%
  filter(!is.na(age)) %>%
  group_by(age) %>%
  summarize(correlation_sightings_activity = cor(sightings, total_activity, use = "complete.obs"))

print(correlation_results)

#     c.	Is there a correlation between frequent sightings and fur color?
squirrel_sightings <- nyc_squirrels %>%
  group_by(unique_squirrel_id, primary_fur_color) %>%
  summarize(sightings = n(), .groups = 'drop')

sightings_by_fur_color <- squirrel_sightings %>%
  group_by(primary_fur_color) %>%
  summarize(
    average_sightings = mean(sightings),
    total_sightings = sum(sightings),
    count_squirrels = n(),
    .groups = 'drop'
  )
#graph is essentially useless
ggplot(sightings_by_fur_color, aes(x = primary_fur_color, y = average_sightings)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sightings by Fur Color", x = "Primary Fur Color", y = "Average Sightings") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 2))


#     d.	Is there a correlation between fur color and certain activities?
library(tidyr)
activities_by_fur_color <- nyc_squirrels %>%
  group_by(primary_fur_color) %>%
  summarize(
    running = sum(running, na.rm = TRUE),
    chasing = sum(chasing, na.rm = TRUE),
    climbing = sum(climbing, na.rm = TRUE),
    eating = sum(eating, na.rm = TRUE),
    foraging = sum(foraging, na.rm = TRUE),
    .groups = 'drop'
  )

activities_long <- activities_by_fur_color %>%
  pivot_longer(cols = running:foraging, names_to = "activity", values_to = "count")

ggplot(activities_long, aes(x = primary_fur_color, y = count, fill = activity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Activities by Fur Color", x = "Primary Fur Color", y = "Activity Count") +
  theme_minimal()

activity_contingency_table <- nyc_squirrels %>%
  pivot_longer(cols = running:foraging, names_to = "activity", values_to = "count") %>%
  group_by(primary_fur_color, activity) %>%
  summarize(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = activity, values_from = count, values_fill = 0)

activity_matrix <- as.matrix(activity_contingency_table[,-1])

rownames(activity_matrix) <- activity_contingency_table$primary_fur_color

chi_squared_result <- chisq.test(activity_matrix)

chi_squared_result

###  3)	What dates are the most prominent for squirrel sightings?
nyc_squirrels$date <- as.Date(nyc_squirrels$date)

sightings_by_date <- nyc_squirrels %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#I dont know why these dates are formatting weird
ggplot(sightings_by_date, aes(x = date, y = count)) +
  geom_line() +
  geom_point() +
  labs(title = "Squirrel Sightings Over Time",
       x = "Date",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#     a.	What squirrel activities are most prominent on those dates?

top_dates <- head(sightings_by_date, 5)

top_dates_list <- top_dates$date

squirrels_top_dates <- nyc_squirrels %>%
  filter(date %in% top_dates_list)

activity_counts <- squirrels_top_dates %>%
    summarise(
      running = sum(running),
      chasing = sum(chasing),
      climbing = sum(climbing),
      eating = sum(eating),
      foraging = sum(foraging),
      kuks = sum(kuks),
      quaas = sum(quaas),
      moans = sum(moans),
      tail_flags = sum(tail_flags),
      tail_twitches = sum(tail_twitches),
      approaches = sum(approaches),
      indifferent = sum(indifferent),
      runs_from = sum(runs_from)
)

activity_counts_long <- gather(activity_counts, key = "activity", value = "count")

ggplot(activity_counts_long, aes(x = reorder(activity, -count), y = count, fill = activity)) +
  geom_bar(stat = "identity") +
  labs(title = "Squirrel Activities on Top Dates",
       x = "Activity",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#     b.	Can it be inferred that specific dates and activities are linked to either mating or hibernation preparation season?
library(tidyr)
library(dplyr)
activities_of_interest <- nyc_squirrels %>%
  select(date, kuks, moans, chasing, eating, foraging, runs_from, indifferent) %>%
  mutate(season = ifelse(month(date) %in% c(3:5), "Spring (Mating)", 
                         ifelse(month(date) %in% c(9:11), "Fall (Mating and Preparation)", "Other")))

activity_of_interest_summary <- activities_of_interest %>%
  group_by(date, season) %>%
  summarise(
    total_kuks = sum(kuks),
    total_moans = sum(moans),
    total_chasing = sum(chasing),
    total_eating = sum(eating),
    total_foraging = sum(foraging),
    total_runs_from = sum(runs_from),
    total_indifferent = sum(indifferent)
  )
#dates still formatting weird
#install.packages("scales")
library(scales)
library(lubridate)
library(ggplot2)

ggplot(activity_of_interest_summary, aes(x = date)) +
  geom_line(aes(y = total_kuks), color = "blue", linetype = "solid", size = 1) +
  geom_line(aes(y = total_moans), color = "orange", linetype = "solid", size = 1) +
  geom_line(aes(y = total_chasing), color = "green", linetype = "solid", size = 1) +
  geom_line(aes(y = total_eating), color = "red", linetype = "solid", size = 1) +
  geom_line(aes(y = total_foraging), color = "purple", linetype = "solid", size = 1) +
  geom_line(aes(y = total_runs_from), color = "brown", linetype = "solid", size = 1) +
  geom_line(aes(y = total_indifferent), color = "black", linetype = "solid", size = 1) +
  labs(title = "Squirrel Activities Over Time",
       x = "Date",
       y = "Count",
       color = "Activity") +
  facet_wrap(~ season, scales = "free_y") +
  scale_color_manual(values = c("blue", "orange", "green", "red", "purple", "brown", "black"),
                     labels = c("Kuks", "Moans", "Chasing", "Eating", "Foraging", "Runs From", "Indifferent")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###  4)	Which community districts have the most and least sightings?

sightings_by_district <- nyc_squirrels %>%
  group_by(community_districts) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

district_most <- sightings_by_district[which.max(sightings_by_district$count), ]
print(paste("Community district with the most sightings:", district_most$community_districts))

district_least <- sightings_by_district[which.min(sightings_by_district$count[sightings_by_district$count > 0]), ]
print(paste("Community district with the least sightings:", district_least$community_districts))

#     a.	Which community districts have the most squirrels approaching humans?

approaching_squirrels <- nyc_squirrels %>%
  filter(approaches == TRUE)

approaches_by_district <- approaching_squirrels %>%
  group_by(community_districts) %>%
  summarise(count_approaches = n()) %>%
  arrange(desc(count_approaches))

#This is wrong
ggplot(approaches_by_district, aes(x = reorder(community_districts, -count_approaches), y = count_approaches)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Squirrel Approaches by Community District",
       x = "Community District",
       y = "Number of Approaches") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#     b.	Which activities are most common in each community district, and can this be inferred that humans should avoid these areas?
activities_by_district <- nyc_squirrels %>%
  group_by(community_districts) %>%
  summarize(
    running = sum(running, na.rm = TRUE),
    chasing = sum(chasing, na.rm = TRUE),
    climbing = sum(climbing, na.rm = TRUE),
    eating = sum(eating, na.rm = TRUE),
    foraging = sum(foraging, na.rm = TRUE),
    .groups = 'drop'
  )

activities_long <- activities_by_district %>%
  pivot_longer(cols = running:foraging, names_to = "activity", values_to = "count")

most_common_activities <- activities_long %>%
  group_by(community_districts) %>%
  slice_max(order_by = count, n = 1)

print(most_common_activities)
#not a super interpretable graph
ggplot(activities_long, aes(x = community_districts, y = count, fill = activity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Common Squirrel Activities by Community District",
       x = "Community District", y = "Count of Activities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###  5)	Which police precincts have the most and least sightings?
sightings_by_precinct <- nyc_squirrels %>%
  group_by(police_precincts) %>%
  summarise(count_sightings = n()) %>%
  arrange(desc(count_sightings))

precinct_most <- sightings_by_precinct[which.max(sightings_by_precinct$count_sightings), ]
print(paste("Police precinct with the most sightings:", precinct_most$police_precincts))

precinct_least <- sightings_by_precinct[which.min(sightings_by_precinct$count_sightings[sightings_by_precinct$count_sightings > 0]), ]
print(paste("Police precinct with the least sightings:", precinct_least$police_precincts))

#Not a super helpful graph, might want to add another excluding 13
ggplot(sightings_by_precinct, aes(x = reorder(police_precincts, -count_sightings), y = count_sightings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Squirrel Sightings by Police Precinct",
       x = "Police Precinct",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Without 13
filtered_squirrels <- nyc_squirrels %>%
  filter(police_precincts != 13)

sightings_by_precinct <- filtered_squirrels %>%
  group_by(police_precincts) %>%
  summarise(count_sightings = n()) %>%
  arrange(desc(count_sightings))

ggplot(sightings_by_precinct, aes(x = reorder(police_precincts, -count_sightings), y = count_sightings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Squirrel Sightings by Police Precinct (Excluding Precinct 13)",
       x = "Police Precinct",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#     a.	Which police precincts have the most squirrels running from humans? 

running_squirrels <- nyc_squirrels %>%
  filter(runs_from == TRUE)

runs_from_by_precinct <- running_squirrels %>%
  group_by(police_precincts) %>%
  summarise(count_runs_from = n()) %>%
  arrange(desc(count_runs_from))

ggplot(runs_from_by_precinct, aes(x = reorder(police_precincts, -count_runs_from), y = count_runs_from)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Number of Squirrel Sightings Where Squirrels Run from Humans by Police Precinct",
       x = "Police Precinct",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#without 13
filtered_squirrels <- nyc_squirrels %>%
  filter(police_precincts != 13) %>%
  filter(runs_from == TRUE)

runs_from_by_precinct <- filtered_squirrels %>%
  group_by(police_precincts) %>%
  summarise(count_runs_from = n()) %>%
  arrange(desc(count_runs_from))

ggplot(runs_from_by_precinct, aes(x = reorder(police_precincts, -count_runs_from), y = count_runs_from)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Number of Squirrel Sightings Where Squirrels Run from Humans by Police Precinct (Excluding Precinct 13)",
       x = "Police Precinct",
       y = "Number of Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#     b.	Which activities are most common in each police precinct, and can this be inferred that police are provoking these actions?

activity_by_precinct <- nyc_squirrels %>%
  group_by(police_precincts) %>%
  summarise(
    running = sum(running),
    chasing = sum(chasing),
    climbing = sum(climbing),
    eating = sum(eating),
    foraging = sum(foraging),
    kuks = sum(kuks),
    quaas = sum(quaas),
    moans = sum(moans),
    tail_flags = sum(tail_flags),
    tail_twitches = sum(tail_twitches),
    approaches = sum(approaches),
    indifferent = sum(indifferent),
    runs_from = sum(runs_from)
  )

activity_long <- activity_by_precinct %>%
  pivot_longer(cols = -police_precincts, names_to = "activity", values_to = "count")

ggplot(activity_long, aes(x = police_precincts, y = activity, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Squirrel Activities Across Police Precincts",
       x = "Police Precinct",
       y = "Activity",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###  6)	What are the most common fur colors among the squirrel population?

fur_color_counts <- nyc_squirrels %>%
  group_by(primary_fur_color) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
print(fur_color_counts)
ggplot(fur_color_counts, aes(x = reorder(primary_fur_color, -count), y = count, fill = primary_fur_color)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Squirrel Fur Colors", x = "Primary Fur Color", y = "Count") +
  scale_fill_manual(values = c("Gray" = "gray", "Cinnamon" = "sienna", "Black" = "black"))

#     a.	Do certain fur colors or combinations correlate with increased interaction with humans? 
interaction_by_fur_color <- nyc_squirrels %>%
  group_by(primary_fur_color) %>%
  summarize(
    approaches = sum(approaches, na.rm = TRUE),
    indifferent = sum(indifferent, na.rm = TRUE),
    runs_from = sum(runs_from, na.rm = TRUE)
  )
interaction_by_combination <- nyc_squirrels %>%
  group_by(combination_of_primary_and_highlight_color) %>%
  summarize(
    approaches = sum(approaches, na.rm = TRUE),
    indifferent = sum(indifferent, na.rm = TRUE),
    runs_from = sum(runs_from, na.rm = TRUE)
  )

print(interaction_by_fur_color)
print(interaction_by_combination)

interaction_by_fur_color_long <- interaction_by_fur_color %>%
  pivot_longer(cols = c(approaches, indifferent, runs_from), names_to = "interaction", values_to = "count")

ggplot(interaction_by_fur_color_long, aes(x = primary_fur_color, y = count, fill = interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Interactions with Humans by Squirrel Fur Color", x = "Primary Fur Color", y = "Count")

interaction_by_combination_long <- interaction_by_combination %>%
  pivot_longer(cols = c(approaches, indifferent, runs_from), names_to = "interaction", values_to = "count")

ggplot(interaction_by_combination_long, aes(x = combination_of_primary_and_highlight_color, y = count, fill = interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Interactions with Humans by Squirrel Fur Color Combination", x = "Fur Color Combination", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#     b.	Is there fur colors most associated with specific activities such as climbing or foraging? 
activity_by_fur_color <- nyc_squirrels %>%
  group_by(primary_fur_color) %>%
  summarize(
    climbing = sum(climbing, na.rm = TRUE),
    foraging = sum(foraging, na.rm = TRUE)
  )

print(activity_by_fur_color)

activity_by_fur_color_long <- activity_by_fur_color %>%
  pivot_longer(cols = c(climbing, foraging), names_to = "activity", values_to = "count")

ggplot(activity_by_fur_color_long, aes(x = primary_fur_color, y = count, fill = activity)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Activities by Squirrel Fur Color", x = "Primary Fur Color", y = "Count")
###  7)	How does the location of sightings vary with time of day? 

ggplot(nyc_squirrels, aes(x = long, y = lat, color = shift)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("AM" = "blue", "PM" = "red")) +
  theme_minimal() +
  labs(title = "Squirrel Sightings by Time of Day",
       x = "Longitude",
       y = "Latitude",
       color = "Shift") +
  theme(legend.position = "bottom")

#     a.	Are adult squirrels more likely to be sighted on the ground or above the ground? 
adult_squirrels <- nyc_squirrels %>%
  filter(age == "Adult")

location_summary <- adult_squirrels %>%
  group_by(location) %>%
  summarize(count = n()) %>%
  filter(location != "")

ggplot(location_summary, aes(x = location, y = count, fill = location)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Locations of Adult Squirrel Sightings", x = "Location", y = "Count") +
  scale_fill_manual(values = c("Ground Plane" = "brown", "Above Ground" = "green"))

#     b.	Does the primary fur color of the squirrels influence their location during different times of day? 
fur_color_location_shift_summary <- nyc_squirrels %>%
  group_by(primary_fur_color, location, shift) %>%
  summarize(count = n()) %>%
  ungroup()


ggplot(fur_color_location_shift_summary, aes(x = primary_fur_color, y = location, fill = count)) +
  geom_tile() +
  facet_wrap(~ shift) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Squirrel Locations by Fur Color and Time of Day", x = "Primary Fur Color", y = "Location", fill = "Count")

###  8)	How do the squirrels interact with humans, how do they vary by location? 
interactions_by_location <- nyc_squirrels %>%
  group_by(location) %>%
  summarize(
    approaches = sum(approaches, na.rm = TRUE),
    indifferent = sum(indifferent, na.rm = TRUE),
    runs_from = sum(runs_from, na.rm = TRUE),
    .groups = 'drop'
  )

interactions_long <- interactions_by_location %>%
  pivot_longer(cols = approaches:runs_from, names_to = "interaction", values_to = "count")

ggplot(interactions_long, aes(x = location, y = count, fill = interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Squirrel Interactions with Humans by Location", x = "Location", y = "Count of Interactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interactions_by_borough <- nyc_squirrels %>%
  group_by(borough_boundaries) %>%
  summarize(
    approaches = sum(approaches, na.rm = TRUE),
    indifferent = sum(indifferent, na.rm = TRUE),
    runs_from = sum(runs_from, na.rm = TRUE),
    .groups = 'drop'
  )

interactions_borough_long <- interactions_by_borough %>%
  pivot_longer(cols = approaches:runs_from, names_to = "interaction", values_to = "count")

ggplot(interactions_borough_long, aes(x = borough_boundaries, y = count, fill = interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Squirrel Interactions with Humans by Borough", x = "Borough", y = "Count of Interactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#     a.	Are squirrels more likely to run from humans when sighted on the ground or above ground? 

#These results are not very helpful
ground_sightings <- nyc_squirrels %>%
  filter(above_ground_sighter_measurement == "ground")

above_ground_sightings <- nyc_squirrels %>%
  filter(above_ground_sighter_measurement == "above ground")

ground_runs_from <- sum(ground_sightings$runs_from, na.rm = TRUE)
above_ground_runs_from <- sum(above_ground_sightings$runs_from, na.rm = TRUE)

runs_from_table <- matrix(c(ground_runs_from, nrow(ground_sightings) - ground_runs_from,
                            above_ground_runs_from, nrow(above_ground_sightings) - above_ground_runs_from),
                          nrow = 2, byrow = TRUE,
                          dimnames = list(Sighted_On = c("Ground", "Above Ground"),
                                          Runs_From = c("Yes", "No")))

fisher_test_result <- fisher.test(runs_from_table)
print(fisher_test_result)

#     b.	Does the time of day influence the interaction? 
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyr)
nyc_squirrels <- nyc_squirrels %>%
  mutate(hour = hour(date))

nyc_squirrels <- nyc_squirrels %>%
  mutate(time_of_day = case_when(
    hour >= 0 & hour < 6 ~ "Night",
    hour >= 6 & hour < 12 ~ "Morning",
    hour >= 12 & hour < 18 ~ "Afternoon",
    hour >= 18 ~ "Evening"
  ))

interactions_by_time <- nyc_squirrels %>%
  group_by(time_of_day) %>%
  summarize(
    approaches = sum(approaches, na.rm = TRUE),
    indifferent = sum(indifferent, na.rm = TRUE),
    runs_from = sum(runs_from, na.rm = TRUE),
    .groups = 'drop'
  )

interactions_long <- interactions_by_time %>%
  pivot_longer(cols = approaches:runs_from, names_to = "interaction", values_to = "count")

library(ggplot2)
ggplot(interactions_long, aes(x = time_of_day, y = count, fill = interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Squirrel Interactions with Humans by Time of Day", x = "Time of Day", y = "Count of Interactions") +
  theme_minimal()

kruskal_test <- kruskal.test(count ~ time_of_day, data = interactions_long)
print(kruskal_test)

###  9)	How do squirrel activities vary by fur color? 

nyc_squirrels$other_activities <- as.numeric(nyc_squirrels$other_activities)

activity_summary <- nyc_squirrels %>%
  group_by(primary_fur_color) %>%
  summarize(
    running = sum(running, na.rm = TRUE),
    chasing = sum(chasing, na.rm = TRUE),
    climbing = sum(climbing, na.rm = TRUE),
    eating = sum(eating, na.rm = TRUE),
    foraging = sum(foraging, na.rm = TRUE),
    other_activities = sum(other_activities, na.rm = TRUE) 
  ) %>%
  gather(key = "activity", value = "count", running:other_activities)

ggplot(activity_summary, aes(x = primary_fur_color, y = count, fill = activity)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Squirrel Activities by Fur Color",
       x = "Fur Color", y = "Count", fill = "Activity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#     a.	Is there a fur color more likely to be running?

running_summary <- nyc_squirrels %>%
  filter(running == TRUE) %>%
  group_by(primary_fur_color) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

fur_color_most_running <- running_summary$primary_fur_color[which.max(running_summary$count)]

cat("Fur color most likely to be running: ", fur_color_most_running, "\n")


#     b.	Is there a fur color more likely to be chasing?

chasing_summary <- nyc_squirrels %>%
  filter(chasing == TRUE) %>%
  group_by(primary_fur_color) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

fur_color_most_chasing <- chasing_summary$primary_fur_color[which.max(chasing_summary$count)]

cat("Fur color most likely to be chasing: ", fur_color_most_chasing, "\n")
#     c.	Is there a fur color more likely to be eating?
eating_summary <- nyc_squirrels %>%
  filter(eating == TRUE) %>%
  group_by(primary_fur_color) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

fur_color_most_eating <- eating_summary$primary_fur_color[which.max(eating_summary$count)]

cat("Fur color most likely to be eating: ", fur_color_most_eating, "\n")

###  10)	 What is the age distribution of squirrels in different boroughs? 




#     a.	Is there any correlation between fur color and age?
#This code runs but the graph is uninterpretable
cross_tab <- table(nyc_squirrels$primary_fur_color, nyc_squirrels$age)

colnames(cross_tab) <- c("Juvenile", "Adult", "Unknown Age")

print(cross_tab)

chi_square <- chisq.test(cross_tab)

print(chi_square)

cross_tab_df <- as.data.frame(cross_tab)
cross_tab_df$primary_fur_color <- rownames(cross_tab_df)

cross_tab_long <- tidyr::gather(cross_tab_df, key = "age", value = "count", -primary_fur_color)

ggplot(cross_tab_long, aes(x = primary_fur_color, y = count, fill = age)) +
  geom_bar(stat = "identity") +
  labs(title = "Fur Color Distribution by Age",
       x = "Primary Fur Color", y = "Count", fill = "Age Category") +
  theme_minimal() +
  scale_fill_manual(values = c("Juvenile" = "#66c2a5", "Adult" = "#fc8d62", "Unknown Age" = "#8da0cb")) +  # Custom colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#What is the age distribution of different burroughs

library(tidyverse)
nyc_squirrels_notna <- nyc_squirrels %>%
  filter(!is.na(age) & age != "")

age_distribution <- nyc_squirrels %>%
  group_by(borough_boundaries, age) %>%
  summarise(count = n(), .groups = 'drop')

print(age_distribution)
library(ggplot2)
ggplot(age_distribution, aes(x = borough_boundaries, y = count, fill = age)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Distribution of Squirrels by Borough in NYC",
       x = "Borough",
       y = "Count",
       fill = "Age") +
  theme_minimal()