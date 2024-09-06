## Loading Libraries:
library(tidyverse)
library(ggplot2)
setwd("C:/Zcosas/Data Analyst/Data Analyst Google Certificated/8 Case Studies/CAPSTONE PROJECT")
library(here)
library(lubridate)
library(dplyr)
library(patchwork)
library(knitr)

#Loading files:
population_data <- read.csv("crime_and_incarceration_by_state.csv")

## Knowing our data:
View(population_data)
str(population_data)

## Making a new data frame to only stay with the data we are going to use
population_data <- subset(population_data, select = c(jurisdiction, year, prisoner_count, state_population, 
                                                murder_manslaughter, rape_legacy, robbery, agg_assault, violent_crime_total,  
                                                burglary, larceny, vehicle_theft, property_crime_total))
population_data <- mutate(population_data, total_crimes = population_data$violent_crime_total + population_data$property_crime_total)


##Creating a new table where I'll put all the index and population:
criminal_indexes <- subset(population_data, select = c(jurisdiction, year, state_population))

View(criminal_indexes)
## NOW STARTS THE 1ST STAGE OF THE PROCESS WHERE I GOING TO ANALYZE EACH CASE BY SEPARATE.

### Working now with the "Murder Manslaughter" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           murder_index = (population_data$murder_manslaughter/population_data$state_population)*100000)

### Working now with the "Rape Legacy" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           rape_index = (population_data$rape_legacy/population_data$state_population)*100000)

### Working now with the "Robbery" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           robbry_index = (population_data$robbery/population_data$state_population)*100000)
criminal_indexes <- rename(criminal_indexes, robbery_index = robbry_index)

### Working now with the "Agg Assault" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           agg_assault_index = (population_data$agg_assault/population_data$state_population)*100000)

### Working now with the "Total Violent Crime" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           total_violent_index = (population_data$violent_crime_total/population_data$state_population)*100000)

### Working now with the "Burglary Crime" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           burglary_index = (population_data$burglary/population_data$state_population)*100000)

### Working now with the "Larceny Crime" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           larceny_index = (population_data$larceny/population_data$state_population)*100000)

### Working now with the "Vehicle Theft Crime" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           vehicle_theft_index = (population_data$vehicle_theft/population_data$state_population)*100000)

### Working now with the "Property Crime" to see how the variable change over the years:
criminal_indexes <- mutate(criminal_indexes, 
                           property_crime_index = (population_data$property_crime_total/population_data$state_population)*100000)

## General crime index
criminal_indexes <- mutate(criminal_indexes, 
                           crime_rate = (population_data$total_crimes/population_data$state_population)*100000)

## Calculating the most changed State:
crime_rate_summary <- criminal_indexes %>% 
  group_by(jurisdiction) %>% 
  summarize(first_year_rate = crime_rate[which.min(year)], last_year_rate = crime_rate[which.max(year)]) %>% 
  mutate(percentage_change =((last_year_rate - first_year_rate) / first_year_rate) * 100)

min_variation <- crime_rate_summary %>% 
  drop_na() %>% 
  summarize(min_variation = min(percentage_change))
min_variation <- crime_rate_summary %>%
  filter(percentage_change >= -44.95 & percentage_change <= -44.93) %>%
  select(jurisdiction, percentage_change)
min_variation

## Murder Manslaughter
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = murder_index), color = "red") + 
  labs(title = "Murder Manslaughter vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Murder Manslaughter Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Rape Legacy
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = rape_index), color = "red") + 
  labs(title = "Rape Legacy vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Rape Legacy Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Robbery
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = robbery_index), color = "red") + 
  labs(title = "Robbery vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Robbery Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Agg Assaults plot
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = agg_assault_index), color = "red") + 
  labs(title = "Agg Assault Index vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Agg Assault Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Total Violent Crime
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = total_violent_index), color = "red") + 
  labs(title = "Total Violent Crime vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Total Violent Crime") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Burglary Crime
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = burglary_index), color = "red") + 
  labs(title = "Burglary Crime vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Burglary Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Larceny Crime
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = larceny_index), color = "red") + 
  labs(title = "Larceny Crime vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Larceny Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Vehicle Theft Crime
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = vehicle_theft_index), color = "red") + 
  labs(title = "Vehicle Theft Crime vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Vehicle Theft Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Property Crime
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = property_crime_index), color = "red") + 
  labs(title = "Property Crime vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Property Crime Index") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))


##------------------------ Crime Rate on the last years ----------------------------------
ggplot(data = criminal_indexes) + geom_smooth(mapping = aes(x = year, y = crime_rate), color = "red") + 
  labs(title = "Crime Rate vs. Year", subtitle = "Index behavior over the last 16 years", x = "Year", y = "Crime") + 
  theme_minimal(base_size = 14) + 
  theme(panel.background = element_rect(fill = "white", color = NA), 
        panel.grid.major = element_line(color = "gray80", size = 0.5), 
        panel.grid.minor = element_line(colour = "gray95", size = 0.25), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.margin = margin(20, 20, 20, 20))

## Finding correlation between property crime, violent crime and population:
cor <- population_data[, c("violent_crime_total", "property_crime_total", "state_population")]
correlation_matrix <- round(cor(cor, use = "complete.obs"), 2)
print(correlation_matrix)

kable(correlation_matrix, caption = "Correlation Matrix of Crime and Population Data")
