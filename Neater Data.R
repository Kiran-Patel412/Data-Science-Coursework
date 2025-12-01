continents.according.to.our.world.in.data <- read.csv("~/Documents/GitHub/Data-Science-Coursework/continents-according-to-our-world-in-data.csv")
gdp.per.capita.worldbank <- read.csv("~/Documents/GitHub/Data-Science-Coursework/gdp-per-capita-worldbank.csv", header=FALSE)
youth.not.in.education.employment.training <- read.csv("~/Documents/GitHub/Data-Science-Coursework/youth-not-in-education-employment-training.csv")

library(tidyverse)
library(dplyr)

# Arrange data set so all countries in the same continent are listed together
Sorted_continents <- continents.according.to.our.world.in.data %>%
  arrange(Continent) %>%
  # Get rid of Antarctica
  filter(Continent != "Antarctica")

# Get rid of Year column for easier combination of data sets
Sorted_continents <- Sorted_continents[ , -3]

# Get rid of first row
GDP_per_capita <- gdp.per.capita.worldbank[-1, ]

# Rename columns
colnames(GDP_per_capita) <- c("Entity", "Code", "Year", "GDP per capita, PPP (constant 2017 international $)")

# Convert to numeric data
GDP_per_capita <- GDP_per_capita %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(`GDP per capita, PPP (constant 2017 international $)` = as.numeric(`GDP per capita, PPP (constant 2017 international $)`))


# Combine data sets
GDP_with_Continents <- full_join(Sorted_continents, GDP_per_capita)



# Create data sets for each continent

Africa_GDP <- GDP_with_Continents %>%
  # select countries in Afria
  filter(Continent == "Africa") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)


Asia_GDP <- GDP_with_Continents %>%
  # select countries in Asia
  filter(Continent == "Asia") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)


Europe_GDP <- GDP_with_Continents %>%
  # select countries in Europe
  filter(Continent == "Europe") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)


North_America_GDP <- GDP_with_Continents %>%
  # select countries in North America
  filter(Continent == "North America") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)


Oceania_GDP <- GDP_with_Continents %>%
  # select countries in Oceania
  filter(Continent == "Oceania") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)


South_America_GDP <- GDP_with_Continents %>%
  # select countries in South America
  filter(Continent == "South America") %>%
  
  # order by country, followed by chronology
  arrange(Entity, Year)



# Create data sets for each continent's GDP growth and GDP growth rate
compute_growth <- function(df) {
  df %>%
    # Ensure calculations are within each country
    group_by(Entity) %>%
    
    # Add column for GDP growth
    mutate(GDP_Growth = (`GDP per capita, PPP (constant 2017 international $)` - lag(`GDP per capita, PPP (constant 2017 international $)`)) ) %>%
    
    # Add column for GDP growth rate   
    mutate(GDP_Growth_Rate = (`GDP per capita, PPP (constant 2017 international $)` - lag(`GDP per capita, PPP (constant 2017 international $)`)) / lag(`GDP per capita, PPP (constant 2017 international $)`) * 100) %>%
    
    # Get rid of country groupings (return data to original state)
    ungroup()
}

Africa_GDP_Growth_Rate <- compute_growth(Africa_GDP)
Asia_GDP_Growth_Rate <- compute_growth(Asia_GDP)
Europe_GDP_Growth_Rate <- compute_growth(Europe_GDP)
North_America_GDP_Growth_Rate <- compute_growth(North_America_GDP)
Oceania_GDP_Growth_Rate <- compute_growth(Oceania_GDP)
South_America_GDP_Growth_Rate <- compute_growth(South_America_GDP)


# Create data set of average GDP for each continent in 2021

library(ggplot2)

# Plot graph for each country's GDP growth in a Continent
graph_growth <- function(df) {
ggplot(df) +
  aes(x = Year, y = GDP_Growth, colour = Entity) +
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth (PPP)",
    title = "Graph of GDP per capita Growth against Time"
  ) +
  theme_minimal()
}

graph_growth(Africa_GDP_Growth_Rate)
graph_growth(Asia_GDP_Growth_Rate)
graph_growth(Europe_GDP_Growth_Rate)
graph_growth(North_America_GDP_Growth_Rate)
graph_growth(Oceania_GDP_Growth_Rate)
graph_growth(South_America_GDP_Growth_Rate)



# Plot graph for each country's GDP growth rate in a Continent
graph_growth_rate <- function(df) {
  ggplot(df) +
    aes(x = Year, y = GDP_Growth_Rate, colour = Entity) +
    geom_line(linewidth = 0.2) +
    scale_color_hue(direction = 1) +
    labs(
      x = "Year",
      y = "GDP per capita growth rate (%)",
      title = "Graph of GDP per capita Growth Rate against Time"
    ) +
    theme_minimal()
}

graph_growth_rate(Africa_GDP_Growth_Rate)
graph_growth_rate(Asia_GDP_Growth_Rate)
graph_growth_rate(Europe_GDP_Growth_Rate)
graph_growth_rate(North_America_GDP_Growth_Rate)
graph_growth_rate(Oceania_GDP_Growth_Rate)
graph_growth_rate(South_America_GDP_Growth_Rate)


