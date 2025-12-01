continents.according.to.our.world.in.data <- read.csv("~/Documents/GitHub/Data-Science-Coursework/continents-according-to-our-world-in-data.csv")
gdp.per.capita.worldbank <- read.csv("~/Documents/GitHub/Data-Science-Coursework/gdp-per-capita-worldbank.csv", header=FALSE)
youth.not.in.education.employment.training <- read.csv("~/Documents/GitHub/Data-Science-Coursework/youth-not-in-education-employment-training.csv")

library(tidyverse)
library(dplyr)

# Arrange data frame so all countries in the same continent are listed together
Sorted_continents <- continents.according.to.our.world.in.data %>%
  arrange(Continent) %>%
  # Get rid of Antarctica
  filter(Continent != "Antarctica")

# Get rid of Year column for easier combination of data frames
Sorted_continents <- Sorted_continents[ , -3]

# Get rid of first row
GDP_per_capita <- gdp.per.capita.worldbank[-1, ]

# Rename columns
colnames(GDP_per_capita) <- c("Entity", "Code", "Year", "GDP per capita, PPP (constant 2017 international $)")

# Convert to numeric data
GDP_per_capita <- GDP_per_capita %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(`GDP per capita, PPP (constant 2017 international $)` = as.numeric(`GDP per capita, PPP (constant 2017 international $)`))


# Combine data frames
GDP_with_Continents <- full_join(Sorted_continents, GDP_per_capita)



# Create data frames for each continent

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



# Create data frames for each continent's GDP growth and GDP growth rate
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






# Plot graphs for each country's GDP and GDP growth rate in each Continent
library(ggplot2)

# Plot graph for GDP growth
graph_growth <- function(df) {
ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth (PPP)",
    title = "Graph of GDP per capita Growth against Time"
  ) +
  theme_minimal()
}

# Create plots for each Continent
graph_growth(Africa_GDP_Growth_Rate)
graph_growth(Asia_GDP_Growth_Rate)
graph_growth(Europe_GDP_Growth_Rate)
graph_growth(North_America_GDP_Growth_Rate)
graph_growth(Oceania_GDP_Growth_Rate)
graph_growth(South_America_GDP_Growth_Rate)


# Use line of best fit
graph_average_growth <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita growth (PPP)",
      title = "Graph of GDP per capita Growth Trend against Time"
    ) +
    theme_minimal()
}

# Create plots for each Continent
graph_average_growth(Africa_GDP_Growth_Rate)
graph_average_growth(Asia_GDP_Growth_Rate)
graph_average_growth(Europe_GDP_Growth_Rate)
graph_average_growth(North_America_GDP_Growth_Rate)
graph_average_growth(Oceania_GDP_Growth_Rate)
graph_average_growth(South_America_GDP_Growth_Rate)

# Save all graphs
ggsave(
  filename = "Africa GDP Growth Regression.png",
  plot = graph_average_growth(Africa_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Asia GDP Growth Regression.png",
  plot = graph_average_growth(Asia_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Europe GDP Growth Regression.png",
  plot = graph_average_growth(Europe_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "North America GDP Growth Regression.png",
  plot = graph_average_growth(North_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Oceania GDP Growth Regression.png",
  plot = graph_average_growth(Oceania_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "South America GDP Growth Regression.png",
  plot = graph_average_growth(South_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)


# Plot graph for GDP growth rate
graph_growth_rate <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    geom_line(linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita growth rate (%)",
      title = "Graph of GDP per capita Growth Rate against Time"
    ) +
    theme_minimal()
}

# Create plots for each Continent
graph_growth_rate(Africa_GDP_Growth_Rate)
graph_growth_rate(Asia_GDP_Growth_Rate)
graph_growth_rate(Europe_GDP_Growth_Rate)
graph_growth_rate(North_America_GDP_Growth_Rate)
graph_growth_rate(Oceania_GDP_Growth_Rate)
graph_growth_rate(South_America_GDP_Growth_Rate)


# Use line of best fit
graph_average_growth_rate <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita growth rate (%)",
      title = "Graph of GDP per capita Growth Rate Trend against Time"
    ) +
    theme_minimal()
}

# Create plots for each Continent
graph_average_growth_rate(Africa_GDP_Growth_Rate)
graph_average_growth_rate(Asia_GDP_Growth_Rate)
graph_average_growth_rate(Europe_GDP_Growth_Rate)
graph_average_growth_rate(North_America_GDP_Growth_Rate)
graph_average_growth_rate(Oceania_GDP_Growth_Rate)
graph_average_growth_rate(South_America_GDP_Growth_Rate)

# Save all graphs
ggsave(
  filename = "Africa GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(Africa_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Asia GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(Asia_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Europe GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(Europe_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "North America GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(North_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Oceania GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(Oceania_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "South America GDP Growth Rate Regression.png",
  plot = graph_average_growth_rate(South_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
