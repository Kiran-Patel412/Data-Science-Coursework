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

# Create sets for each continent
Africa_GDP <- GDP_with_Continents %>%
  filter(Continent == "Africa")
Asia_GDP <- GDP_with_Continents %>%
  filter(Continent == "Asia")
Europe_GDP <- GDP_with_Continents %>%
  filter(Continent == "Europe")
North_America_GDP <- GDP_with_Continents %>%
  filter(Continent == "North America")
Oceania_GDP <- GDP_with_Continents %>%
  filter(Continent == "Oceania")
South_America_GDP <- GDP_with_Continents %>%
  filter(Continent == "South America")


library(ggplot2)

# Plot graph for each continent
ggplot(Africa_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()

ggplot(Asia_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()

ggplot(Europe_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()

ggplot(North_America_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()

ggplot(Oceania_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()


ggplot(South_America_GDP) +
  aes(
    x = Year,
    y = `GDP per capita, PPP (constant 2017 international $)`,
    colour = Code
  ) +
  geom_line(linewidth = 0.2) +
  scale_color_hue(direction = 1) +
  labs(
    title = "GDP per capita (PPP) against Years",
    color = "Country Code"
  ) +
  theme_classic()
