continents.according.to.our.world.in.data <- read.csv("continents-according-to-our-world-in-data.csv")
gdp.per.capita.worldbank <- read.csv("gdp-per-capita-worldbank.csv", header=FALSE)
youth.not.in.education.employment.training <- read.csv("youth-not-in-education-employment-training.csv")
# Install packages
install.packages("tidyverse")
install.packages("dplyr")

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
Growth <- function(df) {
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

Africa_GDP_Growth_Rate <- Growth(Africa_GDP)
Asia_GDP_Growth_Rate <- Growth(Asia_GDP)
Europe_GDP_Growth_Rate <- Growth(Europe_GDP)
North_America_GDP_Growth_Rate <- Growth(North_America_GDP)
Oceania_GDP_Growth_Rate <- Growth(Oceania_GDP)
South_America_GDP_Growth_Rate <- Growth(South_America_GDP)



# Plot graph for each Continent's GDP growth rate

# Calculate Average GDP Growth rate for a Continent
Average_Growth <- function(df) {
  df %>%
    # Keep only relevant columns
    select(Year, GDP_Growth_Rate, Continent) %>%
    
    # Ensure calculations are by year and Continent column is kept
    group_by(Year, Continent) %>%
    
    # Calculate average GDP growth rate across all countries that year
    summarise(Average_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE))
}

# Combine all Continents' GDP Growth Rate in a data frame
Continents_Growth_Rate <- rbind(Average_Growth(Africa_GDP_Growth_Rate), Average_Growth(Asia_GDP_Growth_Rate), Average_Growth(Europe_GDP_Growth_Rate), Average_Growth(North_America_GDP_Growth_Rate), Average_Growth(Oceania_GDP_Growth_Rate), Average_Growth(South_America_GDP_Growth_Rate))

# Plot graphs
install.packages("ggplot2")
library(ggplot2)

Graph_Continents_Growth_Rate <- ggplot(Continents_Growth_Rate, aes(x = Year, y = Average_GDP_Growth_Rate, colour = Continent)) +
  # Each Continent has a different colour for better visualisation
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "Average GDP per capita growth rate (%)",
    title = "Graph of Average GDP per capita Growth Rate against Time"
  ) +
  # Minimal theme for cleaner plot
  theme_minimal()


# Add line of best fit for easier comparison of general trend
Graph_Continents_Average_Growth_Rate <- ggplot(Continents_Growth_Rate, aes(x = Year, y = Average_GDP_Growth_Rate, colour = Continent)) +
  # Each Continent has a different colour for better visualisation
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
  labs(
    x = "Year",
    y = "Average GDP per capita growth rate (%)",
    title = "Graph of Average GDP per capita Growth Rate Trend against Time"
  ) +
  # Minimal theme for cleaner plot
  theme_minimal()


# Save both plots
ggsave(
  filename = "All Continents GDP Growth Rate.png",
  plot = Graph_Continents_Growth_Rate,
  width = 12,
  height = 8,
  # width and height chosen to ensure entire plot is saved
)

ggsave(
  filename = "All Continents GDP Growth Rate Regression.png",
  plot = Graph_Continents_Average_Growth_Rate,
  width = 12,
  height = 8,
)





# Plot graphs for each country's GDP and GDP growth rate in each Continent


# Plot graph for GDP growth
Graph_Growth <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
    # Each country has a different colour for better visualisation
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth (PPP)",
    title = "Graph of GDP per capita Growth against Time"
  ) +
  # Minimal theme for cleaner plot
  theme_minimal()
}

# Create plots for each Continent
Graph_Growth(Africa_GDP_Growth_Rate)
Graph_Growth(Asia_GDP_Growth_Rate)
Graph_Growth(Europe_GDP_Growth_Rate)
Graph_Growth(North_America_GDP_Growth_Rate)
Graph_Growth(Oceania_GDP_Growth_Rate)
Graph_Growth(South_America_GDP_Growth_Rate)


# Use line of best fit
Graph_Average_Growth <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
    # Each country has a different colour for better visualisation
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth (PPP)",
    title = "Graph of GDP per capita Growth Trend against Time"
    ) +
  # Minimal theme for cleaner plot
  theme_minimal()
}

# Create plots for each Continent
Graph_Average_Growth(Africa_GDP_Growth_Rate)
Graph_Average_Growth(Asia_GDP_Growth_Rate)
Graph_Average_Growth(Europe_GDP_Growth_Rate)
Graph_Average_Growth(North_America_GDP_Growth_Rate)
Graph_Average_Growth(Oceania_GDP_Growth_Rate)
Graph_Average_Growth(South_America_GDP_Growth_Rate)

# Save all graphs with line of best fit
ggsave(
  filename = "Africa GDP Growth Regression.png",
  plot = Graph_Average_Growth(Africa_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Asia GDP Growth Regression.png",
  plot = Graph_Average_Growth(Asia_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Europe GDP Growth Regression.png",
  plot = Graph_Average_Growth(Europe_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "North America GDP Growth Regression.png",
  plot = Graph_Average_Growth(North_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Oceania GDP Growth Regression.png",
  plot = Graph_Average_Growth(Oceania_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "South America GDP Growth Regression.png",
  plot = Graph_Average_Growth(South_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)






# Plot graph for GDP growth rate
Graph_Growth_Rate <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    # Each country has a different colour for better visualisation
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth rate (%)",
    title = "Graph of GDP per capita Growth Rate against Time"
    ) +
  # Minimal theme for cleaner plot
  theme_minimal()
}

# Create plots for each Continent
Graph_Growth_Rate(Africa_GDP_Growth_Rate)
Graph_Growth_Rate(Asia_GDP_Growth_Rate)
Graph_Growth_Rate(Europe_GDP_Growth_Rate)
Graph_Growth_Rate(North_America_GDP_Growth_Rate)
Graph_Growth_Rate(Oceania_GDP_Growth_Rate)
Graph_Growth_Rate(South_America_GDP_Growth_Rate)


# Use line of best fit
Graph_Average_Growth_Rate <- function(df) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    # Each country has a different colour for better visualisation
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita growth rate (%)",
    title = "Graph of GDP per capita Growth Rate Trend against Time"
    ) +
  # Minimal theme for cleaner plot
  theme_minimal()
}

# Create plots for each Continent
Graph_Average_Growth_Rate(Africa_GDP_Growth_Rate)
Graph_Average_Growth_Rate(Asia_GDP_Growth_Rate)
Graph_Average_Growth_Rate(Europe_GDP_Growth_Rate)
Graph_Average_Growth_Rate(North_America_GDP_Growth_Rate)
Graph_Average_Growth_Rate(Oceania_GDP_Growth_Rate)
Graph_Average_Growth_Rate(South_America_GDP_Growth_Rate)

# Save all graphs with line of best fit
ggsave(
  filename = "Africa GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(Africa_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Asia GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(Asia_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Europe GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(Europe_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "North America GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(North_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "Oceania GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(Oceania_GDP_Growth_Rate),
  width = 12,
  height = 8,
)
ggsave(
  filename = "South America GDP Growth Rate Regression.png",
  plot = Graph_Average_Growth_Rate(South_America_GDP_Growth_Rate),
  width = 12,
  height = 8,
)






# Part 2 of Task 1
Additional_csv_1 <- read.csv("UNSD — Methodology.csv", header=FALSE, sep=";")

# Re-format table to make first row the Column titles
colnames(Additional_csv_1) <- Additional_csv_1[1, ]

# Convert to character in case of factor
Additional_csv_1$"Least Developed Countries (LDC)" <- as.character(Additional_csv_1$"Least Developed Countries (LDC)")

# Make blank rows "NA"
Additional_csv_1$"Least Developed Countries (LDC)"[Additional_csv_1$"Least Developed Countries (LDC)" == ""] <- NA

# Delete the first row + keep only country and LDC columns + keep rows where countries are LDCs
LDCs <- Additional_csv_1[-1, ] %>%
  select("Country or Area", "Least Developed Countries (LDC)") %>%
  filter(!is.na(`Least Developed Countries (LDC)`))

# Create data frame of all countries' GDP Growth Rates
Countries_GDP_Growth_Rate <- rbind(Africa_GDP_Growth_Rate, Asia_GDP_Growth_Rate, Europe_GDP_Growth_Rate, North_America_GDP_Growth_Rate, Oceania_GDP_Growth_Rate, South_America_GDP_Growth_Rate)

# Create data frame of LDCs' GDP Growth Rates
LDCs_GDP_Growth_Rate <- inner_join(LDCs, Countries_GDP_Growth_Rate,
                                   by = c("Country or Area" = "Entity"))

# Remove column indicating if a country is a LDC
LDCs_GDP_Growth_Rate <- LDCs_GDP_Growth_Rate[ , -2]


# Graph of LDCs GDP growth rate against time
ggplot(LDCs_GDP_Growth_Rate,
       aes(x = Year,
           y = GDP_Growth_Rate,
           group = `Country or Area`,
           colour = `Country or Area`)) +
  geom_line() +
  labs(
    title = "GDP per Capita Growth Rate – Least Developed Countries",
    x = "Year",
    y = "GDP per capita growth rate (%)",
    colour = "Country"
  ) +
  theme_minimal()



# Remove all terms with missing values
max_year <- max(LDCs_GDP_Growth_Rate$Year, na.rm = TRUE)

# As the previous graph is too complicated, we are only using last 5 years for the graph
LDC_Growth_Recent <- LDCs_GDP_Growth_Rate %>%
  filter(Year >= max_year - 4)

# Graph of LDCs GDP growth rate from 2017-2021 against time
ggplot(LDC_Growth_Recent,
       aes(x = Year,
           y = GDP_Growth_Rate,
           group = `Country or Area`,
           colour = `Country or Area`)) +
  geom_line() +
  labs(
    title = "GDP per Capita Growth Rate (Last 5 Years) for Least Developed Countries",
    x = "Year",
    y = "GDP per capita growth rate (%)",
    colour = "Country"
  ) +
  theme_minimal()


# Create data frame with mean growth of LDCs per Continent per year
Continents_LDC_Growth_Rate <- LDCs_GDP_Growth_Rate %>%
  group_by(Continent, Year) %>%
  summarise(Mean_Growth = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()


# Graph of LDCs GDP growth rate per Continent against time
ggplot(Continents_LDC_Growth_Rate,
       aes(x = Year,
           y = Mean_Growth,
           colour = Continent)) +
  geom_line(size = 1) +
  labs(
    title = "Average GDP per Capita Growth Rate by Continent (LDCs)",
    x = "Year",
    y = "Average GDP per capita growth rate (%)",
    colour = "Continent"
  ) +
  theme_minimal()

# Install packages used for the map graph
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")  

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

LDCs_Growth_Rate_2021 <- LDCs_GDP_Growth_Rate %>%
  filter(Year == 2021)

LDCs_Growth_Rate_2021 <- LDCs_Growth_Rate_2021 %>%
  mutate(Growth_Category = case_when(
      GDP_Growth_Rate >= 7  ~ "≥ 7% (Meets Target)",
      GDP_Growth_Rate >= 4  ~ "4% – 7%",
      GDP_Growth_Rate >= 0  ~ "0% – 4%",
      TRUE ~ "Negative Growth"))

World <- ne_countries(scale = "medium", returnclass = "sf")

World_LDCs <- World %>%
  left_join(LDCs_Growth_Rate_2021, by = c("iso_a3" = "Code"))

# Create world map separating LDCs by growth rates
ggplot(World_LDCs) +
  geom_sf(aes(fill = Growth_Category), color = "grey", size = 0.1) +
  scale_fill_manual(
    values = c(
      "≥ 7% (Meets Target)" = "darkgreen",    
      "4% – 7%"              = "lightgreen",   
      "0% – 4%"              = "orange",    
      "Negative Growth"      = "red"     
    )
  ) +
  labs(
    title = "LDC GDP per Capita Growth Rate Categories (2021)",
    fill  = "Growth Category"
  ) +
  theme_minimal()




