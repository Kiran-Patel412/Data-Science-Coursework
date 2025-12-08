continents.according.to.our.world.in.data <- read.csv("~/Documents/GitHub/Data-Science-Coursework/continents-according-to-our-world-in-data.csv")
gdp.per.capita.worldbank <- read.csv("~/Documents/GitHub/Data-Science-Coursework/gdp-per-capita-worldbank.csv", header=FALSE)
youth.not.in.education.employment.training <- read.csv("~/Documents/GitHub/Data-Science-Coursework/youth-not-in-education-employment-training.csv")

# Install and load packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# Background for Task 1

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

Graph_Continents_Growth_Rate <- ggplot(Continents_Growth_Rate, aes(x = Year, y = Average_GDP_Growth_Rate, colour = Continent)) +
  # Each Continent has a different colour for better visualisation
  geom_line(linewidth = 0.2) +
  labs(
    x = "Year",
    y = "Average GDP per capita Growth Rate (%)",
    title = "Average GDP per capita Growth Rate against Time"
  ) +
  # Minimal theme for cleaner plot
  theme_minimal()


# Add line of best fit for easier comparison of general trend
Graph_Continents_Average_Growth_Rate <- ggplot(Continents_Growth_Rate, aes(x = Year, y = Average_GDP_Growth_Rate, colour = Continent)) +
  # Each Continent has a different colour for better visualisation
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
  labs(
    x = "Year",
    y = "Average GDP per capita Growth Rate (%)",
    title = "Average GDP per capita Growth Rate Trend against Time"
  ) +
  # Minimal theme for cleaner plot
  theme_minimal()




# Plot graphs for each country's GDP and GDP growth rate in each Continent


# Plot graph for GDP growth
Graph_Growth <- function(df, Continent) {
  ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
    # Each country has a different colour for better visualisation
    geom_line(linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita Growth (PPP)",
      title = paste0("GDP per capita Growth against Time (",Continent,")")
    ) +
    # Minimal theme for cleaner plot
    theme_minimal()
}

# Create plots for each Continent
Graph_Growth(Africa_GDP_Growth_Rate, Continent = "Africa")
Graph_Growth(Asia_GDP_Growth_Rate, Continent = "Asia")
Graph_Growth(Europe_GDP_Growth_Rate, Continent = "Europe")
Graph_Growth(North_America_GDP_Growth_Rate, Continent = "North America")
Graph_Growth(Oceania_GDP_Growth_Rate, Continent = "Oceania")
Graph_Growth(South_America_GDP_Growth_Rate, Continent = "South America")


# Use line of best fit
Graph_Average_Growth <- function(df, Continent) {
  ggplot(df, aes(x = Year, y = GDP_Growth, colour = Entity)) +
    # Each country has a different colour for better visualisation
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita Growth (PPP)",
      title = paste0("GDP per capita Growth Trend against Time (",Continent,")")
    ) +
    # Minimal theme for cleaner plot
    theme_minimal()
}

# Create plots for each Continent
Graph_Average_Growth(Africa_GDP_Growth_Rate, Continent = "Africa")
Graph_Average_Growth(Asia_GDP_Growth_Rate, Continent = "Asia")
Graph_Average_Growth(Europe_GDP_Growth_Rate, Continent = "Europe")
Graph_Average_Growth(North_America_GDP_Growth_Rate, Continent = "North America")
Graph_Average_Growth(Oceania_GDP_Growth_Rate, Continent = "Oceania")
Graph_Average_Growth(South_America_GDP_Growth_Rate, Continent = "South America")




# Plot graph for GDP growth rate
Graph_Growth_Rate <- function(df, Continent) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    # Each country has a different colour for better visualisation
    geom_line(linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita Growth Rate (%)",
      title = paste0("GDP per capita Growth Rate against Time (",Continent,")")
    ) +
    # Minimal theme for cleaner plot
    theme_minimal()
}

# Create plots for each Continent
Graph_Growth_Rate(Africa_GDP_Growth_Rate, Continent = "Africa")
Graph_Growth_Rate(Asia_GDP_Growth_Rate, Continent = "Asia")
Graph_Growth_Rate(Europe_GDP_Growth_Rate, Continent = "Europe")
Graph_Growth_Rate(North_America_GDP_Growth_Rate, Continent = "North America")
Graph_Growth_Rate(Oceania_GDP_Growth_Rate, Continent = "Oceania")
Graph_Growth_Rate(South_America_GDP_Growth_Rate, Continent = "South America")


# Use line of best fit
Graph_Average_Growth_Rate <- function(df, Continent) {
  ggplot(df, aes(x = Year, y = GDP_Growth_Rate, colour = Entity)) +
    # Each country has a different colour for better visualisation
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.2) +
    labs(
      x = "Year",
      y = "GDP per capita Growth Rate (%)",
      title = paste0("GDP per capita Growth Rate Trend against Time (",Continent,")")
    ) +
    # Minimal theme for cleaner plot
    theme_minimal()
}

# Create plots for each Continent
Graph_Average_Growth_Rate(Africa_GDP_Growth_Rate, Continent = "Africa")
Graph_Average_Growth_Rate(Asia_GDP_Growth_Rate, Continent = "Asia")
Graph_Average_Growth_Rate(Europe_GDP_Growth_Rate, Continent = "Europe")
Graph_Average_Growth_Rate(North_America_GDP_Growth_Rate, Continent = "North America")
Graph_Average_Growth_Rate(Oceania_GDP_Growth_Rate, Continent = "Oceania")
Graph_Average_Growth_Rate(South_America_GDP_Growth_Rate, Continent = "South America")







# Part 1 of Task 1

# Create data frame of all countries' GDP Growth Rates
Countries_GDP_Growth_Rate <- rbind(Africa_GDP_Growth_Rate, Asia_GDP_Growth_Rate, Europe_GDP_Growth_Rate, North_America_GDP_Growth_Rate, Oceania_GDP_Growth_Rate, South_America_GDP_Growth_Rate)

# Create data frame sorting each country into their income group
Countries_Income_Group <- Countries_GDP_Growth_Rate %>%
  # Reduce timeframe to be the recent 5 years
  filter(between(Year, 2017, 2021)) %>%
  mutate(Income_Group = case_when(
    `GDP per capita, PPP (constant 2017 international $)` <= 1135  ~ "Low Income",
    `GDP per capita, PPP (constant 2017 international $)` <= 4495  ~ "Lower Middle Income",
    `GDP per capita, PPP (constant 2017 international $)` <= 13935  ~ "Upper Middle Income",
    `GDP per capita, PPP (constant 2017 international $)` > 13935 ~ "High Income"
     )) %>%
  # Ensure each country only has one income group (following the status in the most recent year)
  group_by(Entity) %>%
  mutate(Income_Group = Income_Group[which.max(Year)]) %>%
  ungroup()


# Create data frames for each Income group
Low_Income <- Countries_Income_Group %>%
  filter(Income_Group == "Low Income")

Lower_Middle_Income <- Countries_Income_Group%>%
  filter(Income_Group == "Lower Middle Income")

Upper_Middle_Income <- Countries_Income_Group %>%
  filter(Income_Group == "Upper Middle Income")

High_Income <- Countries_Income_Group %>%
  filter(Income_Group == "High Income")


# Graphs of GDP Growth Rate for each Income Group
Graph_Low_Income <- ggplot(Low_Income) +
  aes(x = Year, y = GDP_Growth_Rate, colour = Entity) +
  geom_line(linewidth = 0.2) +

  # Add a dashed line at 7% target
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    title = "GDP per capita Growth Rate against Time (Low Income)"
  ) +

  # Minimal theme for cleaner plot
  theme_minimal()


Graph_Lower_Middle_Income <- ggplot(Lower_Middle_Income) +
  aes(x = Year, y = GDP_Growth_Rate, colour = Entity) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 5% target
  geom_hline(yintercept = 5, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    title = "GDP per capita Growth Rate against Time (Lower Middle Income)"
  ) +
  
  # Minimal theme for cleaner plot
  theme_minimal()


Graph_Upper_Middle_Income <- ggplot(Upper_Middle_Income) +
  aes(x = Year, y = GDP_Growth_Rate, colour = Entity) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 4% target
  geom_hline(yintercept = 4, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    title = "GDP per capita Growth Rate against Time (Upper Middle Income)"
  ) +
  
  # Minimal theme for cleaner plot
  theme_minimal()


Graph_High_Income <- ggplot(High_Income) +
  aes(x = Year, y = GDP_Growth_Rate, colour = Entity) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 2% target
  geom_hline(yintercept = 2, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    title = "GDP per capita Growth Rate against Time (High Income)"
  ) +
  
  # Minimal theme for cleaner plot
  theme_minimal()



# Calculate mean GDP per capita Growth Rate for each Continent within each income group
Average_Low_Income <- Low_Income %>%
  group_by(Continent, Year) %>%
  summarise(Mean_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()

Average_Lower_Middle_Income <- Lower_Middle_Income %>%
  group_by(Continent, Year) %>%
  summarise(Mean_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()

Average_Upper_Middle_Income <- Upper_Middle_Income %>%
  group_by(Continent, Year) %>%
  summarise(Mean_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()

Average_High_Income <- High_Income %>%
  group_by(Continent, Year) %>%
  summarise(Mean_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()


# Graphs of mean GDP per capita Growth Rates in each Continent for each income group
Graph_Average_Low_Income <- ggplot(Average_Low_Income) +
  aes(x = Year, y = Mean_GDP_Growth_Rate, colour = Continent) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  labs(
    title = "Average GDP per capita Growth Rate per Continent against Time (Low Income)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)"
  ) +
  theme_minimal()

Graph_Average_Lower_Middle_Income <- ggplot(Average_Lower_Middle_Income)+
  aes(x = Year, y = Mean_GDP_Growth_Rate, colour = Continent) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "black") +
  labs(
    title = "Average GDP per capita Growth Rate per Continent against Time (Lower Middle Income)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)"
  ) +
  theme_minimal()

Graph_Average_Upper_Middle_Income <- ggplot(Average_Upper_Middle_Income)+
  aes(x = Year, y = Mean_GDP_Growth_Rate, colour = Continent) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = 4, linetype = "dashed", colour = "black") +
  labs(
    title = "Average GDP per capita Growth Rate per Continent against Time (Upper Middle Income)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)"
  ) +
  theme_minimal()

Graph_Average_High_Income <- ggplot(Average_High_Income)+
  aes(x = Year, y = Mean_GDP_Growth_Rate, colour = Continent) +
  geom_line(linewidth = 0.2) +
  geom_hline(yintercept = 2, linetype = "dashed", colour = "black") +
  labs(
    title = "Average GDP per capita Growth Rate per Continent against Time (High Income)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)"
  ) +
  theme_minimal()


# Create data frame of total number of countries in each Continent
Total_countries_in_Continent <- Sorted_continents %>%
  group_by(Continent) %>%
  summarise(Number_Countries = n())

# Create data frames of number of countries per Continent that met their GDP Growth Rate Targets for each income group per Year
Low_Income_With_Growth <- Low_Income %>%
  filter(GDP_Growth_Rate >= 7) %>%
  group_by(Year, Continent) %>%
  summarise(
    Number = length(unique(Entity))
  ) %>%
  ungroup()

Lower_Middle_Income_With_Growth <- Lower_Middle_Income %>%
  filter(GDP_Growth_Rate >= 5) %>%
  group_by(Year, Continent) %>%
  summarise(
    Number = length(unique(Entity))
  ) %>%
  ungroup()

Upper_Middle_Income_With_Growth <- Upper_Middle_Income %>%
  filter(GDP_Growth_Rate >= 4) %>%
  group_by(Year, Continent) %>%
  summarise(
    Number = length(unique(Entity))
  ) %>%
  ungroup()

High_Income_With_Growth <- High_Income %>%
  filter(GDP_Growth_Rate >= 2) %>%
  group_by(Year, Continent) %>%
  summarise(
    Number = length(unique(Entity))
  ) %>%
  ungroup()


# Combine in a data frame
Achieved_Growth <- rbind(Low_Income_With_Growth, Lower_Middle_Income_With_Growth, Upper_Middle_Income_With_Growth, High_Income_With_Growth)

# Create data frame of proportion of countries per Continent that met their GDP Growth Rate Targets per Year
Proportion_Achieved_Growth <- Achieved_Growth %>%
  group_by(Year, Continent) %>%
  summarise(Total_Number = sum(Number)) %>%
  ungroup() %>%
  left_join(Total_countries_in_Continent, by = "Continent") %>%
  mutate(Proportion = Total_Number / Number_Countries)

# Graph of proportion of countries that met their GDP Growth Rate Target per Year by Continent
Graph_Proportion_Achieved_Growth <- ggplot(Proportion_Achieved_Growth) +
  aes(x = Year, y = Proportion, fill = Continent) +
  geom_col() +
  facet_wrap(~Continent) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Proportion of Countries that met GDP per capita Growth Rate Target by Continent"
  ) +
  theme_minimal() +
  
  # Increase facet label size
  theme(
    strip.text = element_text(size = 15)
  )


# Save the graph
ggsave(
  filename = "Proportion of Countries that met their GDP Growth Rate Target.png",
  plot = Graph_Proportion_Achieved_Growth,
  width = 12,
  height = 8,
)

# Part 2 of Task 1
Additional_csv_1 <- read.csv("~/Documents/GitHub/Data-Science-Coursework/UNSD — Methodology.csv", header=FALSE, sep=";")

# Re-format table to make first row the Column titles
colnames(Additional_csv_1) <- Additional_csv_1[1, ]

# Convert to character in case of factor
Additional_csv_1$"Least Developed Countries (LDC)" <- as.character(Additional_csv_1$"Least Developed Countries (LDC)")

# Make blank rows "NA"
Additional_csv_1$"Least Developed Countries (LDC)"[Additional_csv_1$"Least Developed Countries (LDC)" == ""] <- NA


# Create data frame of only LDCs
# Get rid of the first row
LDCs <- Additional_csv_1[-1, ] %>%
  
  # Keep only country and LDC columns
  select("Country or Area", "Least Developed Countries (LDC)") %>%
  
  # Only keep rows where countries are LDCs
  filter(!is.na(`Least Developed Countries (LDC)`))


# Create data frame of LDCs' GDP Growth Rates
LDCs_GDP_Growth_Rate <- inner_join(LDCs, Countries_GDP_Growth_Rate,
                                   by = c("Country or Area" = "Entity"))

# Remove column indicating if a country is a LDC
LDCs_GDP_Growth_Rate <- LDCs_GDP_Growth_Rate[ , -2]


# Graph of LDCs GDP growth rate against time
Graph_LDCs <- ggplot(LDCs_GDP_Growth_Rate) +
  aes(x = Year, y = GDP_Growth_Rate, group = `Country or Area`, colour = `Country or Area`) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 7% target
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    title = "GDP per capita Growth Rate against Time (LDCs)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    colour = "Country"
  ) +
  theme_minimal()



# Remove all terms with missing values
max_year <- max(LDCs_GDP_Growth_Rate$Year, na.rm = TRUE)


# Create new data frame to reduce timeframe to be the recent 5 years
LDC_GDP_Growth_Recent <- LDCs_GDP_Growth_Rate %>%
  filter(Year >= max_year - 4)

# Graph of LDCs GDP growth rate from 2017-2021 against time
Graph_LDCs_Recent <- ggplot(LDC_GDP_Growth_Recent) +
  aes(x = Year, y = GDP_Growth_Rate, group = `Country or Area`, colour = `Country or Area`) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 7% target
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    title = "GDP per capita Growth Rate agianst Time (LDCs)",
    x = "Year",
    y = "GDP per capita Growth Rate (%)",
    colour = "Country"
  ) +
  theme_minimal()



# Create data frame with mean growth of LDCs per Continent per year
Continents_LDC_Growth_Rate <- LDCs_GDP_Growth_Rate %>%
  group_by(Continent, Year) %>%
  summarise(Mean_Growth = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  ungroup()


# Graph of LDCs GDP growth rate per Continent against time
Graph_Continents_LDC_Growth_Rate <- ggplot(Continents_LDC_Growth_Rate) +
  aes(x = Year, y = Mean_Growth, colour = Continent) +
  geom_line(linewidth = 0.2) +
  
  # Add a dashed line at 7% target
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black", linewidth = 0.2) +
  labs(
    title = "Average GDP per capita Growth Rate by Continent (LDCs)",
    x = "Year",
    y = "Average GDP per capita growth rate (%)",
    colour = "Continent"
  ) +
  theme_minimal()

# Save the graph
ggsave(
  filename = "LDCs GDP Growth Rate from 2017 to 2021 by Continents.png",
  plot = Graph_Continents_LDC_Growth_Rate,
  width = 12,
  height = 8,
)




# Keep only data from 2021
LDCs_GDP_Growth_Rate_2021 <- LDCs_GDP_Growth_Rate %>%
  filter(Year == 2021)

# Categorise LDCs by GDP growth rates
LDCs_GDP_Growth_Rate_2021 <- LDCs_GDP_Growth_Rate_2021 %>%
  mutate(Growth_Category = case_when(
    GDP_Growth_Rate >= 7  ~ "≥ 7% (Meets Target)",
    GDP_Growth_Rate >= 4  ~ "4% – 7%",
    GDP_Growth_Rate >= 0  ~ "0% – 4%",
    TRUE ~ "Negative Growth"))


# Create data frame for world map graph
World <- ne_countries(scale = "medium", returnclass = "sf")

# Create data frame combined with GDP data
World_LDCs <- World %>%
  left_join(LDCs_GDP_Growth_Rate_2021, by = c("iso_a3" = "Code"))

# Create world map separating LDCs by growth rates
Graph_World <- ggplot(World_LDCs) +
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
    title = "LDCs GDP per capita Growth Rate Categories (2021)",
    fill  = "Growth Category"
  ) +
  theme_minimal()

# Save the graph
ggsave(
  filename = "World Map of LDCs GDP Growth Rates.png",
  plot = Graph_World,
  width = 12,
  height = 8,
)





LDCs_According_Growth <- LDC_GDP_Growth_Recent %>%
  mutate(
    Target = ifelse(GDP_Growth_Rate >= 7,
                    "Meets target (≥ 7%)",
                    "Below target")
  ) %>%
  group_by(Year, Target, Continent) %>%
  summarise(LDCs_Number = length(unique(Code))) %>%
  ungroup()

All_Years      <- 2017:2021
All_Targets    <- c("Meets target (≥ 7%)", "Below target")
All_Continents <- unique(LDC_GDP_Growth_Recent$Continent)

Full_Grid <- expand.grid(
  Year      = All_Years,
  Target    = All_Targets,
  Continent = All_Continents,
  stringsAsFactors = FALSE
)

LDCs_According_Growth_Complete <- Full_Grid %>%
  left_join(LDCs_According_Growth,
            by = c("Year", "Target", "Continent")) %>%
  mutate(LDCs_Number = ifelse(is.na(LDCs_Number), 0, LDCs_Number))


LDCs_According_Growth_Complete <- LDCs_According_Growth_Complete %>%
  mutate(
    Growth_Status = ifelse(Target == "Meets target (≥ 7%)", "Achieved", "Did Not Achieve"),
    Year_Status   = paste0(Year, " (", Growth_Status, ")")
  )

Order <- LDCs_According_Growth_Complete %>%
  arrange(Year, desc(Growth_Status)) %>%
  pull(Year_Status) %>%
  unique()

LDCs_According_Growth_Complete$Year_Status <- factor(LDCs_According_Growth_Complete$Year_Status,
                                                     levels = Order)

# Prepare unique labels for custom x-axis labels (one layer for Year, another for Achievement Status)
x_labels <- LDCs_According_Growth_Complete %>%
  distinct(Year_Status, Year, Growth_Status)

# Graph showing number of LDCs that did not achieve or achieved 7% GDP per capita Growth Rate
Graph_LDCs_Growth_Status <- ggplot(LDCs_According_Growth_Complete) +
  aes(x = Year_Status, y = LDCs_Number, fill = Continent) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year and Achievement Status",
    y = "Number of LDCs",
    title = "Number of LDCs that did not achieve or achieved 7% GDP per capita Growth Rate",
    fill = "Continent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove default x-axis labels
    axis.ticks.x = element_blank(),
    plot.margin = margin(20, 20, 40, 20)  # Add extra bottom space for custom labels
  ) +
  # Add two-row x-axis labels
  geom_text(aes(x = Year_Status, y = -0.5, label = Year), 
            data = x_labels, inherit.aes = FALSE, vjust = 1.2) +  # Bottom row shows Year
  geom_text(aes(x = Year_Status, y = -1.5, label = Growth_Status), 
            data = x_labels, inherit.aes = FALSE, vjust = 1.2) +  # Top row shows Achievement Status
  coord_cartesian(clip = "off")  # allow text outside plot area


# Save the graph
ggsave(
  filename = "Number of LDCs that achieved 7% GDP Growth Rate or not.png",
  plot = Graph_LDCs_Growth_Status,
  width = 12,
  height = 8
)

ldc_raw <- read_excel("UN LDC data.xlsx", sheet = "2024")
ldc_codes <- ldc_raw %>%
  filter(Status == "LDC") %>%          # only LDCs
  transmute(Code = `ISO -3`) %>%       # backticks because of space & dash
  distinct()
gdp <- read_csv("gdp-per-capita-worldbank.csv")
gdp <- gdp %>%
  select(Entity, Code, Year,
         gdp_pc_ppp = `GDP per capita, PPP (constant 2017 international $)`)
continents <- read_csv("continents-according-to-our-world-in-data.csv") %>%
  select(Code, Continent) %>%
  distinct()
gdp_ldc <- gdp %>%
  inner_join(ldc_codes, by = "Code")
gdp_ldc_growth <- gdp_ldc %>%
  arrange(Code, Year) %>%
  group_by(Code) %>%
  mutate(
    GDP_lag    = lag(gdp_pc_ppp),
    gdp_growth = (gdp_pc_ppp - GDP_lag) / GDP_lag * 100
  ) %>%
  ungroup()
gdp_ldc_growth <- gdp_ldc_growth %>%
  left_join(continents, by = "Code")
ldc_target_counts <- gdp_ldc_growth %>%
  filter(Year >= 2016,
         Year <= 2021,
         !is.na(Continent),
         !is.na(gdp_growth)) %>%
  mutate(
    target = ifelse(gdp_growth >= 7,
                    "Meets target (≥ 7%)",
                    "Below target")
  ) %>%
  group_by(Year, target, Continent) %>%
  summarise(n_ldc = length(unique(Code)), .groups = "drop")
all_years      <- 2016:2021
all_targets    <- c("Meets target (≥ 7%)", "Below target")
all_continents <- unique(gdp_ldc_growth$Continent)

full_grid <- expand.grid(
  Year      = all_years,
  target    = all_targets,
  Continent = all_continents,
  stringsAsFactors = FALSE
)

ldc_target_complete <- full_grid %>%
  left_join(ldc_target_counts,
            by = c("Year", "target", "Continent")) %>%
  mutate(n_ldc = ifelse(is.na(n_ldc), 0, n_ldc))



ldc_target_complete <- ldc_target_complete %>%
  mutate(
    target_short = ifelse(target == "Meets target (≥ 7%)", ">=7%", "<7%"),
    YearTarget   = paste0(Year, " (", target_short, ")")
  )

ordered_levels <- ldc_target_complete %>%
  arrange(Year, desc(target_short)) %>%   # >=7% first, then <7%
  pull(YearTarget) %>%
  unique()

ldc_target_complete$YearTarget <- factor(ldc_target_complete$YearTarget,
                                         levels = ordered_levels)


ggplot(ldc_target_complete,
       aes(x = YearTarget,
           y = n_ldc,
           fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year and Target Group",
    y = "Number of LDCs",
    title = "LDCs Meeting / Not Meeting 7% GDP per Capita Growth Target\nStacked by Continent (2016–2021)",
    fill = "Continent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
