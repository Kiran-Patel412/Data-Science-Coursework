
#  Packages

library(tidyverse)
library(scales)
library(ggthemes)

# 2. Colour palette for continents

continent_colors <- c(
  "Asia"          = "#377EB8",
  "Europe"        = "#4DAF4A",
  "Africa"        = "#984EA3",
  "North America" = "#E41A1C",
  "South America" = "#FF7F00",
  "Oceania"       = "#A65628"
)

# 3. Load datasets

neet_raw        <- read_csv("youth-not-in-education-employment-training.csv")
continents_raw  <- read_csv("continents-according-to-our-world-in-data.csv")
youth_unemp_raw <- read_csv("youth_unemployment.csv", skip = 3)
population_raw  <- read_csv("population.csv")   # Country Name, Country Code, Year, Value

# 4. Clean NEET data (1990–2020)

neet <- neet_raw %>%
  select(
    Entity,
    Code,
    Year,
    neet_share = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  mutate(
    Year       = as.integer(Year),
    neet_share = as.numeric(neet_share)
  ) %>%
  filter(
    !is.na(Code),
    !is.na(neet_share),
    Year >= 1990,
    Year <= 2020
  )

# 5. Clean continent mapping

continents <- continents_raw %>%
  select(Entity, Code, Continent) %>%
  distinct(Code, .keep_all = TRUE) %>%
  filter(Continent != "Antarctica")

# 6. Attach continent to NEET data

neet_continent <- neet %>%
  inner_join(continents, by = c("Code", "Entity"))

# 7. Clean youth unemployment (WDI) 1990–2020

year_cols <- names(youth_unemp_raw)[5:ncol(youth_unemp_raw)]

youth_unemp_long <- youth_unemp_raw %>%
  rename(
    Country_Name   = `Country Name`,
    Country_Code   = `Country Code`,
    Indicator_Name = `Indicator Name`,
    Indicator_Code = `Indicator Code`
  ) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "Year",
    values_to = "youth_unemployment"
  ) %>%
  mutate(
    Year               = as.integer(Year),
    youth_unemployment = as.numeric(youth_unemployment)
  ) %>%
  filter(
    Year >= 1990,
    Year <= 2020,
    !is.na(youth_unemployment)
  )

# Add continent to unemployment data
youth_unemp_continent <- youth_unemp_long %>%
  inner_join(
    continents,
    by = c("Country_Code" = "Code", "Country_Name" = "Entity")
  )

# 8. Clean population data (1990–2020)

population <- population_raw %>%
  rename(
    Country_Name = `Country Name`,
    Country_Code = `Country Code`,
    population   = Value
  ) %>%
  mutate(
    Year       = as.integer(Year),
    population = as.numeric(population)
  ) %>%
  filter(
    Year >= 1990,
    Year <= 2020,
    !is.na(population)
  )

# 9. Population-weighted NEET by continent & year

# Add population to NEET data
neet_pop <- neet_continent %>%
  inner_join(
    population,
    by = c("Code" = "Country_Code", "Year" = "Year")
  )

# Weighted NEET:
#   sum(neet_share * population) / sum(population)
neet_continent_year <- neet_pop %>%
  group_by(Continent, Year) %>%
  summarise(
    total_pop      = sum(population, na.rm = TRUE),
    total_neet_pop = sum(neet_share/100 * population, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(
    neet_avg = (total_neet_pop / total_pop) * 100
  )

# 10. GRAPH 1 – NEET time trend (population-weighted)

ggplot(neet_continent_year,
       aes(x = Year, y = neet_avg, colour = Continent)) +
  geom_line(linewidth = 0.9) +
  theme_economist_white() +
  scale_color_manual(values = continent_colors) +
  ggtitle("Are continents reducing NEET by 2020? (population-weighted)") +
  xlab("Year") +
  ylab("Average NEET (%)")

# NEET values for 2000, 2015, 2020 (weighted)

neet_2000 <- neet_continent_year %>%
  filter(Year == 2000) %>%
  select(Continent, neet_2000 = neet_avg)

neet_2015 <- neet_continent_year %>%
  filter(Year == 2015) %>%
  select(Continent, neet_2015 = neet_avg)

neet_2020 <- neet_continent_year %>%
  filter(Year == 2020) %>%
  select(Continent, neet_2020 = neet_avg)

# 12. GRAPH 2 – Change in NEET 2000 → 2020 (bars, weighted)

neet_change_2000 <- neet_2000 %>%
  inner_join(neet_2020, by = "Continent") %>%
  mutate(
    change_pp = neet_2020 - neet_2000   # percentage point change
  )

neet_change_2000

ggplot(neet_change_2000,
       aes(x = reorder(Continent, change_pp),
           y = change_pp,
           fill = Continent)) +
  geom_col() +
  coord_flip() +
  theme_economist_white() +
  scale_fill_manual(values = continent_colors) +
  ggtitle("Change in NEET from 2000 to 2020 (population-weighted)") +
  xlab("Continent") +
  ylab("Change in NEET (percentage points)") +
  geom_hline(yintercept = 0, linetype = "dashed")

# 13. GRAPH 3 – Change in NEET 2015 → 2020 (bars, weighted)

neet_change_2015 <- neet_2015 %>%
  inner_join(neet_2020, by = "Continent") %>%
  mutate(
    change_pp = neet_2020 - neet_2015
  )

neet_change_2015

ggplot(neet_change_2015,
       aes(x = reorder(Continent, change_pp),
           y = change_pp,
           fill = Continent)) +
  geom_col() +
  coord_flip() +
  theme_economist_white() +
  scale_fill_manual(values = continent_colors) +
  ggtitle("Change in NEET from 2015 to 2020 (population-weighted)") +
  xlab("Continent") +
  ylab("Change in NEET (percentage points)") +
  geom_hline(yintercept = 0, linetype = "dashed")

# 14. Population-weighted youth unemployment

# Attach population to youth unemployment
unemp_pop <- youth_unemp_continent %>%
  inner_join(
    population,
    by = c("Country_Code", "Year")
  )

# Compute weighted unemployment:
# sum(unemp_i * pop_i) / sum(pop_i)
unemp_continent_year <- unemp_pop %>%
  group_by(Continent, Year) %>%
  summarise(
    total_pop       = sum(population, na.rm = TRUE),
    total_unemp_pop = sum(youth_unemployment/100 * population, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    unemp_avg = (total_unemp_pop / total_pop) * 100
  )

# GRAPH 4 – Youth unemployment time trend (weighted)

ggplot(unemp_continent_year,
       aes(x = Year, y = unemp_avg, colour = Continent)) +
  geom_line(linewidth = 0.9) +
  theme_economist_white() +
  scale_color_manual(values = continent_colors) +
  ggtitle("Youth unemployment (15–24) by continent (population-weighted)") +
  xlab("Year") +
  ylab("Youth unemployment rate (%)")

# Youth unemployment values for 2000, 2015, 2020 (weighted)

unemp_2000 <- unemp_continent_year %>%
  filter(Year == 2000) %>%
  select(Continent, unemp_2000 = unemp_avg)

unemp_2015 <- unemp_continent_year %>%
  filter(Year == 2015) %>%
  select(Continent, unemp_2015 = unemp_avg)

unemp_2020 <- unemp_continent_year %>%
  filter(Year == 2020) %>%
  select(Continent, unemp_2020 = unemp_avg)



# GRAPH 5: Change in youth unemployment 2000 → 2020


unemp_change_2000 <- unemp_2000 %>%
  inner_join(unemp_2020, by = "Continent") %>%
  mutate(
    change_pp = unemp_2020 - unemp_2000
  )

unemp_change_2000

ggplot(unemp_change_2000,
       aes(x = reorder(Continent, change_pp),
           y = change_pp,
           fill = Continent)) +
  geom_col() +
  coord_flip() +
  theme_economist_white() +
  scale_fill_manual(values = continent_colors) +
  ggtitle("Change in youth unemployment from 2000 to 2020 (population-weighted)") +
  xlab("Continent") +
  ylab("Change in youth unemployment (percentage points)") +
  geom_hline(yintercept = 0, linetype = "dashed")



# GRAPH 6: Change in youth unemployment 2015 → 2020

unemp_change_2015 <- unemp_2015 %>%
  inner_join(unemp_2020, by = "Continent") %>%
  mutate(
    change_pp = unemp_2020 - unemp_2015
  )

unemp_change_2015

ggplot(unemp_change_2015,
       aes(x = reorder(Continent, change_pp),
           y = change_pp,
           fill = Continent)) +
  geom_col() +
  coord_flip() +
  theme_economist_white() +
  scale_fill_manual(values = continent_colors) +
  ggtitle("Change in youth unemployment from 2015 to 2020 (population-weighted)") +
  xlab("Continent") +
  ylab("Change in youth unemployment (percentage points)") +
  geom_hline(yintercept = 0, linetype = "dashed")
