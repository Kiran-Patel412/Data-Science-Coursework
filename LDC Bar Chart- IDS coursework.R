library(readr)    # read_csv
library(dplyr)    # filter, select, mutate, arrange, group_by, summarise, etc.
library(ggplot2)  # ggplot, geom_bar


setwd("UN LDC data.csv")
# GDP per capita data
gdp <- read_csv("gdp-per-capita-worldbank.csv")


gdp <- gdp %>%
  select(Entity, Code, Year,
         gdp_pc_ppp = `GDP per capita, PPP (constant 2017 international $)`)

# Continents
continents <- read_csv("continents-according-to-our-world-in-data.csv") %>%
  select(Code, Continent) %>%
  distinct()

# UN LDC list
ldc_raw <- read_csv("UN LDC data.csv")



ldc_codes <- ldc_raw %>%
  filter(Status == "LDC") %>%   # use Status column
  select(Code) %>%
  distinct()

## 3. Keep only LDCs in the GDP data -------------------------------------

gdp_ldc <- gdp %>%
  inner_join(ldc_codes, by = "Code")

## 4. Create GDP_lag and GDP growth per year -----------------------------

gdp_ldc_growth <- gdp_ldc %>%
  arrange(Code, Year) %>%             # sort within each country
  group_by(Code) %>%
  mutate(
    GDP_lag    = lag(gdp_pc_ppp),     # previous year's GDP per capita
    gdp_growth = (gdp_pc_ppp - GDP_lag) / GDP_lag * 100
  ) %>%
  ungroup()

## 5. Add continent information ------------------------------------------

gdp_ldc_growth <- gdp_ldc_growth %>%
  left_join(continents, by = "Code")



ldc_growth_7 <- gdp_ldc_growth %>%
  filter(Year >= 2016,
         Year <= 2021,
         !is.na(Continent),
         gdp_growth >= 7)



ldc_counts <- ldc_growth_7 %>%
  group_by(Year, Continent) %>%
  summarise(
    n_ldc = length(unique(Code))
  ) %>%
  ungroup()



ggplot(ldc_counts,
       aes(x = factor(Year),
           y = n_ldc,
           fill = Continent)) +
  geom_bar(stat = "identity") +
 labs(
    x = "Year",
    y = "Number of LDCs with >= 7% GDP growth",
    fill = "Continent",
    title = "LDCs with >= 7% GDP per capita growth by continent (2016–2021)"
)