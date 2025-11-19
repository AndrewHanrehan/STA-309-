library(tidyverse)
library(janitor)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

production <- read_csv("https://raw.githubusercontent.com/mahsaashouri/STA309-Dataset/main/Milk-Production-Consumption/milk-production-tonnes.csv")

consumption <- read_csv("https://raw.githubusercontent.com/mahsaashouri/STA309-Dataset/main/Milk-Production-Consumption/per-capita-milk-consumption.csv")

head(production)
head(consumption)

world_map <- map_data("world") %>%
  rename(Entity = region)

head(world_map)

prod_latest <- production %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  rename(Production = `Milk Production (tonnes)`)

map_prod <- world_map %>%
  left_join(prod_latest, by = "Entity")

Global_Milk_Production <- ggplot(map_prod, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Production), color = "gray40", size = 0.1) +
  scale_fill_viridis_c(option = "magma", na.value = "lightgray") +
  coord_map("mercator") +
  labs(
    title = "Global Milk Production (Latest Year)",
    fill = "Tonnes"
  ) +
  theme_minimal()

Global_Milk_Production

cons_latest <- consumption %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  rename(Consumption = 'Milk consumption (kilograms per year per capita)')

map_cons <- world_map %>%
  left_join(cons_latest, by = "Entity")

Global_Milk_Consumption <- ggplot(map_cons, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Consumption), color = "gray40", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgray") +
  coord_map("mercator") +
  labs(
    title = "Global Milk Consumption per Capita (Latest Year)",
    fill = "kg/person"
  ) +
  theme_minimal()

Global_Milk_Consumption

production_trend <- production %>%
  group_by(Year) %>%
  summarize(GlobalProduction = sum(`Milk Production (tonnes)`, na.rm = TRUE))

ggplot(production_trend, aes(x = Year, y = GlobalProduction)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue") +
  labs(
    title = "Global Milk Production Over Time",
    x = "Year",
    y = "Total Production (tonnes)"
  ) +
  theme_minimal()

consumption_trend <- consumption %>%
  group_by(Year) %>%
  summarize(AverageConsumption = mean(`Milk consumption (kilograms per year per capita)`, na.rm = TRUE))

ggplot(consumption_trend, aes(x = Year, y = AverageConsumption)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Average Milk Consumption Per Capita Over Time",
    x = "Year",
    y = "kg per capita"
  ) +
  theme_minimal()

ggplot(trend, aes(x = Year)) +
  
  geom_line(aes(y = GlobalProduction), 
            color = "steelblue", size = 1.2) +
  
  geom_line(aes(y = AverageConsumption * 5e6),    # adjust factor if needed
            color = "darkgreen", size = 1.2) +
  
  scale_y_continuous(
    name = "Global Production (tonnes)",
    sec.axis = sec_axis(~ . / 5e6,
                        name = "Avg Consumption (kg per capita)")
  ) +
  
  labs(
    title = "Global Milk Production vs. Consumption Over Time",
    x = "Year"
  ) +
  theme_minimal()
