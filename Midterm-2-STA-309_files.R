## Andrew Hanrehan
## Midterm 2
## 11/19/2025
library(tidyverse)
library(janitor)
library(plotly)
library(leaflet)
library(maps)
library(readxl)
library(tidytext)
library(wordcloud2)
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
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue") +
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
  geom_line(color = "green", size = 1.2) +
  geom_point(color = "green") +
  labs(
    title = "Average Milk Consumption Per Capita Over Time",
    x = "Year",
    y = "kg per capita"
  ) +
  theme_minimal()

production_trend <- production %>%
  group_by(Year) %>%
  summarize(GlobalProduction = sum(`Milk Production (tonnes)`, na.rm = TRUE))

consumption_trend <- consumption %>%
  group_by(Year) %>%
  summarize(AverageConsumption = mean(`Milk consumption (kilograms per year per capita)`, na.rm = TRUE))

milk_data <- production_trend %>%
  inner_join(consumption_trend, by = "Year")

ggplot(milk_data, aes(x = Year)) +
  geom_line(aes(y = GlobalProduction), size = 1.2) +
  geom_line(aes(y = AverageConsumption * 5e6), size = 1.2) +
  scale_y_continuous(
    name = "Global Milk Production (tonnes)",
    sec.axis = sec_axis(~ . / 5e6,
                        name = "Average Consumption (kg per person)")) +
  labs(
    title = "Global Milk Production vs. Consumption Over Time",
    x = "Year") +
  theme_minimal()



spotify <- read_csv("taylor_swift_spotify_data.csv")%>%
  mutate(`Song Name` = `Song Name` %>%
           str_to_lower() %>%
           str_replace_all("[^a-z0-9]", "") %>%
           str_squish(),
         Album = str_trim(Album))
  
spotify_albums <- spotify %>%
  filter(Album %in% c("Lover", "folklore")) %>%
  select(`Song Name`, Album, Energy, Key)

lyrics <- read_excel("Taylor_Swift_Words_Data (1).xlsx")%>%
  mutate(`Song Name` = `Song Name` %>%
           str_to_lower() %>%
           str_replace_all("[^a-z0-9 ]", "") %>%
           str_squish())

clean_lyrics <- lyrics %>%
  pivot_longer(
    cols = -`Song Name`,
    names_to = "word",
    values_to = "count"
  ) %>%
  filter(count > 0)

clean_lyrics <- clean_lyrics %>%
  inner_join(spotify_albums, by = "Song Name")

clean_lyrics <- clean_lyrics %>%
  anti_join(stop_words, by = "word")

sentiment_data <- clean_lyrics %>%
  inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
  group_by(Album, sentiment) %>%
  summarize(total = sum(count), .groups = "drop")

lover_words <- clean_lyrics %>%
  filter(Album == "Lover") %>%
  group_by(word) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total))

wordcloud2(lover_words, size = 1.0)

folklore_words <- clean_lyrics %>%
  filter(Album == "folklore") %>%
  group_by(word) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total))

wordcloud2(folklore_words, size = 1.0)

song_audio_sentiment <- sentiment_data %>%
  inner_join(spotify, by = "Album")

ggplot(song_audio_sentiment, aes(x = Energy, y = total, color = sentiment)) +
  geom_point() +
  labs(title = "Song Sentiment vs Energy")

ggplot(song_audio_sentiment, aes(x = Key, y = total, color = sentiment)) +
  geom_point() +
  labs(title = "Song Sentiment vs Key")

ggplot(sentiment_data, aes(x = Album, y = total, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment Comparison: Lover vs Folklore",
       x = "Album", y = "Word Count") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
  theme_minimal()


