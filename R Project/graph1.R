setwd("c:/Users/tusha/Downloads/archive")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# ------------------------------------------------------------
# DATA CLEANING
city_day <- read_csv("city_day.csv", show_col_types = FALSE)
city_hour <- read_csv("city_hour.csv", show_col_types = FALSE)
station_day <- read_csv("station_day.csv", show_col_types = FALSE)
station_hour <- read_csv("station_hour.csv", show_col_types = FALSE)
stations <- read_csv("stations.csv", show_col_types = FALSE)

df <- city_day

df <- df %>%
  select(City, Date, PM2.5, PM10, NO2, SO2, CO, O3, AQI, AQI_Bucket)

df <- df %>% filter(!is.na(Date), !is.na(City))

df <- df %>% distinct()

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

suppressWarnings({
  numeric_cols <- c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3", "AQI")
  df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
})

df <- df %>%
  mutate(
    Month = month(Date, label = TRUE, abbr = TRUE),
    Year = year(Date)
  )
# ------------------------------------------------------------
# GRAPH 1: Top 10 Most Polluted Cities
# ------------------------------------------------------------
top_cities <- df %>%
  group_by(City) %>%
  summarise(Avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Avg_AQI)) %>%
  arrange(desc(Avg_AQI)) %>%
  head(10)

p1 <- ggplot(top_cities, aes(x = reorder(City, Avg_AQI), y = Avg_AQI)) +
  geom_bar(stat = "identity", fill = "#2c3e50", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Most Polluted Cities",
    x = "City",
    y = "Average AQI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
print(p1)
