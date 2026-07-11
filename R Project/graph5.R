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
# GRAPH 5: Monthly Average AQI
# ------------------------------------------------------------
monthly_aqi <- df %>%
  filter(!is.na(Month)) %>%
  group_by(Month) %>%
  summarise(Avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(monthly_aqi, aes(x = Month, y = Avg_AQI, group = 1)) +
  geom_line(color = "#2980b9", size = 1.2) +
  geom_point(color = "#e67e22", size = 3) +
  labs(
    title = "Monthly Average AQI",
    x = "Month",
    y = "Average AQI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
print(p5)
