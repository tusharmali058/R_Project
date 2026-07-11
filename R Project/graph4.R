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
# GRAPH 4: AQI Category Distribution
# ------------------------------------------------------------
aqi_cat <- df %>%
  filter(!is.na(AQI_Bucket)) %>%
  group_by(AQI_Bucket) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(desc(AQI_Bucket)) %>%
  mutate(Label_Pos = cumsum(Percentage) - 0.5 * Percentage)

p4 <- ggplot(aqi_cat, aes(x = "", y = Percentage, fill = AQI_Bucket)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = Label_Pos, label = paste0(round(Percentage, 1), "%")), 
            size = 4, color = "black") +
  labs(
    title = "AQI Category Distribution",
    fill = "AQI Category"
  ) +
  theme_void(base_size = 12) +
  scale_fill_brewer(palette = "Pastel1") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    legend.title = element_text(face = "bold")
  )
print(p4)