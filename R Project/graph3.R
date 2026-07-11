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
# GRAPH 3: Pollutant Comparison
# ------------------------------------------------------------
pollutant_data <- data.frame(
  Group = "Average",
  Pollutant = c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3"),
  Concentration = c(
    mean(df$PM2.5, na.rm = TRUE),
    mean(df$PM10, na.rm = TRUE),
    mean(df$NO2, na.rm = TRUE),
    mean(df$SO2, na.rm = TRUE),
    mean(df$CO, na.rm = TRUE),
    mean(df$O3, na.rm = TRUE)
  )
)

# Convert Pollutant to factor to maintain specific order
pollutant_data$Pollutant <- factor(pollutant_data$Pollutant, levels = c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3"))

p3 <- ggplot(pollutant_data, aes(x = Group, y = Concentration, fill = Pollutant)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(Concentration, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Pollutant Comparison",
    x = "",
    y = "Average Concentration",
    fill = "Pollutant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(face = "bold")
  )
print(p3)