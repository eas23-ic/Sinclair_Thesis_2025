
#set wd
setwd("C:/Users/User/Downloads")

# Load necessary libraries
library(readr)
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)

#Load in data
weather_data <- read_csv("Automated Weather Station Daily 1984-2024.csv",
                         locale = locale(encoding = "Latin1"))

#Unsure that date strings are read as dates
weather_data <- weather_data %>%
  mutate(Date = dmy(`Time(UTC)`))

#Filter down the dataset to only only date and air temperature columns, renaming for clarity
weather_data <- weather_data %>%
  rename(
    Time = `Time(UTC)`,
    AirTemp = `AirTemperature (Â°C)`
  ) %>%
  select(Date, AirTemp)

#Filter for relavant years
weather_data <- weather_data %>%
  filter(year(Date) %in% c(2018, 2019, 2021, 2022, 2023, 2024))

#Filter dates for only those within the bumblebee season (19/05 to 19/08)

weather_data <- weather_data %>%
  filter(
    month(Date) >= 5 & month(Date) <= 8,
    !(month(Date) == 5 & day(Date) < 19),
    !(month(Date) == 8 & day(Date) > 19)
  )

#Creating the plot

library(ggplot2)
library(zoo)

# Add Year and DayOfYear columns
weather_data <- weather_data %>%
  mutate(
    Year = factor(year(Date)),
    DayOfYear = yday(Date)
  )

# Create the line plot
ggplot(weather_data, aes(x = DayOfYear, y = AirTemp, color = Year)) +
  geom_line(size = 1) +
  labs(
    title = "Air Temperature During Bumblebee Season (2018–2024)",
    x = "Day of Year (May 19 – August 19)",
    y = "Air Temperature (°C)",
    color = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(yday(ymd("2023-05-19")), yday(ymd("2023-08-19")), by = 7),
    labels = format(seq(ymd("2023-05-19"), ymd("2023-08-19"), by = "1 week"), "%b %d")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#line graph with 7 day rolling averages to show more distinction in the years

# Calculate 7-day rolling mean for each year
weather_smooth <- weather_data %>%
  group_by(Year) %>%
  arrange(DayOfYear) %>%
  mutate(RollingTemp = rollmean(AirTemp, k = 7, fill = NA, align = "center")) %>%
  ungroup()

# Plot
ggplot(weather_smooth, aes(x = DayOfYear, y = RollingTemp, color = Year)) +
  geom_line(size = 1) +
  labs(
    x = "Day of Year (May 19 – August 19)",
    y = "7-Day Average Air Temperature (°C)",
    color = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(yday(ymd("2023-05-19")), yday(ymd("2023-08-19")), by = 7),
    labels = format(seq(ymd("2023-05-19"), ymd("2023-08-19"), by = "1 week"), "%b %d")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#Displaying years temperature as a box-plot

# Create the box plot
ggplot(weather_data, aes(x = Year, y = AirTemp, fill = Year)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
  labs(
    x = "Year",
    y = "Air Temperature (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#Stats tests to see which years are hot/cold

# Ensure 'Year' is a factor
weather_data <- weather_data %>%
  mutate(Year = factor(year(Date)))

# Run ANOVA: AirTemp by Year
anova_result <- aov(AirTemp ~ Year, data = weather_data)

# View ANOVA summary
summary(anova_result)

# Run Tukey HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)

# View Tukey HSD results
print(tukey_result)

# Optional: plot Tukey results for visualization
plot(tukey_result)

#calculating general stats

summary_stats <- weather_data %>%
  group_by(Year) %>%
  summarise(
    Mean_Temp = mean(AirTemp, na.rm = TRUE),
    Median_Temp = median(AirTemp, na.rm = TRUE),
    SD_Temp = sd(AirTemp, na.rm = TRUE),
    Min_Temp = min(AirTemp, na.rm = TRUE),
    Max_Temp = max(AirTemp, na.rm = TRUE),
    N = n()
  ) %>%
  arrange(Year)

# View the results
print(summary_stats)

#Calculating days above 10 degrees for each bumblebee season

hot_days_by_year <- weather_data %>%
  group_by(Year) %>%
  summarise(Days_Above_12C = sum(AirTemp >= 12.5, na.rm = TRUE)) %>%
  arrange(Year)

# View result
print(hot_days_by_year)
