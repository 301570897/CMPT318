# Load necessary packages
library(dplyr)
library(zoo)
library(ggplot2)

#Read data
data <- read.table("DataAssign1.txt", header = TRUE, sep = ",")

data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")

#Slice the data into complete weeks (Monday-Sunday)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data_weekly <- data %>%
  mutate(Week = as.numeric(format(Date, "%W"))) %>%
  group_by(Week) %>%
  filter(n() >= 7) %>%
  ungroup()

#Compute the moving average for Global_intensity over each complete week
data_weekly$Global_intensity_moving_average <- rollapplyr(data_weekly$Global_intensity, width = 7, mean, align = 'right', fill = NA)

#Calculate the average smoothened week
avg_sw <- data_weekly %>%
  group_by(Datetime) %>%
  summarise(Global_intensity_average = mean(Global_intensity_moving_average, na.rm = TRUE))

#Calculate deviation of each smoothened week from the average smoothened week
data_weekly <- data_weekly %>%
  left_join(avg_sw, by = "Datetime") %>%
  mutate(Global_intensity_deviation = Global_intensity_moving_average - Global_intensity_average)

#Check for missing values and remove them
data_weekly <- na.omit(data_weekly)

#Identify the most and least anomalous weeks
most_anomalous_week <- data_weekly %>%
  arrange(desc(abs(Global_intensity_deviation))) %>%
  slice(1)

least_anomalous_week <- data_weekly %>%
  arrange(abs(Global_intensity_deviation)) %>%
  slice(1)

#Anomaly scores table
anomaly_scores <- data_weekly %>%
  select(Datetime, Global_intensity_average, Global_intensity_deviation) %>%
  rename(Week = Datetime, Average = Global_intensity_average, Deviation = Global_intensity_deviation)

#plot of the average smoothened week and the most and least anomalous weeks
ggplot(data = data_weekly, aes(x = Datetime)) +
  geom_line(aes(y = Global_intensity_average), color = "blue", linetype = "dashed") +
  geom_point(aes(y = Global_intensity_deviation, x = Datetime), color = "green") +
  geom_hline(aes(yintercept = max(Global_intensity_deviation)), color = "yellow", linetype = "dashed") +
  geom_hline(aes(yintercept = min(Global_intensity_deviation)), color = "pink", linetype = "dashed") +
  labs(x = "Datetime", y = "Global Intensity") +
  theme_minimal()



