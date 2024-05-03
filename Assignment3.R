library(lubridate)
library(ggcorrplot)
library(zoo)
library(dplyr)
library(TTR)
library(ggplot2)
library(depmixS4)
# Q1

wkDays <- c("Mondays", "Tuesdays", "Wednesdays", "Thursdays", "Fridays", "Saturdays", "Sundays")
timeWindowStart <- "07:00:00"
timeWindowEnd <- "10:00:00"

df <- read.table('Group_Assignment_Dataset.txt', header = TRUE, sep = ",")


# Make a new column for the day of the week: 0-6, starting with Sunday
df$Day_of_week <- as.POSIXlt(df$Date, format = "%d/%m/%Y")$wday

# Convert the time column of the data into a datetime (for the graph output later)
df$Time <- as.POSIXct(strptime(df$Time, format="%H:%M:%S"))

# Make a list of dataframes where each dataframe contains all of a day of the week (from Monday to Sunday)
wkData <- list(df[df$Day_of_week == 1,], df[df$Day_of_week == 2,], df[df$Day_of_week == 3,], df[df$Day_of_week == 4,], df[df$Day_of_week == 5,], df[df$Day_of_week == 6,], df[df$Day_of_week == 7,])

for(i in 1:6){
  # Compute aggregate means for the global active power 
  dayData <- aggregate(Global_active_power ~ Time, data = wkData[[i]], mean)
  
  # Convert our table into a dataframe
  dayData <- as.data.frame(dayData)
  
  p <- ggplot(data = dayData, aes(x = Time, y = Global_active_power)) + 
    geom_point() +
    scale_x_datetime(date_breaks = "3 hour", date_labels = "%I:%M %p") +
    ggtitle(paste(wkDays[i], "- Global Active Power"))
  print(p)
}

# After looking at the graphs for each day of the week, we choose 7:00 to 10:00 on Wednesdays as our time window..
timeWindowData <- wkData[[3]][as.POSIXlt(wkData[[3]]$Time, format = "%H:%M:%S") >= as.POSIXlt(timeWindowStart, format = "%H:%M:%S") & as.POSIXlt(wkData[[3]]$Time, format = "%H:%M:%S") < as.POSIXlt(timeWindowEnd, format = "%H:%M:%S"),]

# Compute aggregate data for the time window
aggTimeWindowData <- aggregate(Global_active_power ~ Time, data = timeWindowData, mean)

# Convert our table into a dataframe
aggTimeWindowData <- as.data.frame(aggTimeWindowData)

p <- ggplot(data = aggTimeWindowData, aes(x = Time, y = Global_active_power)) + 
  geom_point() +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%I:%M %p") +
  ggtitle("Time window - Global Active Power")
print(p)


set.seed(1)

# Compute the log-likelihood and BIC for i states and determine the best result
newDataL <- list()
newDataB <- list()
for(i in c(4, 8, 12, 16)){
  print(paste("Computing the log-likelihood and BIC for", i, "states."))
  mod <- depmix(response = Global_active_power ~ 1, data = timeWindowData, nstates = i)
  fm <- fit(mod)
  newDataL <- c(newDataL, list(c(i, logLik(fm))))
  newDataB <- c(newDataB, list(c(i, BIC(fm))))
  
  print(fm)
}

newDataL1 <- list()
newDataB1 <- list()
for(i in c(9, 10, 11, 13, 14, 15)){
  print(paste("Computing the log-likelihood and BIC for", i, "states."))
  mod <- depmix(response = Global_active_power ~ 1, data = timeWindowData, nstates = i)
  fm <- fit(mod)
  newDataL1 <- c(newDataL1, list(c(i, logLik(fm))))
  newDataB1 <- c(newDataB1, list(c(i, BIC(fm))))
  
  print(fm)
}

# Convert lists to data
newDataL <- do.call(rbind, newDataL)
newDataB <- do.call(rbind, newDataB)
newDataL1 <- do.call(rbind, newDataL1)
newDataB1 <- do.call(rbind, newDataB1)

# Rename columns
colnames(newDataL) <- c("States", "LogLikelihood")
colnames(newDataB) <- c("States", "BIC")
colnames(newDataL1) <- c("States", "LogLikelihood")
colnames(newDataB1) <- c("States", "BIC")

newDataL <- as.data.frame(newDataL)
newDataB <- as.data.frame(newDataB)
newDataL1 <- as.data.frame(newDataL1)
newDataB1 <- as.data.frame(newDataB1)

P1 <- ggplot(newDataL, aes(x = States, y = LogLikelihood)) +
  geom_line(color = "blue") +
  labs(x = "Number of States", y = "Log-Likelihood", title = "Log-Likelihood vs Number of States")

# Plot BIC
P2 <- ggplot(newDataB, aes(x = States, y = BIC)) +
  geom_line(color = "red") +
  labs(x = "Number of States", y = "BIC", title = "BIC vs Number of States")
P3 <- ggplot(newDataL1, aes(x = States, y = LogLikelihood)) +
  geom_line(color = "blue") +
  labs(x = "Number of States", y = "Log-Likelihood", title = "Log-Likelihood vs Number of States")

# Plot BIC
P4 <- ggplot(newDataB1, aes(x = States, y = BIC)) +
  geom_line(color = "red") +
  labs(x = "Number of States", y = "BIC", title = "BIC vs Number of States")

# print the plots
print(P1)
print(P2)
print(P3)
print(P4)

#Q2
# Discretizing the Global_active_power variable to the nearest half-integer
timeWindowData$Discrete_Global_active_power <- round(timeWindowData$Global_active_power * 2) / 2

# Storage for discretized model results
discreteDataL <- list()
discreteDataB <- list()

# Fitting HMM models to the discretized data and calculate log-likelihood and BIC
for(i in unique(c(4, 8, 12, 16))){
  cat("Fitting discretized HMM for", i, "states...\n")
  mod_discrete <- depmix(response = Discrete_Global_active_power ~ 1, data = timeWindowData, nstates = i)
  fm_discrete <- fit(mod_discrete, mehod = "optimx")
  
  # Store results
  discreteDataL <- c(discreteDataL, list(c(i, logLik(fm_discrete))))
  discreteDataB <- c(discreteDataB, list(c(i, BIC(fm_discrete))))
  print(fm_discrete)
}


# Storage for discretized model results
discreteDataL1 <- list()
discreteDataB1 <- list()
for(i in unique(c(9, 10, 11, 13, 14, 15))){
  cat("Fitting discretized HMM for", i, "states...\n")
  mod_discrete <- depmix(response = Discrete_Global_active_power ~ 1, data = timeWindowData, nstates = i)
  fm_discrete <- fit(mod_discrete, methid = "optimx")
  
  discreteDataL1 <- c(discreteDataL1, list(c(i, logLik(fm))))
  discreteDataB1 <- c(discreteDataB1, list(c(i, BIC(fm))))
  
  print(fm_discrete)
}
# Converting lists to data frames for discretized data
discreteDataL <- as.data.frame(do.call(rbind, discreteDataL))
discreteDataB <- as.data.frame(do.call(rbind, discreteDataB))
discreteDataL1 <- as.data.frame(do.call(rbind, discreteDataL1))
discreteDataB1 <- as.data.frame(do.call(rbind, discreteDataB1))
# Renaming columns
colnames(discreteDataL) <- c("States", "LogLikelihood")
colnames(discreteDataB) <- c("States", "BIC")
colnames(discreteDataL1) <- c("states", "LogLikelihood")
colnames(discreteDataB1) <- c("States", "BIC")

# Combining continuous and discretized data for comparison
combinedDataL <- rbind(newDataL, newDataL1, setNames(discreteDataL, colnames(newDataL)))
combinedDataB <- rbind(newDataB, newDataB1, setNames(discreteDataB, colnames(newDataB)))
combinedDataL1 <- rbind(newDataL, newDataL1, setNames(discreteDataL1, colnames(newDataL1)))
combinedDataB1 <- rbind(newDataB, newDataB1, setNames(discreteDataB1, colnames(newDataB1)))
# Marking the data
combinedDataL$type <- rep(c("Continuous", "Discrete"), each = nrow(combinedDataL)/2)
combinedDataB$type <- rep(c("Continuous", "Discrete"), each = nrow(combinedDataB)/2)
combinedDataL1$type <- rep(c("Continuous", "Discrete"), each = nrow(combinedDataL1)/2)
combinedDataB1$type <- rep(c("Continuous", "Discrete"), each = nrow(combinedDataB1)/2)

# Plotting the results
P5 <- ggplot(combinedDataL, aes(x = States, y = LogLikelihood, color = type)) +
  geom_line() +
  labs(x = "Number of States", y = "Log-Likelihood", title = "Log-Likelihood: Continuous vs Discretized")

P6 <- ggplot(combinedDataB, aes(x = States, y = BIC, color = type)) +
  geom_line() +
  labs(x = "Number of States", y = "BIC", title = "BIC: Continuous vs Discretized")
P7 <- ggplot(combinedDataL1, aes(x = States, y = LogLikelihood, color = type)) +
  geom_line() +
  labs(x = "Number of States", y = "Log-Likelihood", title = "Log-Likelihood: Continuous vs Discretized")

P8 <- ggplot(combinedDataB1, aes(x = States, y = BIC, color = type)) +
  geom_line() +
  labs(x = "Number of States", y = "BIC", title = "BIC: Continuous vs Discretized")
# Print the plots
print(P5)
print(P6)
print(P7)
print(P8)
