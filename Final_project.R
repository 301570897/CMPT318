install.packages(c("lubridate", "ggcorrplot", "zoo", "dplyr", "TTR", "ggplot2","colorspace","depmixS4", "ggbiplot","crayon"))
m= #Packages
library(depmixS4)
library(zoo)
library(dplyr)
library(lubridate)
library(ggcorrplot)
library(ggbiplot)

#Organizing Data
Data <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
head(Data)
#Feature Scaling
#Removing NA's
which(is.na(Data))
Data <- Data %>% mutate(Global_active_power = na.approx(Global_active_power))
Data <- Data %>% mutate(Global_reactive_power = na.approx(Global_reactive_power))
Data <- Data %>% mutate(Voltage = na.approx(Voltage))
Data <- Data %>% mutate(Global_intensity = na.approx(Global_intensity))
Data <- Data %>% mutate(Sub_metering_1 = na.approx(Sub_metering_1))
Data <- Data %>% mutate(Sub_metering_2 = na.approx(Sub_metering_2))
Data <- Data %>% mutate(Sub_metering_3 = na.approx(Sub_metering_3))

which(is.na(Data))
# Check again for any remaining NAs
if(anyNA(Data)) {
  Data <- na.omit(Data)  # Remove rows with NAs
}

#Normalizing the Dataset
#Z scores (Normalized Values for all data)
#Global_active_power
GAPMean <- mean(Data$Global_active_power,na.rm = TRUE)
GAPSD<- sd(Data$Global_active_power,na.rm = TRUE)
GAPz <- ((Data$Global_active_power - GAPMean) / GAPSD)

#Global_reactive_power
GRPMean <- mean(Data$Global_reactive_power,na.rm = TRUE)
GRPSD<- sd(Data$Global_reactive_power,na.rm = TRUE)
GRPz <- ((Data$Global_reactive_power - GRPMean) / GRPSD)

#Voltage
VoltMean <- mean(Data$Voltage,na.rm = TRUE)
VoltSD<- sd(Data$Voltage,na.rm = TRUE)
Voltz <- ((Data$Voltage - VoltMean) / VoltSD)

#Global_intensity
GIMean <- mean(Data$Global_intensity,na.rm = TRUE)
GISD<- sd(Data$Global_intensity,na.rm = TRUE)
GIz <- ((Data$Global_intensity - GIMean) / GISD)

#Sub_metering_1
SM1Mean <- mean(Data$Sub_metering_1,na.rm = TRUE)
SM1SD<- sd(Data$Sub_metering_1,na.rm = TRUE)
SM1z <- ((Data$Sub_metering_1 - SM1Mean) / SM1SD)

#Sub_metering_2
SM2Mean <- mean(Data$Sub_metering_2,na.rm = TRUE)
SM2SD<- sd(Data$Sub_metering_2,na.rm = TRUE)
SM2z <- ((Data$Sub_metering_2 - SM2Mean) / SM2SD)

#Sub_metering_3
SM3Mean <- mean(Data$Sub_metering_3,na.rm = TRUE)
SM3SD<- sd(Data$Sub_metering_3,na.rm = TRUE)
SM3z <- ((Data$Sub_metering_3 - SM3Mean) / SM3SD)

#Standarized Proof
GRPMeanCloseto0 <- mean(GRPz,na.rm = TRUE)
GRPSDCloseto1<- sd(GRPz,na.rm = TRUE)

GAPMeanCloseto0 <- mean(GAPz,na.rm = TRUE)
GAPDCloseto1<- sd(GAPz,na.rm = TRUE)

GIMeanCloseto0 <- mean(GIz,na.rm = TRUE)
GISDCloseto1<- sd(GIz,na.rm = TRUE)

VoltMeanCloseto0 <- mean(Voltz,na.rm = TRUE)
VoltSDCloseto1<- sd(Voltz,na.rm = TRUE)

SM1MeanCloseto0 <- mean(SM1z,na.rm = TRUE)
SM1SDCloseto1<- sd(SM1z,na.rm = TRUE)
SM2MeanCloseto0 <- mean(SM2z,na.rm = TRUE)
SM2SDCloseto1<- sd(SM2z,na.rm = TRUE)
SM3MeanCloseto0 <- mean(SM3z,na.rm = TRUE)
SM3SDCloseto1<- sd(SM3z,na.rm = TRUE)

#Feature Engineering
scaled_data <- data.frame(GAPz, GRPz, Voltz, GIz, SM1z, SM2z, SM3z)

# Choose the weekday
weekday <- "Monday"

# Convert the Date column format
Data$Date <- as.Date(Data$Date, format = "%d/%m/%Y")
Data$Time <- as.POSIXct(Data$Time, format = '%H:%M:%S')

# Add a new column for the day of the week
Data$DayOfWeek <- weekdays(as.Date(Data$Date))

# Replace NA values by interpolation
scaled_data <- scaled_data %>%
  mutate(across(c('GAPz', 'GRPz', 'Voltz', 'GIz', 'SM1z', 'SM2z', 'SM3z'), ~ na.approx(.x, na.rm = FALSE, rule=2)))

# Defining time window 
start_time <- as.POSIXct("06:00:00", format = "%H:%M:%OS")
end_time <- as.POSIXct("10:00:00", format = "%H:%M:%OS")

# Extract the time window
time_window <- Data %>%
  filter(DayOfWeek == weekday, Time >= start_time, Time <= end_time) %>%
  select(Time, Global_active_power, Global_reactive_power, Voltage, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3) 

# Calculate the mean of the time window data
mean_time <- aggregate(.~Time, data=time_window, FUN=function(x) c(mean=mean(x)))
mean_time <- subset(mean_time, select = -c(Time))

# Standardize the data set
st_data <- mean_time %>% mutate_at(c('Global_active_power', 'Global_reactive_power', 'Voltage','Global_intensity','Sub_metering_1','Sub_metering_2','Sub_metering_3'), ~(scale(.) %>% as.vector))

pca_result <- prcomp(st_data, scale. = TRUE)

# Analyze the variance explained by each principal component
summary(pca_result)

# Calculating the percentage of variance
var_per <- pca_result$sdev^2
var_per <- round(var_per / sum(var_per) * 100, 1)

result <- list("pca" = pca_result, "var_per" = var_per)

# loadings for PC1
pc1_loadings <- pca_result$rotation[, 1]

# Create a data frame with response variable names and their corresponding loading scores for PC1
response_variables <- c('Global_active_power', 'Global_reactive_power', 'Voltage', 
                        'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')

pc1_loading_scores <- data.frame(Response_Variable = response_variables, PC1_Loading = pc1_loadings)

# Plot 
ggbiplot(pca_result, choices = c(1, 2), var.axes = TRUE) 
barplot(var_per, names.arg = paste("PC", 1:length(var_per)), 
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Parcentage variation",
        col = "skyblue",
        ylim = c(0, max(var_per) * 1.2))

text(1:length(var_per), var_per, labels = var_per, pos = 3, cex = 0.8, col = "black")

# Preparing for Plotting
# graphics.off()  # Close all previous graphics devices
# grid.newpage() # Start a new page for grid graphics

#HMM Training and Testing

# Declare the first date of the test data
testStartDate <- "1/1/2010"

# Make a new column for the day of the week: 0-6, starting with Sunday
Data$Day_of_week <- as.POSIXlt(Data$Date, format = "%d/%m/%Y")$wday

# Keep only the monday rows
Data <- Data[Data$Day_of_week == 1 ,]

# Keep only the rows from 6:00 to 9:59 
Data <- Data[as.POSIXlt(Data$Time, format = "%H:%M:%S") >= as.POSIXlt(start_time, format = "%H:%M:%S") & 
               as.POSIXlt(Data$Time, format = "%H:%M:%S") < as.POSIXlt(end_time, format = "%H:%M:%S"),]

# Interpolate the NA values for each of the columns
for(col in response_variables){
  Data[, col] <- na.approx(Data[, col], maxgap = Inf, rule = 2)
}

# Split the Data into training data and testing data
train <- Data[as.POSIXlt(Data$Date, format = "%d/%m/%Y") < as.POSIXlt(testStartDate, format = "%d/%m/%Y"),]
test <- Data[as.POSIXlt(Data$Date, format = "%d/%m/%Y") >= as.POSIXlt(testStartDate, format = "%d/%m/%Y"),]

train$Time <- as.POSIXct(strptime(train$Time, format="%H:%M:%S"))

## Training Univariate HMM

# Compute the log-likelihood and BIC for i states and determine the best result
modTrainUni <- depmix(Global_active_power~1, data = train, nstates = 8)

fmodTrainUni <- fit(modTrainUni) 

print(fmodTrainUni)

#Testing Univariate Model
modTestUni <- depmix(response = Global_active_power ~ 1, data = test, nstates = 8)
summary(modTestUni)
fmodTestUni <- setpars(modTestUni,getpars(fmodTrainUni))
summary(fmodTestUni)
logLik(fmodTestUni)

normalizedTrainUniLogLik <- logLik(fmodTrainUni) / nrow(train)
normalizedTestUniLogLik <- logLik(fmodTestUni) / nrow(test)

print(normalizedTrainUniLogLik)
print(normalizedTestUniLogLik)


#Anomaly Detection


#partitioning the dataset into 10 subsets
test_subsets <- split(test, cut(as.Date(test$Date), breaks = 10))

# Initializing a vector to store log-likelihoods for each subset
log_likelihoods <- numeric(length(test_subsets))

log_likelihoods <- sapply(test_subsets, function(subset) {
  if(nrow(subset) > 0) {
    model <- depmix(response = Global_active_power ~ 1, data = subset, nstates = 8)
    model <- setpars(model, getpars(fmodTrainUni))
    fit(model) -> fitted
    return(logLik(fitted))
  } else {
    return(NA)  # NA handling for empty subsets
  }
})

# Calculate threshold and detect anomalies
threshold <- mean(log_likelihoods, na.rm = TRUE) - 0.5 * sd(log_likelihoods, na.rm = TRUE)
anomalies <- which(log_likelihoods < threshold)

# Print outputs
print(threshold)
print(log_likelihoods)
print(anomalies)

# Plotting log-likelihoods with the threshold line to visually identify anomalies
plot(log_likelihoods, type = 'h', main = "Log-Likelihoods of HMM Fits", xlab = "Subset Index", ylab = "Log-Likelihood")
abline(h = threshold, col = "red", lty = 2, lwd = 2)
points(anomalies, log_likelihoods[anomalies], col = "red", pch = 19)
