
setwd("D:/硕士/阿德莱德/data/悉尼降雨量monthly")

library(ggplot2)

raw_data <- read.csv("raw_sydney_rainfall_1885_latest.csv", header = TRUE)


temp_data <- raw_data[, c(3, 4, 5)]
colnames(temp_data) <- c("Year", "Month", "Rainfall")

temp_data$Year <- as.numeric(as.character(temp_data$Year))
temp_data$Rainfall <- as.numeric(as.character(temp_data$Rainfall))

master_data <- subset(temp_data, Year >= 1968 & Year <= 1998)
master_data <- na.omit(master_data)

write.csv(master_data, "sydney_rainfall_1968_1998.csv", row.names = FALSE)

ggplot(master_data, aes(x = Rainfall)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "white", boundary = 0) +
  labs(title = "Distribution of Monthly Rainfall in Sydney (1968-1998)",
       subtitle = "Station: 066006",
       x = "Monthly Rainfall (mm)",
       y = "Number of Months (Frequency)") +
  theme_minimal()

summary(master_data$Rainfall)


ggplot(master_data, aes(sample = Rainfall)) +   
  stat_qq_line(distribution = stats::qnorm, color = "red") +   
  stat_qq(distribution = stats::qnorm, size = 1) +   
  labs(title = "Normal Q-Q Plot: Sydney Rainfall",
       subtitle = "Dots deviating from the red line indicate non-normality",
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +   
  theme_minimal()

wilcox_result <- wilcox.test(master_data$Rainfall, mu = 100)

print(wilcox_result)
