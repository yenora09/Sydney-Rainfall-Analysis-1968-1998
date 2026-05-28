setwd("E:/R/Yuliang Fan_AnnualRainfall_Analysis")

rain <- read.csv("IDCJAC0001_066006_Data12.csv")

names(rain)

plot_data <- subset(rain, Year >= 1968 & Year <= 1998, select = c(Year, Annual))

print(plot_data)

plot(
  plot_data$Year,
  plot_data$Annual,
  main = "Annual Rainfall in Sydney Botanic Gardens (1968-1998)",
  xlab = "Year",
  ylab = "Annual rainfall (mm)",
  pch = 19
)

abline(lm(Annual ~ Year, data = plot_data), lwd = 2)

plot_data$Annual <- as.numeric(as.character(plot_data$Annual))
cor(plot_data$Year, plot_data$Annual, use = "complete.obs")

# new
setwd("E:/R/Yuliang Fan_AnnualRainfall_Analysis")

rain <- read.csv("IDCJAC0001_066006_Data12.csv")

names(rain)

plot_data <- subset(
  rain,
  Year >= 1968 & Year <= 1998,
  select = c(Year, Annual)
)


plot_data$Year <- as.numeric(plot_data$Year)
plot_data$Annual <- as.numeric(as.character(plot_data$Annual))


plot_data <- na.omit(plot_data)


print(plot_data)
summary(plot_data$Annual)

# linear regression line
plot(
  plot_data$Year,
  plot_data$Annual,
  main = "Annual Rainfall in Sydney Botanic Gardens (1968–1998)",
  xlab = "Year",
  ylab = "Annual rainfall (mm)",
  pch = 19,
  col = "black"
)

model <- lm(Annual ~ Year, data = plot_data)
abline(model, col = "red", lwd = 2)

grid()

# results
model <- lm(Annual ~ Year, data = plot_data)

summary(model)

confint(model)

cor(plot_data$Year, plot_data$Annual)

cor.test(plot_data$Year, plot_data$Annual)

# QQ plot regre
residuals_model <- residuals(model)

qqnorm(
  residuals_model,
  main = "QQ Plot of Regression Residuals"
)

qqline(
  residuals_model,
  col = "red",
  lwd = 2
)

# Residuals vs fitted values
plot(
  fitted(model),
  residuals(model),
  main = "Residuals vs Fitted Values",
  xlab = "Fitted annual rainfall (mm)",
  ylab = "Residuals",
  pch = 19,
  col = "black"
)

abline(h = 0, col = "red", lwd = 2)
grid()

# new

plot_data$year_center <- plot_data$Year - mean(plot_data$Year)

model_linear_c <- lm(Annual ~ year_center, data = plot_data)

model_quad <- lm(Annual ~ year_center + I(year_center^2), data = plot_data)

summary(model_linear_c)
summary(model_quad)

anova(model_linear_c, model_quad)

AIC(model_linear_c, model_quad)

# Plot linear and quadratic trend lines

year_seq <- seq(
  min(plot_data$Year),
  max(plot_data$Year),
  by = 0.1
)

new_data <- data.frame(
  Year = year_seq,
  year_center = year_seq - mean(plot_data$Year)
)

pred_linear <- predict(model_linear_c, newdata = new_data)
pred_quad <- predict(model_quad, newdata = new_data)

plot(
  plot_data$Year,
  plot_data$Annual,
  main = "Linear and Quadratic Trend Check",
  xlab = "Year",
  ylab = "Annual rainfall (mm)",
  pch = 19,
  col = "black"
)

lines(year_seq, pred_linear, col = "red", lwd = 2)
lines(year_seq, pred_quad, col = "blue", lwd = 2, lty = 2)

legend(
  "topright",
  legend = c("Linear trend", "Quadratic trend"),
  col = c("red", "blue"),
  lwd = 2,
  lty = c(1, 2)
)

grid()

# 9

train_data <- subset(plot_data, Year <= 1988)
test_data <- subset(plot_data, Year >= 1989)

prediction_model <- lm(Annual ~ Year, data = train_data)

test_data$pred_lm <- predict(prediction_model, newdata = test_data)

test_data$pred_mean <- mean(train_data$Annual)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

prediction_results <- data.frame(
  Model = c("Linear regression using Year", "Training mean baseline"),
  RMSE = c(
    rmse(test_data$Annual, test_data$pred_lm),
    rmse(test_data$Annual, test_data$pred_mean)
  ),
  MAE = c(
    mae(test_data$Annual, test_data$pred_lm),
    mae(test_data$Annual, test_data$pred_mean)
  )
)

print(prediction_results)

print(test_data[, c("Year", "Annual", "pred_lm", "pred_mean")])

# Plot actual vs predicted rainfall
plot(
  test_data$Year,
  test_data$Annual,
  main = "Prediction Check for Annual Rainfall (1989–1998)",
  xlab = "Year",
  ylab = "Annual rainfall (mm)",
  pch = 19,
  col = "black",
  ylim = range(c(test_data$Annual, test_data$pred_lm, test_data$pred_mean))
)

lines(test_data$Year, test_data$pred_lm, col = "red", lwd = 2)
lines(test_data$Year, test_data$pred_mean, col = "blue", lwd = 2, lty = 2)

legend(
  "topright",
  legend = c("Actual rainfall", "Linear regression prediction", "Training mean baseline"),
  col = c("black", "red", "blue"),
  pch = c(19, NA, NA),
  lty = c(NA, 1, 2),
  lwd = c(NA, 2, 2)
)

grid()