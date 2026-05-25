# 1. Column bar diagram for average monthly rainfall
library(ggplot2)

data <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  rainfall = c(130.9, 132.6, 149.2, 119.5, 115.2, 128.1,
               66.7, 94.4, 77.0, 81.3, 108.2, 76.1)
)

data$month <- factor(
  data$month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

ggplot(data, aes(x = month, y = rainfall)) +
  geom_col(fill = "deepskyblue", color = "black", width = 0.7) +
  geom_text(aes(label = rainfall), vjust = -0.3, size = 3.5) +
  labs(
    title = "Average Rainfall from January to December and Annual Rainfall",
    x = "Month",
    y = "Average Rainfall (mm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2. Normal QQ plots for monthly rainfall analysis
library(ggplot2)

raw_data_text <- "
Year Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
1968 132.7 20.2 115.4 16.0 116.7 23.9 64.3 28.7 5.1 4.4 18.1 86.9
1969 53.0 251.2 100.8 186.5 49.4 204.0 51.0 164.7 85.3 42.9 248.0 35.5
1970 142.8 49.3 203.4 58.5 10.6 40.7 1.8 34.4 165.4 20.3 127.5 274.4
1971 132.2 181.2 78.2 84.6 114.6 70.2 19.6 180.6 54.6 2.5 76.0 129.5
1972 400.7 110.3 201.0 70.6 96.3 112.0 4.1 34.9 11.1 170.0 63.5 42.8
1973 279.8 351.1 108.5 77.4 28.5 73.8 107.6 91.2 72.6 178.6 121.8 33.4
1974 200.6 44.0 284.6 164.6 229.5 220.5 17.4 175.8 46.4 74.7 115.2 33.4
1975 47.6 140.0 388.6 78.0 18.6 379.2 49.0 25.8 49.6 115.0 32.2 37.4
1976 293.6 221.0 304.3 50.8 22.9 155.4 153.4 38.0 135.4 286.0 112.6 23.6
1977 99.0 174.4 256.8 25.8 133.2 111.4 10.0 20.6 49.0 20.0 36.0 30.8
1978 225.4 15.4 250.2 141.0 118.2 301.8 19.4 37.2 113.6 88.3 96.8 93.0
1979 122.1 16.1 200.2 24.6 121.2 169.4 26.4 9.2 28.0 50.0 91.8 2.6
1980 169.7 81.4 76.2 22.6 132.1 47.4 52.4 14.8 5.5 34.1 41.2 50.2
1981 64.4 208.0 38.2 133.4 90.0 43.4 39.8 10.4 4.0 244.0 126.8 66.2
1982 51.2 31.4 164.8 20.2 15.8 125.4 160.8 23.9 208.5 51.2 20.0 22.2
1983 42.6 50.2 310.2 157.8 203.2 78.6 40.6 145.4 46.6 158.0 35.0 103.2
1984 191.2 130.5 276.6 100.6 142.5 89.8 150.8 8.8 52.4 54.0 517.3 97.9
1985 2.2 54.9 87.0 297.4 128.8 123.8 83.6 27.5 76.2 140.0 63.0 91.0
1986 202.1 90.2 18.6 117.7 55.8 16.0 48.7 482.7 31.2 99.2 172.2 6.8
1987 83.2 18.2 149.2 70.8 47.9 82.6 113.9 189.1 8.1 272.5 180.6 82.3
1988 96.3 100.0 79.0 347.7 272.4 86.1 109.3 72.1 145.2 0.6 166.1 132.7
1989 158.8 143.4 143.2 380.4 157.6 249.8 32.7 66.2 2.2 15.3 60.6 141.6
1990 82.9 664.7 139.1 341.5 110.1 42.0 75.7 201.0 202.1 53.6 29.8 42.0
1991 99.6 51.3 17.1 47.4 137.2 347.1 105.3 4.6 18.7 11.9 121.7 185.1
1992 74.5 490.2 86.9 67.5 73.6 97.3 15.8 34.2 25.3 87.7 147.1 205.4
1993 65.5 75.8 92.1 62.8 18.8 60.8 99.8 66.6 169.3 50.5 94.2 32.2
1994 42.0 87.6 171.0 122.0 40.1 70.1 79.9 35.8 28.0 49.2 53.5 69.1
1995 101.8 61.2 176.9 41.7 180.9 114.6 1.5 0.0 223.3 35.5 170.9 81.2
1996 132.5 39.6 59.1 42.6 169.7 176.2 52.2 130.6 142.9 17.3 114.8 37.8
1997 178.0 124.6 27.4 6.6 200.4 110.0 190.6 38.4 137.2 54.8 25.8 30.0
1998 90.8 33.9 19.5 344.1 333.3 148.6 89.8 532.3 43.6 37.9 73.4 59.3"

rainfall <- read.table(text = raw_data_text, header = TRUE)

rainfall_long <- rainfall %>%
  select(-Year) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Month", 
               values_to = "Rainfall") %>%
  mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

ggplot(rainfall_long, aes(sample = Rainfall)) +
  stat_qq_line(distribution = stats::qnorm, color = "red") +
  stat_qq(distribution = stats::qnorm) +
  facet_wrap(~ Month, scales = "free_y", ncol = 4) +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles (Rainfall in mm)", 
       title = "Normal QQ Plots for Monthly Rainfall (1968-1998)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 3. Linear regression analysis
library(ggplot2)

rainfall_data <- data.frame(
  month = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                 levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  month_number = 1:12,
  average_rainfall = c(130.9, 132.6, 149.2, 119.5, 115.2, 128.1,
                       66.7, 94.4, 77.0, 81.3, 108.2, 76.1)
)

rainfall_lm <- lm(average_rainfall ~ month_number, data = rainfall_data)
summary(rainfall_lm)

ggplot(rainfall_data, aes(x = month_number, y = average_rainfall)) +
  geom_point(aes(color = "Monthly average rainfall"), size = 3) +
  geom_smooth(aes(color = "Fitted linear regression"),
              method = "lm", se = FALSE, linewidth = 1) +
  scale_x_continuous(
    breaks = 1:12,
    labels = levels(rainfall_data$month)
  ) +
  scale_color_manual(
    values = c("Monthly average rainfall" = "#1f77b4",
               "Fitted linear regression" = "#1f77b4"),
    name = NULL
  ) +
  labs(
    title = "Linear Regression of Average Monthly Rainfall",
    x = "Month",
    y = "Average rainfall (mm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "topright"
  )