dates=c(as.Date(c("06-12-24", "13-12-24", "17-12-24", "02-01-25", "06-01-25"), format="%d-%m-%y"),
        seq(as.Date("15-01-25", format="%d-%m-%y"),as.Date("03-02-25", format="%d-%m-%y"), by=1))

weights=c(84.45, 83.05, 85.15, 85.95, 84.5, 86.8, 86.35, 86.6, 86.8, 87.3, NA, 87.5, 87.3, 87.0, 86.9, 
          NA, NA, NA, 87.15, 87.7, 87.6, 87.8, 87.45, 87.75, 88.05)

df <- data.frame(dates=dates, weights=weights, measure="past")

ggplot(df[-c(1:5),], aes(x=dates, y=weights)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") 

model <- lm(weights ~ dates, data=df[1:nrow(df),])
new_dates <- data.frame(dates=as.Date(c("2025-03-01","2025-04-01")))
prediction <- predict(model, newdata=new_dates)
prediction

ggplot(df[1:nrow(df),], aes(x=dates, y=weights, color=measure)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size=0.1, color="black", alpha=0.2, linetype="dashed") +
  theme_bw() +
  theme(legend.position="none")

ggplot(rbind(df[1:nrow(df),], data.frame(dates=new_dates, weights=prediction, measure="future")), aes(x=dates, y=weights, color=measure)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size=0.1, color="black", alpha=0.2, linetype="dashed") +
  theme_bw() +
  theme(legend.position="none")

lm(weights ~ dates, data=df[6:24,])
