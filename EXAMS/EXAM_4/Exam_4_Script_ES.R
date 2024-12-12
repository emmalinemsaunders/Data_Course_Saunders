library(tidyverse)
library(dplyr)
library(janitor)


#reading in the data
data <- read.csv("./unicef-u5mr.csv")

#cleaning names 
data <- clean_names(data)

#tidying the dataset
data_longer <- data %>%
  pivot_longer(cols = starts_with("u5mr"), names_to = "Year", values_to = "U5MR") %>%
  na.omit()

#removing U5MR from dataset and converting to numeric
data_longer$Year <- gsub("^[^_]*_", "", data_longer$Year)
data_longer$Year <- as.numeric(as.character(data_longer$Year))




#plotting the data
p <- data_longer %>%
  ggplot(aes(x = Year, y = U5MR, group = country_name)) +
  geom_line()+
  facet_wrap(~continent)+
  theme_bw()

#saving to a certain location
file_path_png <- "./SAUNDERS_plot_1.png"

#saving the plot
ggsave(file_path_png, plot = p, width = 12, height = 8, dpi = 600)




#grouping by mean 
data_mean <- data_longer %>%
  group_by(continent, Year) %>%         
  mutate(Mean_U5MR = mean(U5MR, na.rm = TRUE)) %>%
  ungroup()

#plotting the new data
q <- data_mean %>%
  ggplot(aes(x = Year, y = Mean_U5MR, group = continent, color = continent)) +
  geom_line(linewidth = 1)+
  theme_classic()

#saving to a certain location
file_path_png_2 <- "./SAUNDERS_plot_2.png"

#saving the plot
ggsave(file_path_png_2, plot = q, width = 12, height = 8, dpi = 600)





#creating models

##Model 1
mod1 <- lm(U5MR ~ Year, data = data_longer)

##Model 2
mod2 <- lm(U5MR ~ Year + continent, data = data_longer)

##Model 3
mod3 <- lm(U5MR ~ Year * continent, data = data_longer)



#comparing each model
model_comparison <- data.frame(
  Model = c("Model 1: Year", "Model 2: Year + Continent", "Model 3: Year * Continent"),
  AIC = c(AIC(mod1), AIC(mod2), AIC(mod3)),
  BIC = c(BIC(mod1), BIC(mod2), BIC(mod3)),
  R_squared = c(summary(mod1)$r.squared, summary(mod2)$r.squared, summary(mod3)$r.squared))
print(model_comparison)
#the model that fits the code the best is model 3. It has the lowest AIC and BIC values 
#as well as having the highest R squared value. 



#creating predictions
data_longer_pred <- data_longer %>%
  mutate(
    Pred_mod1 = predict(mod1, newdata = data_longer),
    Pred_mod2 = predict(mod2, newdata = data_longer),
    Pred_mod3 = predict(mod3, newdata = data_longer)
  )

#pivoting dataset longer and cleaning 
predictions <- data_longer_pred %>%
  pivot_longer(cols = starts_with("Pred"), names_to = "model", values_to = "pred_U5MR")

predictions$model <- gsub("^[^_]*_", "", predictions$model)

#plotting the predictions
r <- predictions %>%
  ggplot(aes(x = Year, y = pred_U5MR, group = continent, color = continent)) +
  geom_line() +
  facet_wrap(~model)

#saving to a certain location
file_path_png_3 <- "./SAUNDERS_plot_3.png"

#saving the plot
ggsave(file_path_png_3, plot = r, width = 12, height = 8, dpi = 600)

###BONUS

#filtering the dataset for Ecuador and year 2020
new_data <- data.frame(
  Year = 2020,               # The year for prediction
  continent = "Americas",
  country_name = "Ecuador" # Specify the continent for Ecuador
)

#finding the predicted value and the disparity
ecuador_pred <- predict(mod3, newdata = new_data)
real_U5MR <- 13
difference <- ecuador_pred - real_U5MR

#transform U5MR to a log scale
data_transform <- data_longer %>%
  mutate(log_U5MR = log(U5MR + 1))  # Adding 1 to avoid log(0)

#Fitting a new model
mod_improved <- lm(log_U5MR ~ Year * continent, data = data_transform)

#finding the predicted value and the disparity 
predicted_log_U5MR <- predict(mod_improved, newdata = new_data)
predicted_U5MR_improved <- exp(predicted_log_U5MR) - 1 #making it back to original scale
difference_improved <- predicted_U5MR_improved - real_U5MR
print(difference_improved)

