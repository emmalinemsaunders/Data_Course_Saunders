#LESSON####


library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)


#loading data
data("mtcars")
glimpse(mtcars)

#a simple linear model with displacement and horsepower as explanatory variables
mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)


#plotting data visually
ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#model with speed
mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#actual vs predicted
df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")



# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here
newdf = data.frame(disp = c(500,600,700,800,900)) # anything specified in the model needs to be here with exact matching column names

# making predictions
pred = predict(mod1, newdata = newdf)

# combining hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)

# Add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(df,hyp_preds)

# plot those predictions on our original graph
ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()

# Define a 3rd model
mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)

# put all models into a list
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
# apply "performance" function on all in the list and combine 
map(mods,performance) %>% reduce(full_join)

mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()

# gather predictions from all 3 models
mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)

report(mod3)



#ASSIGNMENT####

library(tidyverse)

#reading in the data
data <- read.csv("./mushroom_growth.csv")

#response is the growth rate the predictors are the light, nitrogen, humidity and temperature

#plotting the data growthrate vs light
data %>%
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_smooth()

#plotting the data growth rate vs nitrogen
data %>%
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_smooth()

#plotting the data growth rate vs humidity
data %>%
  ggplot(aes(x = Humidity, y = GrowthRate)) +
  geom_smooth()

#plotting the data growth rate vs temperature
data %>%
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_smooth()

#creating and plotting model 1
mod1 <-  lm(GrowthRate ~ Light, data = data)
summary(mod1)

ggplot(data, aes(x=Light,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#creating and plotting model 2
mod2 = lm(GrowthRate ~ Nitrogen, data = data)
ggplot(data, aes(x=Nitrogen,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#creating and plotting model 3
mod3 <- glm(data=data,
            formula = GrowthRate ~ Light + Nitrogen + Temperature)

mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods,performance) %>% reduce(full_join)

data %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()

#creating and plotting model 4
mod4 <- lm(GrowthRate ~ Light * Nitrogen, data = data)
summary(mod4)

#Plotting model 4: Nitrogen vs. GrowthRate with Light as an interaction
ggplot(data, aes(x = Nitrogen, y = GrowthRate, color = Light)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  theme_minimal() 

#Calculate MSE for model 1
pred_mod1 <- predict(mod1, data)
mse_mod1 <- mean((data$GrowthRate - pred_mod1)^2)

#Calculate MSE for model 2
pred_mod2 <- predict(mod2, data)
mse_mod2 <- mean((data$GrowthRate - pred_mod2)^2)

#Calculate MSE for model 3
pred_mod3 <- predict(mod3, data)
mse_mod3 <- mean((data$GrowthRate - pred_mod3)^2)

#Calculate MSE for model 4
pred_mod4 <- predict(mod4, data)
mse_mod4 <- mean((data$GrowthRate - pred_mod4)^2)


#Step 1: Calculate the MSE for all models (from previous step)
mse_list <- c(mse_mod1, mse_mod2, mse_mod3, mse_mod4)
names(mse_list) <- c("Model 1", "Model 2", "Model 3", "Model 4")

# Select the best model based on the lowest MSE
best_model_index <- which.min(mse_list)
best_model <- list(mod1, mod2, mod3, mod4)[[best_model_index]]
cat("Best Model:", names(mse_list)[best_model_index], "\n")

#Step 2: Create new hypothetical values for independent variables
# Assuming variables Light, Nitrogen, and Temperature
new_data <- data.frame(
  Light = seq(min(data$Light), max(data$Light), length.out = 10),
  Nitrogen = seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 10),
  Temperature = mean(data$Temperature) # Use mean for fixed effect if needed
)

# Predict GrowthRate for hypothetical values
new_data$PredictedGrowthRate <- predict(best_model, newdata = new_data)


#Step 3: Plot predictions alongside the real data

ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point(aes(color = "Real Data"), alpha = 0.7) +
  geom_line(data = new_data, aes(x = Light, y = PredictedGrowthRate, color = "Predicted Data"), size = 1) +
  theme_minimal() 




#question 3####

non_linear_data <- read.csv("./non_linear_relationship.csv")

#Fit a linear model with a quadratic term for the predictor
mod_nonlinear <- lm(response ~ predictor + I(predictor^2), data = non_linear_data)

#Summarize the model
summary(mod_nonlinear)

#Plot the data and the fitted curve
ggplot(non_linear_data, aes(x = predictor, y = response)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "blue", se = FALSE) +
  theme_minimal() 
