#setting up libraries
library(ggplot2)
library(tidyverse)
library(modelr)
library(easystats)
library(GGally)

#reading in the data
grad_data <- read.csv("./GradSchool_Admissions.csv")

#looking at the data 
glimpse(grad_data)

#frequency distrobution of categroical variables 
table(grad_data$admit)
table(grad_data$rank)

#visual distributions
hist(grad_data$gre, main = "GRE Score Distribution", xlab = "GRE", col = "lightblue")
hist(grad_data$gpa, main = "GPA Distribution", xlab = "GPA", col = "lightgreen")

#admission rate by variable
prop.table(table(grad_data$admit, grad_data$rank), margin = 2)

#comparing admit rates for different gpa and gre ranges
grad_data$GRE_band <- cut(grad_data$gre, breaks = c(200, 300, 400, 500, 600, 700, 800))
prop.table(table(grad_data$admit, grad_data$GRE_band), margin = 2)

#visualizations of relationships
boxplot(gre ~ admit, data = grad_data, main = "GRE by Admission Status", col = c("red", "green"))
boxplot(gpa ~ admit, data = grad_data, main = "GPA by Admission Status", col = c("red", "green"))

#scatterplot correlations
ggplot(grad_data, aes(x = gre, y = gpa, color = factor(admit))) +
  geom_point() +
  labs(title = "GRE vs GPA by Admission Status", color = "Admit")

#models
mod1 <- glm(data=grad_data,
            formula=admit ~ gre + gpa)

mod2 <- glm(data=grad_data,
            formula=admit ~ gre * gpa)

mod3 <- glm(data=grad_data,
            formula=admit ~ gre * gpa * rank)

mod4 <- glm(data=grad_data,
            formula=admit ~ gre * gpa + rank)

step <- MASS::stepAIC(mod3,trace=0) # trace=0 suppresses lengthy output to console
mod5 <- glm(data=grad_data,
            formula=step$formula) # use that AIC-selected model as mod5 (it's too big to print here)

#comparing
comps <- compare_performance(mod1,mod2,mod3,mod4,mod5,
                             rank=TRUE)
comps
 
#plotting
comps %>%
  plot()

#retraining model 5
set.seed(123)
training_samples <- caret::createDataPartition(seq_along(grad_data$admit),
                                               p=.8)
train_data <- grad_data[training_samples$Resample1,]
test_data <- grad_data[-training_samples$Resample1,] # all rows not in training_samples

mod5_formula <- mod5$formula

mod5 <- glm(data=train_data,
            formula = mod5_formula)

mod5 %>% model_parameters()
