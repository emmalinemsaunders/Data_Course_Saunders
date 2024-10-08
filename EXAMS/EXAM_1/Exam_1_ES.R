library(tidyverse)


#Step 1####
#reading in the covid data
data <- read.csv("data/cleaned_covid_data.csv")

#Step 2####
#filtering A states
A_states <- data %>%
  filter(str_starts(Province_State, "A"))

#Step 3####
#creating a plot showing deaths over time of subset of data
A_states %>%
  ggplot(aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = "free") +
  theme_bw()


#Step 4####

#creating a new subset with descending maximum fatality ratio numbers
state_max_fatality_rate <- data %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

#Step 5####
#plotting the data by max fatality rate vs state
state_max_fatality_rate %>%
  ggplot(aes(x = factor(Province_State, levels = rev(Province_State[order(Maximum_Fatality_Ratio)])), 
             y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("State") +
  ylab("Max Fatality Ratio")

#Step 6####
#grouping data by time and finding the cumulative data by time
step_6 <- data %>%
  group_by(Last_Update) %>%
  summarize(total_deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(c_deaths = cumsum(total_deaths))

#plotting step 6
step_6 %>%
  ggplot(aes(x = Last_Update, y = c_deaths)) +
  geom_point() +
  geom_smooth() +
  xlab("Time -->") +
  ylab("Cumulative Deaths") +
  theme_bw() +
  theme(axis.text.x = element_blank())

