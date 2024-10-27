library(tidyverse)
library(janitor)
library(gganimate)

#reading in assignments
  data <- read.csv("./BioLog_Plate_Data.csv")

#pivoting data longer and cleaning up time and adding a soil and water column
# and filtering for dilution = 0.1
data_1 <- data %>%
  pivot_longer(cols = c("Hr_24", "Hr_48", "Hr_144"), 
               names_to = "hours", 
               values_to = "absorbance") %>%
  mutate(hours = case_when(hours == "Hr_24" ~ 24,
                           hours == "Hr_48" ~ 48,
                           hours == "Hr_144" ~ 144))

#cleaning names
data_1 <- clean_names(data_1)

#adding sample type column
data_sample_type <- data_1 %>%
  mutate(Type = case_when(sample_id %in% c("Clear_Creek", "Waste_Water") ~ "water",
                          TRUE ~ "soil")) %>%
  filter(dilution == 0.1)


#plotting the data
data_sample_type %>%
  ggplot(aes(x = hours, y = absorbance, color = Type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~substrate) +
  ylim(0,2) +
  labs(x = "Time", 
       y = "Absorbance") +
  theme_minimal()


##animating the plot
mean_absorbance <- data_1 %>%
  mutate(Type = case_when(sample_id %in% c("Clear_Creek", "Waste_Water") ~ "water",
                          TRUE ~ "soil")) %>%
  group_by(hours, Type, substrate, sample_id, dilution) %>%
  summarize(mean_absorbance = mean(absorbance, na.rm = TRUE), .groups = 'drop')

# Filter for "Itaconic Acid" substrate (or adjust for all substrates)
itaconic_data <- mean_absorbance %>%
  filter(substrate == "Itaconic Acid") 

# Create animated plot
animated_plot <- itaconic_data %>%
  ggplot(aes(x = hours, y = mean_absorbance, color = sample_id, 
             group = interaction(sample_id, dilution))) +  
  geom_line() +
  ylim(0, 2) +
  labs(x = "Time (hours)", 
       y = "Mean Absorbance", 
       title = "Absorbance") +
  facet_wrap(~dilution) +  # Add facet for dilution levels
  theme_minimal() +
  transition_reveal(hours)


animated_plot

