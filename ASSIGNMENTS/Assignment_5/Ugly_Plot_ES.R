library(tidyverse)
data <- read.csv("./ASSIGNMENTS/Assignment_5/disasters.csv")

ggplot(data, aes(x = Year, y = Deaths)) +
  geom_point(size = 5, shape = 21, fill = "magenta", color = "cyan") +  # Clashing point colors
  geom_smooth(method = "lm", color = "yellow", linetype = "dashed", size = 2) +  # Thick, unnecessary line
  theme(
    panel.background = element_rect(fill = "limegreen"),  # Bright background color
    plot.background = element_rect(fill = "orange"),  # Another bright color
    panel.grid.major = element_line(color = "red", linetype = "dotted", size = 1.5),  # Busy grid
    panel.grid.minor = element_line(color = "blue", linetype = "twodash", size = 0.8),
    axis.text.x = element_text(color = "pink", size = 15, angle = 45),  # Large, slanted axis labels
    axis.text.y = element_text(color = "purple", size = 15),
    axis.title.x = element_text(color = "yellow", size = 20),  # Oversized axis titles
    axis.title.y = element_text(color = "orange", size = 20),
    plot.title = element_text(color = "cyan", size = 25, face = "bold", hjust = 0.5),
    legend.background = element_rect(fill = "yellow"),  # Distracting legend background
    legend.title = element_text(color = "red", size = 15),  # Clashing legend text
    legend.key = element_rect(fill = "purple")  # Unnecessary legend key fill
  ) +
  ggtitle("Natural Disaster Deaths") +  # Over-the-top title
  labs(x = "years", y = "deaths") 
