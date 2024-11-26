#loading relevant libraries####
library(tidyverse)
library(janitor)
library(broom)

#Part 1 ####
##reading in the faculty salaries data#####
fs_data <- read.csv("./FacultySalaries_1995.csv")

##exploring the dataset####
head(fs_data)
glimpse(fs_data)
View(fs_data)
names(fs_data)

##cleaning the dataset####
#pivoting the data longer 
fs_data_clean <- fs_data %>%
  pivot_longer(cols = c("AvgFullProfSalary", "AvgAssocProfSalary", 
                        "AvgAssistProfSalary", "AvgProfSalaryAll"),
               names_to = "rank_salary", 
               values_to = "salary") %>%
  pivot_longer(cols = c("AvgFullProfComp", "AvgAssocProfComp", 
                        "AvgAssistProfComp", "AvgProfCompAll"),
               names_to = "rank_comp", 
               values_to = "comp") %>%
  pivot_longer(cols = c("NumFullProfs", "NumAssocProfs", 
                        "NumAssistProfs", "NumInstructors", "NumFacultyAll"),
               names_to = "instructor_type", 
               values_to = "number_faculty")

#cleaning up data in the columns
##rank and salary
fs_data_clean$rank_salary <- gsub("Prof", "", fs_data_clean$rank_salary)
fs_data_clean$rank_salary <- gsub("Avg", "", fs_data_clean$rank_salary)
fs_data_clean$rank_salary <- gsub("Salary", "", fs_data_clean$rank_salary)

##rank and comp
fs_data_clean$rank_comp <- gsub("Prof", "", fs_data_clean$rank_comp)
fs_data_clean$rank_comp <- gsub("Avg", "", fs_data_clean$rank_comp)
fs_data_clean$rank_comp <- gsub("Comp", "", fs_data_clean$rank_comp)

View(fs_data_clean)

##plotting the data####

#filtering out all and VIIB
filtered_fs_data <- fs_data_clean %>%
  filter(rank_salary != "All") %>%
  filter(Tier != "VIIB")

filtered_fs_data %>%
  ggplot(aes(x = rank_salary, y = salary, fill = rank_salary)) +
  geom_boxplot()+
  facet_wrap(~Tier) +
  theme_minimal() +
  labs(
    x = "Rank",           
    y = "Salary",
    fill = "Rank"
  )


#Part 2####
##creating the ANOVA model####
anova_model <- aov(salary ~ State + Tier + rank_salary, data = filtered_fs_data)

# Summary of the ANOVA
summary(anova_model)


#Part 3####
##reading in the juniper oils data####
jo_data <- read.csv("./Juniper_Oils.csv") 

##exploring the data####
head(jo_data)
glimpse(jo_data)
View(jo_data)
names(jo_data)

##cleaning the dataset ####
tidy_jo_data <- jo_data %>%
  pivot_longer(cols = c("alpha.pinene","para.cymene","alpha.terpineol",
                        "cedr.9.ene","alpha.cedrene","beta.cedrene",
                        "cis.thujopsene","alpha.himachalene","beta.chamigrene",
                        "cuparene","compound.1","alpha.chamigrene",
                        "widdrol","cedrol","beta.acorenol",
                        "alpha.acorenol","gamma.eudesmol","beta.eudesmol",
                        "alpha.eudesmol","cedr.8.en.13.ol","cedr.8.en.15.ol",
                        "compound.2","thujopsenal"), 
               names_to = "chem_type", 
               values_to = "ms_conc") %>%
  mutate(chem_type = gsub("\\.", "-", chem_type))


#Part 4####
##Plotting the dataset####
ggplot(tidy_jo_data, aes(x = YearsSinceBurn, y = ms_conc)) +
  geom_smooth() +
  facet_wrap(~ chem_type, scales = "free_y") +
  labs(
    x = "Years Since Burn",
    y = "Concentration"
  ) +
  theme_minimal()

#Part 5####

#filtering and selecting the important stuff
sig_chem <- tidy_jo_data %>%
  group_by(chem_type) %>%                       
  do({
    model <- glm(ms_conc ~ YearsSinceBurn, data = ., family = gaussian())
    tidy(model)                                    
  }) %>%
  ungroup() %>%
  filter(term == "YearsSinceBurn", p.value < 0.05) 

#viewing the table
print(sig_chem)
