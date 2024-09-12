csv_files <- list.files(path = "Data/", pattern = "\\.csv$") #Reading in the data and limiting it to csv files

length (csv_files) #tells the amount of csv files that we have in the data files

df <- read.csv("Data/wingspan_vs_mass.csv") #reading in the wingspan file as df

head(df, 5) #inspecting the first couple rows of the winspan vs mass files

b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)# finding the files that start with a lowercase b

#for loop for printing the top line in every dataset
for (i in b_files) {
  top_line <- readLines(i, n = 1)
  print(top_line)
}


csv_files_1 <- list.files(path = "Data/", full.names = TRUE, pattern = "\\.csv$", recursive = TRUE) #creating a new object to find the top lines of csv files

#for loop for printing the top line in every csv file
for (i in csv_files_1) {
  top_line <- readLines(i, n = 1)
  print(top_line)
}
