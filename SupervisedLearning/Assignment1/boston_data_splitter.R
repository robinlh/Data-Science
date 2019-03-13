rm(list=ls())

#setwd("directory where data file is saved")
fulldata <- read.csv("boston.csv") #Read in all 506 observations

#fulldata <- fulldata[,-1] #Drop ID variable

set.seed(11) #Insert your own project number here
#Example: If your project number is 10, run the line set.seed(10)

my_data <- fulldata[sample(1:nrow(fulldata), 400, replace=FALSE), ]

write.csv(my_data, 'my_boston.csv')