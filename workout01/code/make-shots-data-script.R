#title: short title
#description: a short description of what the script is about
#input(s):
#output(s): 

library(dplyr)

iguodala <- read.csv("../data/andre-iguodala.csv",stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv",stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv",stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv",stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv",stringsAsFactors = FALSE)

iguodala$name <- c('Andre Iguolada')
green$name <- c('Graymond Green')
durant$name <- c('Kevin Durant')
thompson$name <- c('Klay Thompson')
curry$name <- c('Stephen Curry')

iguodala$shot_made_flag <- gsub("n","shot_no",iguodala$shot_made_flag)
iguodala$shot_made_flag <- gsub("y","shot_yes",iguodala$shot_made_flag)
green$shot_made_flag <- gsub("n","shot_no",green$shot_made_flag)
green$shot_made_flag <- gsub("y","shot_yes",green$shot_made_flag)
durant$shot_made_flag <- gsub("n","shot_no",durant$shot_made_flag)
durant$shot_made_flag <- gsub("y","shot_yes",durant$shot_made_flag)
thompson$shot_made_flag <- gsub("n","shot_no",thompson$shot_made_flag)
thompson$shot_made_flag <- gsub("y","shot_yes",thompson$shot_made_flag)
curry$shot_made_flag <- gsub("n","shot_no",curry$shot_made_flag)
curry$shot_made_flag <- gsub("y","shot_yes",curry$shot_made_flag)

iguodala$minute <- 12*iguodala$period-iguodala$minutes_remaining
green$minute <- 12*green$period-green$minutes_remaining
durant$minute <- 12*durant$period-durant$minutes_remaining
thompson$minute <- 12*thompson$period-thompson$minutes_remaining
curry$minute <- 12*curry$period-curry$minutes_remaining

sink(file="../output/andre-iguodala-summary.txt", append=TRUE)
summary(iguodala) 
sink(file="../output/draymond-green-summary.txt", append=TRUE)
summary(green) 
sink(file="../output/kevin-durant-summary.txt", append=TRUE)
summary(durant)
sink(file="../output/klay-thompson-summary.txt", append=TRUE)
summary(thompson)
sink(file="../output/stephen-curry-summary.txt", append=TRUE)
summary(curry)

iguodala <- as.data.frame(iguodala)
green <- as.data.frame(green)
durant <- as.data.frame(durant)
thompson <- as.data.frame(thompson)
curry <- as.data.frame(curry)
shot_data <- bind_rows(iguodala,green,durant,thompson,curry)

write.csv(shot_data, "../data/shots-data.csv")

sink(file="../output/shots-data-summary.txt", append=TRUE)
summary(shot_data)