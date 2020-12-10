install.packages("tidyverse")
install.packages("readxl")
install.packages('fastDummies')
library('fastDummies')
library(tidyverse)
library(readxl)
StudentsPerformance_1_ <- read_excel("~/Downloads/StudentsPerformance (1).xlsx")
data <- StudentsPerformance_1_
##above or below mean##
reading.score.mean <- mean(data$reading.score)
math.score.mean <- mean(data$math.score)
writing.score.mean <- mean(data$math.score)
data  <- data %>% mutate(math.above.below = ifelse(math.score < math.score.mean, "Below Mean", "Above Mean"))
data  <- data %>% mutate(reading.above.below = ifelse(reading.score < reading.score.mean, "Below Mean", "Above Mean"))
data  <- data %>% mutate(writing.above.below = ifelse(writing.score < writing.score.mean, "Below Mean", "Above Mean"))
table(data$math.above.below)
table(data$writing.above.below)
table(data$reading.above.below)
##rename data##
names(data)[names(data) == "math score"] <- "math.score"
names(data)[names(data) == "writing score"] <- "writing.score"
names(data)[names(data) == "reading score"] <- "reading.score"
names(data)[names(data) == "parental level of education"] <- "parent.edu"
names(data)[names(data) == "testing preparation course"] <- "test.prep"
glimpse(data)
summary(data)
##subset data##
data %>% filter(math.score > 90 & reading.score < 75)
data %>% filter(reading.score > 90 & math.score < 75)
data %>% filter(parent.edu == "some high school")
data %>% filter(parent.edu == "some college")
data %>% filter(parent.edu == "some college" & `test preparation course` == "completed")
##Data counts##
data %>% group_by(gender) %>% count(`test preparation course`)
data %>% group_by(gender) %>% count(parent.edu)
data %>% group_by(gender) %>% count(lunch)
data %>% group_by(gender) %>% count(`race/ethnicity`)
data %>% group_by(gender) %>% count(gender)
data %>% group_by(gender) %>% count(math.score)
data %>% group_by(gender) %>% count(reading.score)
data %>% group_by(gender) %>% count(writing.score)
data %>% group_by(gender) %>% count(`race/ethnicity`)
data %>% group_by(`race/ethnicity`) %>% count(`test preparation course`)
data3 <- data %>% count(`race/ethnicity`)
##Data correlation##
data %>% summarize(N = n(), r = cor(math.score, reading.score))
data %>% summarize(N = n(), r = cor(math.score, writing.score))
data %>% summarize(N = n(), r = cor(reading.score, writing.score))
##Graphs- scatter##
options(repr.plot.width = 5, repr.plot.height = 4)
ggplot(data, aes(x = writing.score, y = reading.score)) + geom_point() + geom_jitter() + labs(x = "Writing Score", y = "Reading Score")
ggplot(data, aes(x = writing.score, y = reading.score)) + geom_point() + facet_wrap(~ gender)
ggplot(data, aes(x = writing.score, y = math.score)) + geom_point() + labs(x = "Writing Score", y = "Math Score") + facet_wrap(~ gender)
ggplot(data, aes(x = reading.score, y = math.score)) + geom_point() + facet_wrap(~ gender)
##Graphs _ bar##
ggplot(data, aes(x = parent.edu)) + geom_bar() + coord_flip() 
ggplot(data, aes(x = parent.edu)) + geom_bar() +  facet_wrap(~ gender) + coord_flip() 
ggplot(data, aes(x = gender)) + geom_bar()
ggplot(data3, aes(x = "race/ethnicity")) + geom_bar()
##creating dummies##
data1 <- dummy_cols(data, select_columns = c("test preparation course", "gender"))
data1 <- dummy_cols(data, select_columns = 'gender')
glimpse(data1)
##models##
model <- lm(math.score ~ gender_female + writing.score + reading.score + `test preparation course_completed`, data = data1)
model1.2 <- lm(math.score ~ gender_female + writing.score + reading.score + `test preparation course_none`, data = data1)
model2 <- lm(math.score ~ gender_male + writing.score + reading.score + `test preparation course_completed`, data = data1)
model2.2 <- lm(math.score ~ gender_male + writing.score + reading.score + `test preparation course_none`, data = data1)
model3 <- lm(math.score ~ gender_male + writing.score + reading.score + `test preparation course_none` + parent.edu + `race/ethnicity` , data = data1)
model4 <-  lm(math.score ~ gender_female + writing.score + reading.score + `test preparation course_completed` + parent.edu + `race/ethnicity` , data = data1)
model5 <- lm(writing.score ~ gender_female + math.score + parent.edu + `test preparation course_none`, data = data1)

summary(model)
summary(model2)
summary(model3)
summary(model4)
summmary(model5)




