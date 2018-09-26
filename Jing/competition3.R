setwd("../../data")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$topic


aaa <- train %>% group_by(gender, topic) %>% summarise(count=n())
length(unique(train$user.id))


