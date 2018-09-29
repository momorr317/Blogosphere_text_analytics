setwd("../../data")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$topic


aaa <- train %>% group_by(gender, topic) %>% summarise(count=n())
length(unique(train$user.id))


length(unique(test$user.id))
set.seed(108)
sample(20:30,6440,replace=T)

df = data.frame(user.id =unique(test$user.id), age = runif(6440,20,25))
write.csv(df, 'submission1.csv', row.names = F)

key <- read.csv('age_key.csv')

sum(abs(runif(6440/2,20,25) - key$age[1:3220]))/3220

