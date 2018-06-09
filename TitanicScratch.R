library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)

train <- read.csv("train.csv")
test <-  read.csv("test.csv")
train <- as_tibble(train)
test <- as_tibble(test)

summary(train)


#Age appears to have NA's

#Let's add a feature for title

train <- mutate(train, Title = 
                  gsub("^.*, (.*?)\\..*$", "\\1",as.character(Name)))
test <- mutate(test, Title = 
                 gsub("^.*, (.*?)\\..*$", "\\1",as.character(Name)))
train <- select(train, -c(PassengerId,Cabin,Ticket))
test <- select(test, -c(PassengerId,Cabin,Ticket))
train[train$Embarked =="",]$Embarked <- "S" #assign the mode

#We'll fix the ages later
#In the meantime, let's see how certain variables affect survival

ggpairs(data = select(train,-Name))

#Too many levels in Title.

summary(with(train, aov(Survived ~ Title)))

#Title is significant, clearly

ggplot(data = train, aes(x = Title, y = Survived)) + geom_bar(
    stat = "summary", fun.y = mean) + coord_flip() +
    geom_text(aes(label = ..count.., y= ..prop..), 
    stat= "count", hjust = -0.5)


#Some of these could certainly be combined - try Mrs and Miss

Mrs <- filter(train, Title %in% c("Mrs"))
Miss <- filter(train, Title %in% c("Miss"))
counts <- c(dim(filter(Mrs))[1],dim(filter(Miss))[1])
lived <- c(dim(filter(Mrs,Survived == 1))[1],dim(filter(Miss,Survived == 1))[1])

prop.test(lived,counts,correct = FALSE)$p.value

#