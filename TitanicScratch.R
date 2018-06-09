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

g <- group_by(train,Pclass) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = Pclass, y = Survived))
     
g <- group_by(train,Sex) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = Sex, y = Survived))

g <- group_by(train,SibSp) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = SibSp, y = Survived))

g <- group_by(train,Parch) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = Parch, y = Survived))
  
ggplot(data = train) + geom_histogram(aes(x= Fare,fill = as.factor(Survived)))

train <- mutate(train,FareBin = cut(Fare,quantile(Fare,seq(0,1,length = 6)),include.lowest = TRUE))

g <- group_by(train,FareBin) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = FareBin, y = Survived))
  
# It looks like 0-10.5 and 10.5 - 39.7 would be best binned together

levels(train$FareBin)[1:2] = "[O,10.5]"
levels(train$FareBin)[3:4] = "(10.5,39.7]"

g <- group_by(train,FareBin) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = FareBin, y = Survived))
  
# That's better

g <- group_by(train,Embarked) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = Embarked, y = Survived))
  
g <- group_by(train,Title) %>% summarize(Survived = mean(Survived)) %>% ggplot()
  g + geom_col(aes(x = Title, y = Survived)) + coord_flip()
  
table(train$Title)

# It turns out that there's very few titles outside of Mr, Mrs, and Miss
# As such, I'll flow th following into Mrs. - Lady, Mlle, Mme, the Countess
# Ms will flow into Miss, the rest will flow into Mr, with the exception of
# Master 

train[train$Title %in% c("Lady","Mlle","Mme","the Countess"),]$Title <- "Mrs"
train[train$Title =="Ms",]$Title <- "Miss"
train[train$Title %in% c("Capt","Don","Dr","Jonkheer","Major","Rev","Sir","Col"),]$Title <- "Mr"

# Now running the above graph shows much more useful information

# Now we try to fill in missing age values.

NArows <- which(is.na(train$Age))
trainclean <- train[-NArows,]
AgeLookup <- group_by(trainclean,Pclass,Sex,FareBin,Embarked,Title) %>%
  summarize(MeanAge = mean(Age))
train <- train %>% left_join(AgeLookup, by = c("Pclass","Sex","FareBin","Embarked","Title"))
