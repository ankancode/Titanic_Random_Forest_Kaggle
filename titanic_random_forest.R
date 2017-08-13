train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  geom_histogram(binwidth = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

length(unique(data.combined$Name))

dup.names <- data.combined[which(duplicated(data.combined$Name)), "Name"]

data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

males <- data.combined[which(train$Sex =="male"),]
males[1:5,]

extractTitle <- function(name) {
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

Titles <- NULL
for(i in 1:nrow(data.combined)) {
  Titles <- c(Titles, extractTitle(data.combined[i,"Name"]))
}

data.combined$Title <- as.factor(Titles)

ggplot(data.combined[1:891,], aes(x=Title, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

table(data.combined$Sex)

ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,], aes(x=Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 5) +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x=Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age of the miss by Pclass") +
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch== 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x= SibSp, fill = Survived)) +
  stat_count(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x= Parch, fill = Survived)) +
  stat_count(width = 1) +
  facet_wrap(~Pclass+Title) +
  ggtitle("Pclass Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.SibSp + temp.Parch + 1)

ggplot(data.combined[1:891,], aes(x= Family.size, fill = Survived)) +
  stat_count(width = 1) +
  facet_wrap(~Pclass+Title) +
  ggtitle("Pclass Title") +
  xlab("Family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

str(data.combined$Ticket)
data.combined$Ticket[1:20]


ticket.first.char <- ifelse(data.combined$Ticket == "", " ",substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survibility by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 150) +
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+Title) +
  ggtitle("Pclass + Title") +
  xlab("Pclass + Title") +
  ylab("Total Count") +
  ylim(0, 200) +
  labs(fill = "Survived")

summary(data.combined$Fare)
length(unique(data.combined$Fare))

str(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "u"

cabin.first.char <- as.factor(substr(data.combined$Cabin,1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill =Survived)) +
  geom_bar() +
  ggtitle("Survibility of cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill =Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survibility of cabin.first.char Pclass") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill =Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+Title) +
  ggtitle("Survibility of cabin.first.char Pclass and Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "),"Y","N"))

ggplot(data.combined[1:891,], aes(x= cabin.multiple , fill= Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+Title) +
  ggtitle("Pclass Title") +
  xlab("cabin. multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

str(data.combined$Embarked)
length(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x= Embarked , fill= Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+Title) +
  ggtitle("Pclass Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#install.packages("randomForest")

library(randomForest)

rf.train.1 <- data.combined[1:891, c("Pclass","Title")]
rf.label <- as.factor(train$Survived)
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "Family.size")]
rf.label <- as.factor(train$Survived)
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y=rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
test.new <- data.combined[892:1309, c("Pclass", "Title", "Family.size")]
Survived <- predict(rf.2, newdata = test.new)

PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission_3.csv", row.names = FALSE)

library(caret)
#install.packages("doSNOW")
library(doSNOW)

set.seed(2348)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

table(rf.label)
#table(rf.label[cv.10.folds[[33]]])

ctrl.1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)


c1 <- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)

set.seed(34324)

rf.2.cv.1 <- train(x=rf.train.2, y = rf.label, method = "rf", tuneLength = 3,
                   ntree=1000, trControl = ctrl.1)

stopCluster(c1)

rf.2.cv.1




library(rpart)
library(rpart.plot)

rpart.cv <- function(seed, training, labels, ctrl) {
  c1 <- makeCluster(4, type = "SOCK")
  registerDoSNOW(c1)
  set.seed(seed)
  rpart.cv <- train(x=training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl )
  stopCluster(c1)
  return(rpart.cv)
}

features <- c("Pclass","Title", "Family.size") 
rpart.train.1 <- data.combined[1:891, features]

rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.1)
rpart.1.cv.1

prp(rpart.1.cv.1$finalModel, type =  0, extra = 1, under = TRUE)

table(data.combined$Title)

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[",1)
last.names[1:10]

data.combined$last.name <- last.names

name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)

unique(titles)

titles[titles %in% c("Dona.","the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"

table(titles)

data.combined$new.title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x=new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rates for new.title by Pclass")

indexes <- which(data.combined$new.title =="Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")

data.combined$new.title[indexes] <- "Mr."


ggplot(data.combined[1:891,], aes(x=new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rates for new.title by Pclass")

features <- c("Pclass", "new.title", "Family.size")

rpart.train.2 <- data.combined[1:891,features]
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.1)
rpart.2.cv.1

prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

data.combined$Sex <- as.factor(data.combined$Sex)
index.first.mr <- which(data.combined$new.title=="Mr." & data.combined$Pclass =="1")
first.mr.df <- data.combined[index.first.mr,]
summary(first.mr.df)

first.mr.df[first.mr.df$Sex == "female",]

indexes <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female" )
data.combined$new.title[indexes] <- "Mrs."

length(which(data.combined$Sex == "female" & (data.combined$new.title == "Mr." | 
         data.combined$new.title == "Master.")))

index.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[index.first.mr,]

summary(first.mr.df[first.mr.df$Survived=="1",])
View(first.mr.df[first.mr.df$Survived=="1",])

indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

ggplot(first.mr.df, aes(x=Fare, fill = Survived)) +
         geom_density(alpha = 0.5) +
         ggtitle("1st class Mr. survival rate by fare")

ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for(i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"]/length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes) 
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

first.mr.df <- data.combined[index.first.mr,]
summary(first.mr.df)


ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x= ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st class 'Mr.' by ticket.party.size ")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x= avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st class 'Mr.' by avg.fare ")

summary(data.combined$avg.fare)

data.combined[is.na(data.combined$avg.fare),]

indexes <- with(data.combined, which(Pclass=="3" & Title =="Mr." & Family.size == 1 & Ticket!= "3701"))
similar.na.passengers <- data.combined[indexes,]  
summary(similar.na.passengers$avg.fare)

data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

preproc.data.combined <- data.combined[, c("ticket.party.size","avg.fare")]
preProc  <- preProcess(preproc.data.combined, method = c("center", "scale"))
postproc.data.combined <- predict(preProc, preproc.data.combined)
View(postproc.data.combined)

cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], postproc.data.combined$avg.fare[indexes])

features <- c("Pclass", "new.title", "Family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.1)

prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

rpart.3.cv.1

test.submit.df <- data.combined[892:1309, features]

rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "kaggle_submission_4.csv" , row.names = FALSE)


features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x= rf.train.temp, y=rf.label, ntree =1000)
rf.temp

test.submit.df <- data.combined[892:1309,features]
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "kaggle_submission_5.csv", row.names = FALSE)





























