setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")
data_orig = read.csv("train.csv", header=TRUE)
data <- data_orig
data <- subset(data, select= -PassengerId)
data <- subset(data, select= -Name)
data <- subset(data, select= -Cabin)
data <- subset(data, select= -Ticket)

data$relatives = data$SibSp + data$Parch
data <- subset(data, select= -SibSp)
data <- subset(data, select= -Parch)

median_age = median(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- median_age

print(data[1,]);

males = data[data$Sex == "male",]
m_survived = sum(males$Survived)

m_labels <- c("Survived", "Not survived")
pie(c(m_survived, nrow(males) - m_survived), m_labels);

females = data[data$Sex == "female",]
f_survived = sum(females$Survived)

f_labels <- c("Survived", "Not survived")
f_pie_elem <- c(f_survived, nrow(females) - f_survived)
pie(f_pie_elem, f_labels);

survived = data[data$Survived == 1,]
survived_1 = sum(survived$Pclass == 1)
survived_2 = sum(survived$Pclass == 2)
survived_3 = sum(survived$Pclass == 3)

labels <- c("1st", "2nd", "3rd")
pie_elem <- c(survived_1, survived_2, survived_3)
pie(pie_elem, labels);

survived = data[data$Survived == 0,]
survived_1 = sum(survived$Pclass == 1)
survived_2 = sum(survived$Pclass == 2)
survived_3 = sum(survived$Pclass == 3)

labels <- c("1st", "2nd", "3rd")
pie_elem <- c(survived_1, survived_2, survived_3)
pie(pie_elem, labels);

poor = data[data$Pclass == 3,]
survived = sum(poor$Survived)

labels <- c("Survived", "Not survived")
pie_elem <- c(survived, nrow(poor) - survived)
pie(pie_elem, labels);

medium_poor = data[data$Pclass == 2,]
survived = sum(medium_poor$Survived)

labels <- c("Survived", "Not survived")
pie_elem <- c(survived, nrow(medium_poor) - survived)
pie(pie_elem, labels);

rich = data[data$Pclass == 1,]
survived = sum(rich$Survived)

labels <- c("Survived", "Not survived")
pie_elem <- c(survived, nrow(rich) - survived)
pie(pie_elem, labels);

rich = data[data$Pclass == 1,]
rich_m = rich[rich$Sex == "male",]
rich_f = rich[rich$Sex == "female",]
survived = sum(rich$Survived)
survived_m = sum(rich_m$Survived)
survived_f = sum(rich_f$Survived)

labels <- c("Survived F", "Survived M", "Not survived M", "Not survived F")
pie_elem <- c(survived_f, survived_m, nrow(rich_m) - survived_m, nrow(rich_f) - survived_f)
pie(pie_elem, labels);

rich = data[data$Pclass == 2,]
rich_m = rich[rich$Sex == "male",]
rich_f = rich[rich$Sex == "female",]
survived = sum(rich$Survived)
survived_m = sum(rich_m$Survived)
survived_f = sum(rich_f$Survived)

labels <- c("Survived F", "Survived M", "Not survived M", "Not survived F")
pie_elem <- c(survived_f, survived_m, nrow(rich_m) - survived_m, nrow(rich_f) - survived_f)
pie(pie_elem, labels);

rich = data[data$Pclass == 3,]
rich_m = rich[rich$Sex == "male",]
rich_f = rich[rich$Sex == "female",]
survived = sum(rich$Survived)
survived_m = sum(rich_m$Survived)
survived_f = sum(rich_f$Survived)

labels <- c("Survived F", "Survived M", "Not survived M", "Not survived F")
pie_elem <- c(survived_f, survived_m, nrow(rich_m) - survived_m, nrow(rich_f) - survived_f)
pie(pie_elem, labels);

boxplot(data$Fare[data$Survived == 0], data$Fare[data$Survived == 1]);

qqnorm(data$Fare[data$Survived == 0]);

qqnorm(data$Fare[data$Survived == 1]);

boxplot(data$relatives[data$Survived == 0], data$relatives[data$Survived == 1]);

hist(data$relatives[data$Survived == 0]);

hist(data$relatives[data$Survived == 1]);

boxplot(data$Age[data$Survived == 0], data$Age[data$Survived == 1]);

hist(data$Age[data$Survived == 0]);

hist(data$Age[data$Survived == 1]);

qqnorm(data$Age[data$Survived == 0]);

qqnorm(data$Age[data$Survived == 1]);

dead = data$Age[data$Survived == 0]
survived = data$Age[data$Survived == 1]

mean_all = mean(data$Age)
sd_all = sd(data$Age)

mean_d = mean(dead)
sd_d = sd(dead)

mean_s = mean(survived)
sd_s = sd(survived)

x = seq(0,100,length=100)
all = dnorm(x,mean_all,sd_all)
dead_y = dnorm(x,mean_d,sd_d)
survived_y = dnorm(x,mean_s,sd_s)

plot(x,all,type="l")
lines(x,dead_y, col="red")
lines(x,survived_y, col="blue");

dead = data$Age[data$Survived == 0]
survived = data$Age[data$Survived == 1]

mean_all = mean(data$Age)
sd_all = sd(data$Age)

mean_d = mean(dead)
sd_d = sd(dead)

mean_s = mean(survived)
sd_s = sd(survived)

x = seq(0,100,length=100)
all = dnorm(x,mean_all,sd_all)
dead_y = dnorm(x,mean_d,sd_d) -all
survived_y = dnorm(x,mean_s,sd_s) -all

plot(x,dead_y,type="n")
lines(x,dead_y, col="red")
lines(x,survived_y, col="blue");

