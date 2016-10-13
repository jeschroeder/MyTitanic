# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipinstall.packages("ggplot2")ulation
library('randomForest') # classification algorithm

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

# QUick check of age and survival to see if there may be reason to look at age groups seperatly
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
geom_histogram(alpha = 0.7, position="identity") + 
facet_grid(.~Sex) + 
theme_few()



# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 

# Show title counts by sex again
Stat<-prop.table(table(full$Survived, full$Title),2)
head(Stat,n=1)

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

#Find the likely side of ship based on cabin


# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


#predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Fsize,
 #                      data=full[!is.na(full$Age),], method="anova")
#full$Age2[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])
#a quick looks shows that no one very young has been lised under mrs and the doctor is in his 40's seems reasonable. Also allmost all masters your quite young, also reasonable. 

#full$Age[is.na(full$Age)]<-full$Age2[is.na(full$Age)]

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model.
full$Age <- mice_output$Age


#recatagorize these ages
full$Child[full$Age <= 2] <- 'Baby'
full$Child[full$Age >2 & full$Age <=12] <- 'Kid'
full$Child[full$Age >12 & full$Age < 18] <- 'Teen'
full$Child[full$Age >= 18 ] <- 'Adult'

#lets see how this break down in terms of survivial for each age group
prop.table(table(full$Survived,full$Sex,full$Child),3)

# Make variables factors into factors
factor_vars <- c('PassengerId','Survived','Pclass','Sex','Embarked',
                 'Title','Child')

full[factor_vars] <- lapply(full[factor_vars], factor)


full$Title <-  factor(full$Title)
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(42)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass+Age+Title+Fsize+Fare+Parch+Sex+Child+SibSp,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.5))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.35, label = Rank),
            hjust=0, vjust=0.5, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
