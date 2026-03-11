#####Run KNN test: Based on this tutorial https://shihchingfu.github.io/knn-caret-example/ and https://dataaspirant.com/knn-implementation-r-using-caret-package/####

library(dplyr)
library(ggplot2)
library(caret)
library(pROC)
library(tidyr)
library("MLmetrics")
library(MLeval)

### Multiclass analysis ###
df
##Exploratory analysis
#mean of features
means_of_sites_by_condition <- df%>%
  group_by(Condition) %>%
  summarise(N = n(), 
            Mean_Site_38739427 = mean(Site_38739427),
            Mean_Site_50320391 = mean(Site_50320391),
            Mean_Site_90234683 = mean(Site_90234683),
            Mean_Site_74582350 = mean(Site_74582350),
            Mean_Site_18288069 = mean(Site_18288069),
            Mean_Site_55107225 = mean(Site_55107225),
            Mean_Site_29857119 = mean(Site_29857119),
            Mean_Site_5255655 = mean(Site_5255655),
            Mean_Site_17628587 = mean(Site_17628587),
            Mean_Site_4445156 = mean(Site_4445156),
            .groups = "keep")


write.csv(means_of_sites_by_condition, file = "C:\\Users\\df_means_of_sites_by_condition.csv")


#distributions

df%>% 
  mutate(ID = 1:n()) %>%
  pivot_longer(Site_38739427:Site_4445156,
               names_to = "Dimension",
               values_to = "Size") %>%
  mutate(Dimension = factor(Dimension),
         ID = factor(ID)) %>%
  ggplot() +
  aes(y = Size, fill = Condition) +
  facet_wrap(~ Dimension, scales = "free") +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "Size (mm)", title = "Comparison of editing of sites")

#remove the samp col
df$Sample <- NULL

#Split dataset 
set.seed(1)
training_index <- createDataPartition(y = df$Condition, 
                                      p = 0.6,   #change partition as needed
                                      list = FALSE)

training_set <- df[training_index, ]
testing_set <- df[-training_index, ]

table(df$Condition)

#check the dimensions of the training and testing df
dim(training_set);dim(testing_set);

#Parameter tuning 
training_control <- trainControl(method = "repeatedcv",
                                 summaryFunction = defaultSummary,
                                 classProbs = TRUE,
                                 number = 10,
                                 repeats = 10)

set.seed(2)
knn_cv <- train(Condition ~ ., 
                data = training_set,
                method = "knn",
                trControl = training_control,
                metric = "Accuracy",
                tuneGrid = data.frame(k = seq(11,85,by = 2)))
knn_cv

sink("dfVsEachCondition_KnnCV.txt")
print(knn_cv)
sink()

#Test set prediction
test_pred <- predict(knn_cv, newdata = testing_set)
test_pred

#Convert testing_set$Condition to factor 

testing_set$Condition <- as.factor(testing_set$Condition)

#Confusion matrix 
confusion <- confusionMatrix(test_pred, testing_set$Condition)

sink("df_ConfusionMatrix.txt")
print(confusion)
sink()

#Accuracy (repeated cross-validation x neighbors)
plot(knn_cv)

