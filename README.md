# ONLINE-SHOPPING-INTENTION-PREDICTION-USING-MACHINE-LEARNING
#ONLINE SHOPPING INTENTION PREDICTION USING MACHINE LEARNING
#THE DATASET USED IN THIS CASE WAS OBTAINED FROM THE UCI MACHINE LEARNING REPOSITORY
#R CODES


require(ggplot2)
require(gridExtra)
require(GGally)
library(caret)
library(data.table)
library(ggpubr)
library(ROSE)
library(class)
library(tree)
library(dtree)
library(randomForest)
library(mltools)
library(rsample)
library(e1071)
library(pheatmap)
library(keras)
library(dummies)
library(mlbench)
library(reticulate)
library(dplyr)
library(infotheo)
library(praznik)
set.seed(2019)
require(ggdensity)
# EDA Plot1 variables 1 to 6
onlineshop=read.csv("C:/Users/USER/Downloads/online_shoppers_intention.csv")
onlineshop
library(ggplot2)
library(gridExtra)

data <- onlineshop

plots <- list(
  ggplot(data, aes(x=1, y=Administrative)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Number of Administrative pages visited") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()),
  
  ggplot(data, aes(x=1, y=Administrative_Duration)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Total time spent in Administrative pages") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()),
  
  ggplot(data, aes(x=1, y=Informational)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Number of Informational pages visited") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()),
  
  ggplot(data, aes(x=1, y=Informational_Duration)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Total time spent in Informational pages") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()),
  
  ggplot(data, aes(x=1, y=ProductRelated)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Number of ProductRelated pages visited") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()),
  
  ggplot(data, aes(x=1, y=ProductRelated_Duration)) + 
    geom_violin(trim=FALSE, fill=gray(0.7), color=gray(0.2)) +
    coord_flip() +
    labs(x = " ", y = "Total time spent in ProductRelated pages") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
)

grid.arrange(grobs = plots, nrow = 3, ncol = 2)

##EDA plot 2 Bounce rates
plot1 <- ggplot(data, aes(x=Revenue, y=Administrative)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "Administrative", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

plot2 <- ggplot(data, aes(x=Revenue, y=Informational)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "Informational", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

plot3 <- ggplot(data, aes(x=Revenue, y=ProductRelated)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "ProductRelated", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

plot4 <- ggplot(data, aes(x=Revenue, y=Administrative_Duration)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "Administrative_Duration", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

plot5 <- ggplot(data, aes(x=Revenue, y=Informational_Duration)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "Informational_Duration", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

plot6 <- ggplot(data, aes(x=Revenue, y=ProductRelated_Duration)) + 
  geom_violin(fill='gray', color='black') + 
  labs(x = "ProductRelated_Duration", y = "") + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol = 3)


#EDA plot 3

plot1 <- ggplot(data, aes(x = BounceRates)) +
  geom_density(fill = "gray", color = "black") +
  geom_vline(aes(xintercept = median(BounceRates)), color = "red", linetype = "dashed") +
  labs(x = "Bounce Rates", y = "") +
  theme_classic()

plot2 <- ggplot(data, aes(x = ExitRates)) +
  geom_density(fill = "gray", color = "black") +
  geom_vline(aes(xintercept = median(ExitRates)), color = "red", linetype = "dashed") +
  labs(x = "Exit Rates", y = "") +
  theme_classic()

plot3 <- ggplot(data, aes(x = PageValues)) +
  geom_density(fill = "gray", color = "black") +
  geom_vline(aes(xintercept = median(PageValues)), color = "red", linetype = "dashed") +
  labs(x = "Page Values", y = "") +
  theme_classic()

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, nrow = 3)


plot1 <- ggplot(data, aes(x=BounceRates, fill=Revenue)) + 
  geom_density(alpha=0.4, color = "black") + 
  labs(y = " ") + 
  scale_fill_grey() +
  theme_classic()

plot2 <- ggplot(data, aes(x=ExitRates, fill=Revenue)) + 
  geom_density(alpha=0.4, color = "black") + 
  labs(y = " ") + 
  scale_fill_grey() +
  theme_classic()

plot3 <- ggplot(data, aes(x=PageValues, fill=Revenue)) + 
  geom_density(alpha=0.4, color = "black") + 
  labs(y = " ") + 
  scale_fill_grey() +
  theme_classic()

grid.arrange(plot1, plot2, plot3, nrow = 3)

#### months


# create a vector of month names in order
ordered_months <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# convert the Month column to a factor with ordered levels
data$Month <- factor(data$Month, levels = ordered_months, ordered = TRUE)

# create the plot with black and white theme
plot <- ggplot(data.frame(data), aes(Month, fill=Revenue)) + 
  geom_bar(color='black') + 
  labs(x = "Month") + 
  labs(y = " ") +
  scale_fill_grey()+
  theme_bw()

plot

###weekend
data$Weekend

plot <- ggplot(data.frame(data), aes(Weekend, fill=Revenue)) + 
  geom_bar(color='black') + 
  labs(x = "Month") + 
  labs(y = " ") +
  scale_fill_grey()+
  theme_bw() +
  geom_text(stat='count', aes(label=scales::percent(..count../sum(..count..))), position=position_stack(vjust=0.5))

plot
#### Visitor type
data$VisitorType
plot <- ggplot(data.frame(data), aes(VisitorType, fill=Revenue)) + 
  geom_bar(color='black') + 
  labs(y = " ") +
  scale_fill_grey()+
  theme_bw() +
  geom_text(stat='count', aes(label=scales::percent(..count../sum(..count..))), position=position_stack(vjust=0.5))

plot


#SVM CLASSIFICATION
# Load the required packages
library(e1071)
data$VisitorType <- as.factor(data$VisitorType)
# Split data into training and testing sets
set.seed(123)
train_index <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
train_data
test_data <- data[-train_index, ]
test_data
# Train the SVM model
svm_model <- svm(VisitorType ~ ., data = train_data, kernel = "linear")

# Make predictions on the test set
svm_pred <- predict(svm_model, newdata = test_data)
svm_pred

# Evaluate model performance
table(svm_pred, test_data$VisitorType)
accuracy=sum(diag(table(svm_pred, test_data$VisitorType)))/sum(table(svm_pred, test_data$VisitorType))
accuracy


##Random forest

library(randomForest)


data$VisitorType <- as.factor(data$VisitorType)
# Split data into training and testing sets
set.seed(123)
train_index <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
train_data
test_data
# Train random forest model
rf_model <- randomForest(VisitorType ~ ., data = train_data, ntree = 500)

# Predict on test set
rf_pred <- predict(rf_model, newdata = test_data)

# Evaluate model performance
table(rf_pred, test_data$VisitorType)
accuracy=sum(diag(table(rf_pred, test_data$VisitorType)))/sum(table(rf_pred, test_data$VisitorType))
accuracy



#### LOGIST REGRESION
# load the required packages
library(nnet)
library(caret)
library(ggplot2)
library(reshape2)

 # Data preparation
 data$VisitorType <- as.factor(data$VisitorType) # Convert the binary variable 'visitor type' to a factor
 data$Weekend <- as.factor(data$Weekend)
 data$Revenue <- as.factor(data$Revenue)
 str(data)
 
# Split data into training and testing sets
set.seed(123) # Set seed for reproducibility
train_idx <- sample(nrow(data), 0.7*nrow(data)) # 70% for training
train_data <- data[train_idx,]
test_data <- data[-train_idx,]
train_data
test_data

# fit the model on the training data
model <- multinom(VisitorType ~ ., data = train_data)

# predict the classes for the test data
predicted_classes <- predict(model, newdata = test_data[, 1:18])

# evaluate the model's accuracy
accuracy <- mean(predicted_classes == test_data$VisitorType)
cat("Accuracy:", accuracy, "\n")

# create a confusion matrix
confusion_matrix <- table(test_data$VisitorType, predicted_classes)
confusion_matrix

# Model summary
summary(model)

# Predict on the testing set
pred <- predict(model, newdata = test, type = "response")

# Convert predicted probabilities to class labels
pred_class <- ifelse(pred > 0.5, 1, 0)

# Model evaluation
confusion_matrix <- table(test$am, pred_class)
confusion_matrix
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

