#install.packages("dplyr")
#install.packages("C50")
#install.packages("rpart")

#oday khallaf - 1190546 -2 

library(dplyr)
library(C50)
library(ggplot2)
library(dplyr)
dataset <- read.csv("C:/Users/user/Desktop/DiabetesData.csv")
View(dataset)
summary(dataset)
# Step 1: Print	the	main	statistics	of	each	of	the	attributes	(i.e.,	mean,	median,	standard	deviation,	min,	and	max	values) in	a	proper	table.

attribute_stats <- summary(dataset)
print(attribute_stats)

# Step 2: Print	out	the	distribution	of	the	target	class (i.e.,	the percentage	of	the	positive	to	the	negative	class).

class_distribution <- table(dataset$Diabetic)
class_percentages <- prop.table(class_distribution) * 100
print(class_percentages)
# to calculate standard	deviation
sd_values <- apply(dataset, 2, sd)

# print standard	deviation
print(sd_values)

# Step 3: 3. Split	the	dataset	into	70%	training	and	30%	test	data.

set.seed(123) # For reproducibility
shuffled_dataset <- dataset[sample(nrow(dataset)), ] # Shuffle the dataset
train_size <- round(0.7 * nrow(dataset))
train_data <- shuffled_dataset[1:train_size, ]
test_data <- shuffled_dataset[(train_size + 1):nrow(dataset), ]

# Print the dimensions of train_data and test_data
print("Dimensions of train_data:")
print(dim(train_data))
print("Dimensions of test_data:")
print(dim(test_data))

# Print the first few rows of train_data and test_data
print("First few rows of train_data:")
print(head(train_data))
print("First few rows of test_data:")
print(head(test_data))


# Step 4: Use	the	Decision	Tree	algorithm	(i.e.,	C4.5	or	C5.0	or	their	alternative	
train_data$Diabetic <- as.factor(train_data$Diabetic)
model_m1 <- C5.0(train_data[, -9], train_data$Diabetic)
summary(model_m1)

# Step 5: Use the test data to test the accuracy of the generated model
test_data$Diabetic <- as.factor(test_data$Diabetic)
predictions_test <- predict(model_m1, test_data[, -9])
accuracy_test <- mean(predictions_test == test_data$Diabetic)
print(paste("Accuracy of Model M1 on the Test Set:", accuracy_test))


# Step 6: Print	the	accuracy	of	the	test	set
predictions_test <- predict(model_m1, test_data[, -9])
accuracy_test <- mean(predictions_test == test_data$Diabetic)
print(paste("Accuracy of Model M1 on the Test Set:", accuracy_test))


# Step 7: Create	another	model	(M2)	on	a	new	data	split	of	50%	training	and	50%	

set.seed(456) # For reproducibility
new_train_size <- round(0.5 * nrow(dataset))
new_train_data <- shuffled_dataset[1:new_train_size, ]
new_test_data <- shuffled_dataset[(new_train_size + 1):nrow(dataset), ]
new_train_data$Diabetic <- factor(new_train_data$Diabetic)
model_m2 <- C5.0(new_train_data[, -9], new_train_data$Diabetic)

model_m2 <- C5.0(new_train_data[, -9], new_train_data$Diabetic)
predictions_m2 <- predict(model_m2, new_test_data[, -9])
accuracy_m2 <- mean(predictions_m2 == new_test_data$Diabetic)

print(paste("Accuracy of Model M2:", accuracy_m2))
print(paste("Difference in accuracies:", accuracy_m2 - accuracy_m1))

#step 8:8. Generate	(plot)	the	generated	decision	tree of	model	M1.
plot(model_m1,type="s")


#step 9:9. Generate	(plot)	the	generated	decision	tree of	model	M2.
plot(model_m2,type="s")

