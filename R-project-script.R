## Importing libraries

library(tidyverse) #for data transformation and wrangling
library(ggplot2) # for creating data visualization
library(ggthemes) #customizing data visualizations
library(dplyr) #data manipulation
library(caret)
library(caTools)

## Loading Data


data <- read.csv("C:/Users/Jucheey/Downloads/data-1.csv")


## Data Inspection

n_rows <- dim.data.frame(data)[1] # number of rows 
n_cols <- dim.data.frame(data)[2] # number of columns

cat("Number of rows is: ", n_rows, "\n")
cat("Number of columns: ", n_cols, "\n")


### Sample observation

# Print the number of sampled households
cat("Number of sampled households is:", n_distinct(data$ID), "\n")
cat("Number of sampled persons is:", length(data$ID), "\n")


### Missing values

# Check for missing values in each column and display the count per column
if (any(colSums(is.na(data)) > 0)) {
  print("Columns with missing values:")
  print(colSums(is.na(data))[colSums(is.na(data)) > 0])
} else {
  print("No missing values found in any column.")
}


## Descriptive Statistics


summary(data[c('Age', 'INC')],na.rm = TRUE)


# Exploratory Data Analysis

# Remove all rows with missing values from the entire data set
cleaned_data <- na.omit(data)


# Creating a scatter plot with a legend based on the 'H8' column
ggplot(cleaned_data, aes(x = Age, y = INC, color = factor(H8))) +
  geom_point(na.rm = TRUE) +
  labs(title = "Age vs. Income with Living Conditions",
       x = "Age",
       y = "Income",
       color = "Living Conditions") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("Non-Exclusive LC", 
                                "Exclusive LC"))



# Create intervals for income levels and assign labels
cleaned_data["income_group"] = cut(cleaned_data$INC, c(0, 20000, 40000, 60000,80000, Inf), c("0-20k", "20-40k", "40-60k", "60-80k", "80k+"), include.lowest=TRUE)
# Create intervals for age and assign labels
cleaned_data$Age_Group <- cut(cleaned_data$Age, c(0, 18, 30, 40,50,60, Inf), c("0-18", "19-30", "31-40", "41-50", "51-60", "60+"), include.lowest=TRUE)



# Calculate the mean income for each age group
mean_income_age <- cleaned_data %>%
  group_by(Age_Group) %>%
  summarise(mean_income = mean(INC, na.rm = TRUE))

# Plotting a bar chart of average income by age group
ggplot(mean_income_age, aes(x = Age_Group, y = mean_income, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Income by Age Group",
       x = "Age Group",
       y = "Average Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot with legend based on H8 for Income by Living Conditions
ggplot(cleaned_data, aes(x = factor(Age_Group), y = INC, fill = factor(H8))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions among Age group",
       x = "Age Group",
       y = "Income",
       fill = "Living Conditions") 



ggplot(cleaned_data, aes(x = factor(Female), y = INC, fill = factor(H8))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions among Gender",
       x = "Sex",
       y = "Income",
       fill = "Living Conditions")

ggplot(cleaned_data, aes(x = factor(Mar_Stat), y = INC, fill = factor(H8))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions by Marital Status",
       x = "Marital status",
       y = "Income",
       fill = "Living Conditions") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(cleaned_data, aes(x = factor(Eth), y = INC, fill = factor(H8))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions among Ethnic group",
       x = "Ethnic group",
       y = "Income",
       fill = "Living Conditions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(cleaned_data, aes(x = factor(Highest.Ed), y = INC, fill = factor(H8))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions among Education status",
       x = "Education Status",
       y = "Income",
       fill = "Living Conditions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Factors that influences the household living conditions

model_data <- cleaned_data
model_data$income_group <- as.integer(factor(model_data$income_group))
model_data$Age_Group <- as.integer(factor(model_data$Age_Group))
model_data$Mar_Stat <- as.integer(factor(model_data$Mar_Stat))
model_data$Eth <- as.integer(factor(model_data$Eth))
model_data$Highest.Ed <- as.integer(factor(model_data$Highest.Ed))
model_data <- model_data[, c( "Mar_Stat", "Female", "H8", "Eth","Highest.Ed", "income_group","Age_Group")]


set.seed(1)
#Used 70% of data set as training set and remaining 30% as testing set
sample <- sample.split(model_data, SplitRatio = 0.7)
train  <- subset(model_data, sample == TRUE)
test   <- subset(model_data, sample == FALSE)



## fit a logistic regression model with the training dataset
log.model <- glm(H8 ~Mar_Stat+Female+Highest.Ed+income_group+Age_Group, data = train, family = binomial(link = "logit"))




summary(log.model)

# Clustering Analysis


cluster_data <- cleaned_data
cluster_data <- cluster_data[, c( "Age","INC","Mar_Stat", "Female", "H8", "Eth","Highest.Ed", "income_group","Age_Group")]

cluster_data$income_group <- as.integer(factor(cluster_data$income_group))
cluster_data$Age_Group <- as.integer(factor(cluster_data$Age_Group))
cluster_data$Mar_Stat <- as.integer(factor(cluster_data$Mar_Stat))
cluster_data$Eth <- as.integer(factor(cluster_data$Eth))
cluster_data$Highest.Ed <- as.integer(factor(cluster_data$Highest.Ed))

set.seed(1)
# K-means clustering with k clusters (adjust 'k' as needed)
k <- 4  # Number of clusters
kmeans_model <- kmeans(cluster_data, centers = k, nstart = 25)

# View the cluster assignments
cluster_assignments <- kmeans_model$cluster
data_with_clusters <- cbind(cluster_data, cluster = cluster_assignments)


ggplot(data_with_clusters, aes(x = Age, y = INC, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Respondents by Age group and income",
       x = "Age",
       y = "Income",
       color = "Cluster") +
  theme_minimal()


ggplot(data_with_clusters, aes(x = factor(H8), y = INC, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Income by Living Conditions",
       x = "Living Condition",
       y = "Income",
       fill = "Clusters") 


