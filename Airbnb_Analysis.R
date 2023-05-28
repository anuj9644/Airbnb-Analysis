#Part 1 Data Preparation & Exploration


#Sub-part 1 Loading the data set
#Important libraries
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(naniar)
library(leaflet)
library(anytime)
library(stringr)
library(tibble)
library(tidyr)
library(readxl)
library(readr)
library(stats)
library(textdata)
library(simputation)
library(caret)
library(e1071)
library(arules)
library(wordcloud)
library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Load the data 

buenos <- read_csv("buenos.csv")
my_data <- buenos[buenos$neighbourhood_cleansed == "Belgrano",]

#Sub Part 2 Dealing with missing values
#Missing values

#Looking for total number or missing values in each column
#In R, missing values are represented as NA, regardless of the data type. Therefore, if there are no missing values in logical columns, the code will still report the presence of NAs in those columns.
#A function that takes a dataset as input and prints the column names with missing values, the number of missing values in each column, and the data types of those columns
get_missing_cols_info <- function(data) {
  # select only numeric and character columns
  num_cols <- sapply(data, is.numeric)
  char_cols <- sapply(data, is.character)
  date_cols <- sapply(data, is.Date)
  non_logical_cols <- num_cols | char_cols | date_cols
  #get column names with total count of missing values
  missing_counts <- colSums(is.na(data[, non_logical_cols]))
  missing_cols <- names(missing_counts[missing_counts > 0])
  #Getting the data types of columns containing missing values.
  missing_col_types <- sapply(data[, missing_cols], class)
  
  # print column names, data types and total count of missing values
  for (col in missing_cols) {
    count <- missing_counts[col]
    type <- missing_col_types[col]
    cat(paste0(col, "(",type ,")", ": ", count, "\n"))
  }
}

#Getting missing values for the original data
get_missing_cols_info(my_data)

#----------------------------------------------------------------------------------------------------
#Getting total number of outliers for each column that has missing values to make imputation more robust and free from impact of outliers

# get column names with total count of missing values
missing_counts <- colSums(is.na(my_data))
missing_cols <- names(missing_counts[missing_counts > 0])

for (col in missing_cols) {
  count <- missing_counts[col]
  cat(paste0(col, ": ", count, "\n"))
}

str(my_data)

# get numeric columns without id, scrape_id and host_id columns that contain missing values
num_cols <- my_data[, intersect(names(my_data)[sapply(my_data, is.numeric)], missing_cols)]
num_cols <- num_cols[, !names(num_cols) %in% c("id", "scrape_id", "host_id")]

# calculate Turkey fences for each numeric column
tf <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
  iqr <- diff(q)
  low <- q[1] - 1.5 * iqr
  high <- q[2] + 1.5 * iqr
  return(c(low, high))
}
turkey_fences <- apply(num_cols, 2, tf)

# identify outliers in each numeric column
outlier_counts <- apply(num_cols, 2, function(x) {
  fences <- tf(x)
  sum(x < fences[1] | x > fences[2], na.rm=TRUE)
})

# create data frame of column names and outlier counts
outlier_df <- data.frame(column = names(outlier_counts), count = outlier_counts, stringsAsFactors = FALSE)

outlier_df

#Tukey's fences is a robust method for identifying outliers that doesn't assume a specific underlying distribution of the data. This method uses the interquartile range (IQR) to identify values that are significantly different from the bulk of the data. The IQR is defined as the range between the 25th and 75th percentiles of the data, and Tukey's fences define the "inner fence" as 1.5 times the IQR below the 25th percentile, and the "outer fence" as 1.5 times the IQR above the 75th percentile.

#Values outside the outer fence are considered potential outliers, and values between the inner and outer fences are considered mild outliers. This method is useful for identifying potential outliers in datasets where the distribution of the data is unknown or complex, or where other outlier detection methods may not be appropriate.
#
#----------------------------------------------------------------------------------------------------
#Imputing missing values
# Load the mice package
library(mice)

# Select the numerical columns with missing values
missing_cols <- c("bedrooms", "beds", "first_review", "last_review", 
                  "review_scores_rating", "review_scores_accuracy", 
                  "review_scores_cleanliness", "review_scores_checkin", 
                  "review_scores_communication", "review_scores_location", 
                  "review_scores_value", "reviews_per_month")

# Convert date columns to numeric data type representing days since 01-01-1970
my_data$first_review <- as.numeric(difftime(as.Date(my_data$first_review), as.Date("1970-01-01"), units = "days"))
my_data$last_review <- as.numeric(difftime(as.Date(my_data$last_review), as.Date("1970-01-01"), units = "days"))

# Impute missing values using mice
imputed_data <- mice(my_data[missing_cols], method = "rf", m = 5, maxit = 20)

# Extract the completed data
completed_data <- complete(imputed_data)

# Convert date columns back to their original format
completed_data$first_review <- as.Date(completed_data$first_review, origin = "1970-01-01")
completed_data$last_review <- as.Date(completed_data$last_review, origin = "1970-01-01")

# Merge the imputed data back to the original data frame and replace the columns that were imputed
my_data[missing_cols] <- completed_data


#Imputing date type missing values 
library(forecast)

# Extract date columns
date_cols <- sapply(my_data, is.Date)
date_data <- my_data[, date_cols]

# Create function to impute missing dates using exponential smoothing
impute_missing_dates <- function(x) {
  if (sum(is.na(x)) == 0) {
    return(x)
  } else {
    # Fit exponential smoothing model to non-missing data
    x_non_missing <- na.omit(x)
    fit <- ets(x_non_missing)
    
    # Forecast missing values using model
    x_missing <- x[is.na(x)]
    x_forecast <- forecast(fit, h = length(x_missing))
    
    # Replace missing values with forecasts
    x[is.na(x)] <- as.numeric(x_forecast$mean)
    return(x)
  }
}

# Impute missing dates using exponential smoothing
imputed_date_data <- apply(date_data, 2, impute_missing_dates)

# Replace original date columns with imputed data
my_data[, date_cols] <- imputed_date_data

#--------------------------------------------------------------------------------------------
#Part 2 Data Summary

#1. Average price per night for top 10 most frequent property types:
my_data$price <- as.numeric(gsub("[\\$,]", "", my_data$price))

my_data %>%
  group_by(property_type) %>%
  summarize(n = n(), avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(n))

#2. Average minimum nights required for top 10 most frequent property types
top_property_types <- my_data %>%
  group_by(property_type) %>%
  summarize(count = n()) %>%
  top_n(10, count) %>%
  pull(property_type)
avg_min_nights <- my_data %>%
  filter(property_type %in% top_property_types) %>%
  group_by(property_type) %>%
  summarize(avg_min_nights = mean(minimum_nights)) %>%
  arrange(avg_min_nights) %>%
  mutate(avg_min_nights = round(avg_min_nights))%>% arrange(desc(avg_min_nights))
avg_min_nights

#3. Minimum and maximum review score, average number of amenities by property type
my_data %>%
  group_by(property_type) %>%
  summarize(min_review_score = min(review_scores_rating, na.rm = TRUE),
            max_review_score = max(review_scores_rating, na.rm = TRUE), 
            avg_amenities = round(mean(str_count(amenities, ",")) + 1))

#4. Count the number of listings with at least one amenity that includes the word "wifi", "internet", or "broadband" for each property type with total number of hosts
my_data %>%
  mutate(has_internet = str_detect(amenities,regex("wifi|internet|broadband", ignore_case = TRUE, collapse = "|"))) %>%
  group_by(property_type) %>%
  summarize(num_listings_with_internet = sum(has_internet, na.rm = TRUE),
            num_hosts = n_distinct(host_id)) %>%
  arrange(desc(num_listings_with_internet))

#5. Number of listings that allow pets, by property type and room type
my_data %>% 
  filter(!is.na(amenities)) %>% 
  mutate(pets_allowed = ifelse(str_detect(amenities, "Pets allowed"), "Yes", "No")) %>% 
  group_by(property_type, room_type, pets_allowed) %>% 
  summarize(num_listings = n()) %>% 
  filter(pets_allowed == "Yes") %>% 
  select(property_type, room_type, num_listings)%>%
  arrange(desc(num_listings))


#-----------------------------------------------------------------------------------------
#Part 3 Data Visualization
# Select columns with no missing values
no_missing_cols <- names(my_data)[colSums(is.na(my_data)) == 0]


# Subset the data frame and select only the complete columns, ignoring the text fields 
completeData <- my_data[, no_missing_cols]


# apply function to count unique values for each column
sapply(completeData, function(x) length(unique(x)))

#Room type by number of reviews
ggplot(completeData, aes(x = room_type, y = number_of_reviews)) +
  geom_boxplot(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Room Type", y = "Number of Reviews", title = "Distribution of Number of Reviews by Room Type")

#Stacked bar plot of superhost vs. non-superhost by room type:
ggplot(completeData, aes(x = host_is_superhost, fill = room_type)) +
  geom_bar() +
  labs(x = "Superhost", y = "Count", title = "Superhost vs. Non-Superhost by Room Type") +
  scale_fill_manual(values = c("#404080", "#69b3a2", "#f8c471", "#d2b4de"))


# Subset the data to only include columns of interest
review_data <- completeData[c("review_scores_rating", "room_type")]

# Create the density plot for reviews by room_type
ggplot(review_data, aes(x = review_scores_rating, fill = room_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#619CFF", "#FF9F3B", "#6FCF97", "#EB5757")) +
  labs(x = "Review Scores", y = "Density", fill = "Room Type") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Side-by-side bar plots for number of bedrooms vs reviews 

ggplot(completeData, aes(x = bedrooms, y = number_of_reviews, fill = room_type)) +
  geom_col(position = "dodge") +
  labs(x = "Number of bedrooms", y = "Number of reviews", fill = "Room Type") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Pie chart of the proportion of superhosts by room type:
superhost_by_type <- completeData %>% 
  group_by(room_type, host_is_superhost) %>% 
  summarize(count = n()) %>% 
  mutate(prop = count / sum(count))
ggplot(superhost_by_type, aes(x = "", y = prop, fill = host_is_superhost)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(x = "", y = "", fill = "Superhost") +
  facet_wrap(~room_type) +
  theme_void()

#-----------------------------------------------------------------------------------------------
#Part 4 Mapping


# Create map centered on Belgrano
map <- leaflet() %>%
  addTiles() %>%
  setView(-34.561042, -58.462573, zoom = 14)


# Define colors for different room types, belgrano has only three types of room
colors <- c("green", "red", "blue")

# Add markers for each listing in Belgrano
belgrano_listings <- my_data[my_data$neighbourhood_cleansed == "Belgrano",]

for (i in 1:nrow(belgrano_listings)) {
  color <- colors[match(belgrano_listings$room_type[i], c("Entire home/apt", "Private room", "Shared room"))]
  icon <- makeAwesomeIcon(icon = "home", markerColor = color, iconColor = "white", library = "ion")
  map <- addAwesomeMarkers(map, lng = belgrano_listings$longitude[i], lat = belgrano_listings$latitude[i], icon = icon)
}

# apply function to count unique values for each column
sapply(belgrano_listings, function(x) length(unique(x)))

# Display map
map

#---------------------------------------------------------------------------------------------
#pART 5 Wordcloud
# Load necessary libraries
library(wordcloud2)
library(tm)
library(SnowballC)

# Filter out missing values in neighborhood overview column
neighborhood_overview <- my_data %>%
  select(neighborhood_overview) %>%
  filter(!is.na(neighborhood_overview))

# Generate a word frequency table after removing stop words and stemming
word_freq <- neighborhood_overview %>%
  unnest_tokens(word, neighborhood_overview, token = "words", drop = FALSE) %>%
  filter(!word %in% c(stop_words, stopwords::stopwords("es"), stopwords::stopwords("en"))) %>%
  mutate(word = wordStem(word)) %>%
  count(word)

# Generate the wordcloud
wordcloud2(data = word_freq, size = 1.2, color = "random-dark", backgroundColor = "#f7f7f7")

# Sort word_freq by decreasing frequency
word_freq_sorted <- word_freq %>% arrange(desc(n))

# Display top 20 rows
head(word_freq_sorted, 20)
#------------------------------------------------------------------------------------------------
# Prediction

# creating the data set

view(my_data)
pred <- as.data.frame(my_data[,c(31,32,24,33,35,38,39,41,43,44,45,46,52,53,54,55,62,71,72)])
pred$price <- log(pred$price)
view(pred)
str(pred)
glimpse(pred)

# Split the data set

pred$property_type <- as.factor(pred$property_type)

set.seed(699)

train <- sample(row.names(pred), 0.6*dim(pred)[1])
valid <- setdiff(row.names(pred), train)
trainpred <- pred[train, ]
validpred <- pred[valid, ]


# Multiple Linear Regression

multi <- lm( price ~ . , data=trainpred)
multi
summary(multi)

# Backward Elimination

multi1 <- step(multi, direction="backward")
summary(multi1)

# MLR with significant variables

multir <- lm(formula=price ~ longitude + property_type + accommodates + bedrooms + availability_30 + 
               availability_60 + availability_90, data = trainpred)
multir
summary(multir)

# MLR Model without property_type

multip <- lm(formula=price ~ accommodates + bedrooms + availability_30 + 
               availability_60 + availability_90, data=trainpred)
multip
summary(multip)

par(mfrow=c(2,2))
plot(multip)

# Accuracy

predac<-predict(multip, trainpred)
accuracy(predac ,trainpred$price)

# Validation set 

predac1<-predict(multip, validpred)
accuracy(predac1 ,validpred$price)

# Calculating the SST, SSR and SSE

SST1<-sum((trainpred$price-mean(trainpred$price))^2)
print(SST1)

SSR1<-sum((multip$fitted.values-mean(trainpred$price))^2)
print(SSR1)

SSR1/SST1

SSE<-sum((multip$residuals)^2)
print(SSE) 


#------------------------------------------------------------------------------------------------
# KNN

view(my_data)
summary(my_data[, sapply(my_data, is.numeric)])
dim(my_data)
colnames(my_data)

#creating the data set 

knear <- as.data.frame(my_data[,c(24,35,38,39,40,41,45,54,57,64,72)])
view(knear)

set.seed(699)

# Splitting the data

train <- sample(row.names(knear), 0.6*dim(knear)[1])
valid <- setdiff(row.names(knear), train)
train <- knear[train, ]
valid <- knear[valid, ]


# Selection of amenities on the basis of particular amenities or combination of amenities 



train1 <- train %>% mutate(tv_there= ifelse(grepl("TV|Wifi",amenities), '1', '0'))
valid1 <- valid %>% mutate(tv_there= ifelse(grepl("TV|Wifi",amenities), '1', '0'))

# placing as factor

class(train1$tv_there)
train1$tv_there <- as.factor(train1$tv_there)
valid1$tv_there <- as.factor(valid1$tv_there)

# Checking the significant difference 

good <- filter(train1, tv_there == "1")
no_good <- filter(train1, tv_there == "0")
view(train1)


num_vars <- c("host_total_listings_count" , "accommodates", "bedrooms", "beds", "price", "maximum_minimum_nights", "availability_90", "number_of_reviews", "review_scores_cleanliness", "calculated_host_listings_count_entire_homes")

for (var in num_vars) {
  t_test <- t.test(good[[var]], no_good[[var]])
  cat(var, "\n")
  cat("p-value:", t_test$p.value, "\n\n")
}
# Through this, we can determine that bedrooms, maximum_minimum_nights, availability_90 and review_scores_cleanliness will not contribute much in the model se we are removing them. 
# Removing mentioned variables

train1 = subset(train1, select = -c(bedrooms, maximum_minimum_nights, availability_90, review_scores_cleanliness))
valid1 = subset(valid1,select= -c(bedrooms, maximum_minimum_nights, availability_90, review_scores_cleanliness))
knear = subset(knear,select= -c(bedrooms, maximum_minimum_nights, availability_90, review_scores_cleanliness))

# Normalizing all the available dataset

knear.new<-data.frame(host_total_listings_count=16,accommodates=3,beds=1,price=4467,number_of_reviews= 20,calculated_host_listings_count_entire_homes=14)

train.norm <- train1
valid.norm <- valid1
knear.norm <- knear

# Now I have undertaken preProcess() from the caret package to normalize the data as follows:

norm.values <- preProcess(train1[,c(1:3,5,6,7)], method=c("center", "scale"))
train.norm[,c(1:3,5,6,7)] <- predict(norm.values, train1[,c(1:3,5,6,7)])
valid.norm[,c(1:3,5,6,7)] <- predict(norm.values, valid1[,c(1:3,5,6,7)])
knear.norm[,c(1:3,5,6,7)] <- predict(norm.values, knear[,c(1:3,5,6,7)])
knk.norm<- predict(norm.values, knear.new)
train.norm <- as.data.frame(train.norm)
view(train1)
# KNN Function

knn<- FNN::knn(train = train.norm[,c(1:3,5,6,7)], test = knk.norm,
               cl = train.norm$tv_there, k = 6, prob = TRUE)
knn
# Here are the six nearest neighbors 

neigh <- train.norm[c(49 , 433 ,  42,   576,  102,   56),]
neigh

# Through this we can consider that there is combination of amenities that include tv and wifi.

if (knn == 1) {
  print("great")
} else {
  print("not great")
}



accuracy <- data.frame(k = seq(1, 12, 1), accuracy = rep(0, 12))

for(i in 1:12) {
  knear.pred <- FNN::knn(train.norm[,c(1:3,5,6,7)], valid.norm[,c(1:3,5,6,7)],
                         cl = train.norm$tv_there, k = i)
  accuracy[i, 2] <- confusionMatrix(knear.pred, valid.norm$tv_there)$overall[1]
}

print(accuracy)

# k value visualization

ggplot(accuracy,  aes(x=k, y=accuracy, color = "firebrick")) + geom_point(size = 3, alpha = 0.7) + geom_line()

# Second Visualization 

ggplot(accuracy, aes(x = k, y = accuracy, fill = factor(k))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  xlab("number of nearest neighbors (k)") +
  ylab("Accuracy") +
  ggtitle("Accuracy vs. Number of nearest neighbors") +
  scale_fill_discrete(name = "k")

# After interpreting both the visualizations, we believe that K=3 is the great.

knn2<- FNN::knn(train = train.norm[,c(1:3,5,6,7)], test = knk.norm,
                cl = train.norm$tv_there, k = 3, prob = TRUE)
knn2

# Nearest neighbors

neigh1 <- train.norm[c(49 , 433 ,  42),]
neigh1

#------------------------------------------------------------------------------------------------
# Naive Bayes

view(my_data)
str(my_data$instant_bookable)
class(my_data$instant_bookable)
sum(is.na(my_data$instant_bookable))

colnames(my_data)

# Creating the dataset

naive <- as.data.frame(my_data[,c(33, 34, 35, 37, 57,70 )])
view(naive)
class(naive$property_type)

# Before proceeding further we have convert characters variable to factor variables as naive bayes requires only categorical varaibles and used with large data sets

naive$property_type <- as.factor(naive$property_type)
naive$room_type <- as.factor(naive$room_type)
naive$accommodates <- as.factor(naive$accommodates)
naive$bathrooms_text <- as.factor(naive$bathrooms_text)
naive$number_of_reviews <- as.factor(naive$number_of_reviews)
naive$instant_bookable <- as.factor(naive$instant_bookable)

# Split the data

train <- sample(row.names(naive), 0.6*dim(naive)[1])
valid <- setdiff(row.names(naive), train)
trainai <- naive[train, ]
validnai <- naive[valid, ]

# Naive Bayes model

nai_b <- naiveBayes(instant_bookable ~ ., data= trainai)
nai_b

# confusion matrix that compares the performance of our model against the training data, and another that shows its performance against the validation data

trainpd <- predict(nai_b, newdata= trainai)
confusionMatrix(trainpd, trainai$instant_bookable)

# Validation set

validpd <- predict(nai_b, newdata=  validnai)
confusionMatrix(validpd,  validnai$instant_bookable)

# ANS 2, Described a fictional apartment

fictionalap <- data.frame(property_type="Entire rental unit", room_type= "Entire home/apt", 
                          accommodates= "2", bathrooms_text= "1", number_of_reviews="104")


#Model

ficpd <- predict(nai_b, newdata = fictionalap , type = "raw")
ficpd

#------------------------------------------------------------------------------------------------
# Classification Tree

view(my_data)
colnames(my_data)

# Creating the dataset

class <- as.data.frame(my_data[,c(19,24,27,33,34,35,37,38,39,41,43,44,45,
                                  46,51,52,53,54,55,62,70,71,72)])

view(traintree)
# Binning the review score rating

unique(class$review_scores_rating)

class$review_scores_rating<-discretize(class$review_scores_rating, method="frequency", 
                                       breaks=4, labels=c("poor","average","good"))

# Split the Dataset

set.seed(699)


train <- sample(row.names(class), 0.6*dim(class)[1])
valid <- setdiff(row.names(class), train)
traintree <- class[train, ]
validtree <- class[valid, ]

# Classification Tree

tree<-rpart(review_scores_rating~.,method="class",data=traintree)
tree
printcp(tree)
summary(traintree)

# Cross Validation

cv_results <- data.frame(size = integer(), error_rate = double())

for (i in 1:10) {
  set.seed(699)
  cv_model <- rpart(review_scores_rating ~ ., data = traintree, method = "class", cp = i/100)
  cv_results <- rbind(cv_results, data.frame(size = i/100, error_rate = 
                                               cv_model$cptable[which.min(cv_model$cptable[,"xerror"]),"xerror"]))
}

plot(cv_results$size, cv_results$error_rate, type = "b", xlab = "Tree Size", 
     ylab = "Cross-Validation Error Rate")

print(cv_results)


result <- print(tree2$cptable)

result1 <- data.frame(result)
print(result1)
which.min(result1$xerror)

# plotting the tree model

rpart.plot(tree, type = 2, extra = 110,box.col = c("lightblue", "salmon"))

# Same tree with different parameters

prp(tree, type = 1, extra = 3, split.font = 1, varlen = -4, fallen.leaves = FALSE , cex = 0.4,  
    branch.lty = 2, box.col = c("lightblue", "salmon"), branch.type = 3, branch.lwd = 1.5,
    main = "Tree model for review score rating")

# Plotting second tree

tree2<-rpart(review_scores_rating~.,method="class",data=traintree,cp=0.0127,minsplit=0)
tree2

rpart.plot(tree2, type = 2,box.col = c("lightblue", "salmon"))


# Confuison matrix for the tree 


tree.pred <- predict(tree,traintree,type = "class")
confusionMatrix(tree.pred, traintree$review_scores_rating)

c <- table(traintree$review_scores_rating, tree.pred)
c
table(traintree$review_scores_rating)

# confusion matrix for validation set

validtree$property_type <- factor(validtree$property_type, levels = levels(traintree$property_type))

tree.pred <- predict(tree,validtree,type = "class")
confusionMatrix(tree.pred, validtree$review_scores_rating)

#------------------------------------------------------------------------------------------------


#K means clustering 

#step 1 - creating the new data frame from the existing data my_data

k_means <- my_data[,c(17,18,35,38,41,42,43,44,45,46,47,48,49)]
view(k_means)

#Step 2 finding the corelation between the numeric values 

cor(k_means[,c(6:13)])

k_means <- my_data[,c(17,18,34,35,40,38,41,42,43,44,45,46,47,48,49)]

#Step 3 -Scalling 
km.norm <- sapply(k_means[,c(6:13)], scale)
view(km.norm)

#Step 4 creating the elbow chart 
#installing the libraries factoextra and cluster to to make the elbow chart 
#and do the further analysis 

library(factoextra)
library(cluster)

elbow <- fviz_nbclust(km.norm,kmeans,method="wss",nstart=130)
elbow

#step 5 
#Making the clusters 
k1 <- kmeans(km.norm,centers = 4, nstart = 130)
k1$centers

#step 4 
k_means <- k_means %>% mutate(Cluster = k1$cluster)
cluster<-k1$cluster
view(cluster)

#Step 5 
#Data Visualization 

#visualization for Room type

ggplot(data = k_means, aes(y = cluster)) +geom_bar (aes(fill = room_type)) +theme(plot.title = element_text(hjust = 0.3))

#cluster count for accommodates

ggplot(k_means, aes(x =price, y = accommodates, color = as.factor(cluster))) + geom_point()


# Count of Clusters by price

ggplot(k_means, aes(y = cluster)) +geom_bar(aes(color = price), fill = "salmon") +
  theme(plot.title = element_text(hjust = 0.3))

#### 

k_means2 <- k_means[,c(4,5,6)]
colnames(k_means2)




