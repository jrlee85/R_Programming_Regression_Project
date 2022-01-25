# Install Packages
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("here")

##############################

# Load libraries
library("gridExtra")
library("tidyverse")
library("here")

##############################

# Load data set
# Import with UTF-8 encoding to accommodate Portuguese characters
airbnb_df <- read.csv(here("listings.csv"), encoding="UTF-8")

##############################
# Data Checking, Cleaning and Manipulation 

View(airbnb_df)
# View column names to easily identify columns to keep
colnames(airbnb_df)
cols_to_keep <- c(1,15:18,24:26, 28,30,31,33,34,36:42,56,61:67,69)
airbnb_df<-airbnb_df[colnames(airbnb_df[cols_to_keep])]
str(airbnb_df)

sapply(airbnb_df, function(x) sum(is.na(x)))

airbnb_df <- airbnb_df[!is.na(airbnb_df$review_scores_rating),]

# Necessary to split out host verifications and amenities into separate columns
airbnb_df$host_verifications <- sub("[", "", airbnb_df$host_verifications, fixed=TRUE)
airbnb_df$host_verifications <- sub("]", "", airbnb_df$host_verifications, fixed=TRUE)
airbnb_df$host_verifications <- gsub("'", "", airbnb_df$host_verifications, fixed=TRUE)
airbnb_df$host_verifications <- strsplit(airbnb_df$host_verifications, split = ",")
verification_methods <- c()
verification_methods <- append(verification_methods, airbnb_df$host_verifications)
verification_methods <- unlist(verification_methods)
unique(verification_methods)
verification_methods <- gsub(" ", "", verification_methods, fixed=TRUE)
verification_methods <- unique(verification_methods)
length(verification_methods)
verification_methods

# Repeat for amenities
airbnb_df$amenities <- sub("[", "", airbnb_df$amenities, fixed=TRUE)
airbnb_df$amenities <- sub("]", "", airbnb_df$amenities, fixed=TRUE)
airbnb_df$amenities <- gsub('"', "", airbnb_df$amenities, fixed=TRUE)
airbnb_df$amenities <- strsplit(airbnb_df$amenities, split = ",")
amenities <- c()
amenities <- append(amenities, airbnb_df$amenities)
amenities <- unlist(amenities)
amenities <- unique(amenities)
length(amenities)
amenities[0:25]
# Too many to fix, so drop
airbnb_df<-airbnb_df %>% select(-amenities)

# Manual one-hot encoding
for (v in verification_methods) {
    airbnb_df[v] <- as.integer(grepl(v, airbnb_df$host_verifications))
}

airbnb_df<-airbnb_df %>% select(-host_verifications)

airbnb_df$host_response_time <- ordered(factor(airbnb_df$host_response_time, levels=c("within an hour", "within a few hours", "within a day", "a few days or more")))

# Function removes characters and convert to doubles
conversion_function <- function(column, value) {
    column <- as.double(sub(value, "", column, fixed=TRUE))
}
airbnb_df$host_response_rate <- conversion_function(airbnb_df$host_response_rate, "%")
airbnb_df$host_acceptance_rate <- conversion_function(airbnb_df$host_acceptance_rate, "%")
airbnb_df[c("host_acceptance_rate", "host_response_rate")] <- airbnb_df[c("host_acceptance_rate", "host_response_rate")]/100
airbnb_df$price <- conversion_function(airbnb_df$price, "$")
# Rename price column so that the currency is clear
airbnb_df <- airbnb_df %>% rename(Price_BRL = price)

unique(airbnb_df$bathrooms_text)
airbnb_df$bathrooms_text <- sub("private ", "", airbnb_df$bathrooms_text)
airbnb_df$bathrooms_text <- sub("Private ", "1 ", airbnb_df$bathrooms_text)
# Caret included so as not to change 20 shared baths
airbnb_df$bathrooms_text <- sub("^0 shared baths", "0 baths", airbnb_df$bathrooms_text)
airbnb_df$bathrooms_text <- sub("Shared half-bath", "0.5 shared baths", airbnb_df$bathrooms_text)
airbnb_df$bathrooms_text <- sub("Half-bath", "0.5 baths", airbnb_df$bathrooms_text)
# Unordered factor due to ambiguity in ordering
airbnb_df$bathrooms_text <- factor(airbnb_df$bathrooms_text)

# Logicals converetd to integers for modelling purposes
airbnb_df[,c(5:7,27)] <- data.frame(lapply(airbnb_df[,c(5:7,27)], function(x) {gsub("t", 1, x)}))
airbnb_df[,c(5:7,27)] <- data.frame(lapply(airbnb_df[,c(5:7,27)], function(x) {gsub("f", 0, x)}))
airbnb_df[,c(5:7,27)] <- data.frame(lapply(airbnb_df[,c(5:7,27)], function(x) {as.integer(x)}))

airbnb_df[,c(8,11)] <- data.frame(lapply(airbnb_df[,c(8,11)], function(x) {factor(x)}))

str(airbnb_df)

sapply(airbnb_df, function(x) sum(is.na(x)))

# Price important factor. No way to impute, so missing observations dropped
airbnb_df <- airbnb_df[!is.na(airbnb_df$Price_BRL),]

# Create a temporary df with some of the originally dropped columns to investigate missing values
# Create vector of mainly NULL values to avoid reading in all columns 
cols_needed <- rep("NULL", 74)
cols_needed [1] <- "numeric"
cols_needed [c(5,6,20)] <- "character"
temp_df <- read.csv(here("listings.csv"), encoding="UTF-8", colClasses = cols_needed)
# Left join used so that only rows in airbnb_df are retained
temp_df <- left_join(airbnb_df, temp_df, by="id")

profile_pic <- temp_df[,c(1,6,46)]
# If url exists, must have profile picture, otherwise not
profile_pic <- profile_pic[!profile_pic$host_has_profile_pic == 1,]
profile_pic <- profile_pic[!profile_pic$host_has_profile_pic == 0,]
unique(profile_pic$host_picture_url)
# Can assume these are false. Change in original
airbnb_df$host_has_profile_pic[is.na(airbnb_df$host_has_profile_pic)] <- 0
# Clean up
remove(profile_pic)

bathrooms <- temp_df[,c(1, 13, 44, 45)]
bathrooms <- bathrooms[bathrooms$bathrooms_text == "",]
matches <- grepl("bath|banho|banheiro", bathrooms$name)
sum(matches)
# Reuse variable 'matches' to save memory
matches <- grepl("bath|banho|banheiro", bathrooms$description)
sum(matches)
# View the descriptions in order to check no. bathrooms
bathrooms$description[matches]
# Display ids to make corrections
bathrooms$id[matches]
airbnb_df[airbnb_df$id == 231497, 13] <- "1 bath"
airbnb_df[airbnb_df$id == 231516, 13] <- "1 bath"
airbnb_df[airbnb_df$id == 916271, 13] <- "1 shared bath"
remove(bathrooms)

beds_and_bedrooms <- temp_df[,c(1,14,15,44,45)]
remove(temp_df)
bedrooms <- beds_and_bedrooms[is.na(beds_and_bedrooms$bedrooms),]
matches <- grepl("bed|quarto|cama|studio", bedrooms$name)
sum(matches)
bedrooms$name[matches]
#List of ID numbers to aid correction in main data frame
id_list <- bedrooms$id[matches]
for (n in c(1:28, 30, 32, 34, 36:39, 42:46, 49:58)) {
    airbnb_df[airbnb_df$id == id_list[n], 14] <- 0
}
for (n in c(29, 33, 35, 40:41, 48)) {
    airbnb_df[airbnb_df$id == id_list[n], 14] <- 1
}

matches <- grepl("bed|quarto|cama|studio", bedrooms$description)
sum(matches)
# 705 values too many to fix manually
remove(bedrooms)

beds <- beds_and_bedrooms[is.na(beds_and_bedrooms$beds),]
matches <- grepl("bed|cama", beds$name)
sum(matches)
beds$name[matches]
beds$id[matches]
airbnb_df[airbnb_df$id == 14298873, 15] <- 1

matches <- grepl("bed|cama", beds$description)
sum(matches)
beds$description[matches]
id_list <- beds$id[matches]
for (n in c(1, 3:5, 10, 12:15, 17:18)) {
    airbnb_df[airbnb_df$id == id_list[n], 15] <- 1
}
for (n in c(2, 6:7, 11, 16)) {
    airbnb_df[airbnb_df$id == id_list[n], 15] <- 2
}
remove(beds)
remove(beds_and_bedrooms)

sapply(airbnb_df, function(x) sum(is.na(x)))

##############################
# Exploratory Analysis

summary(airbnb_df)

boxplot(airbnb_df$minimum_nights)
# Set y axis to max 50 to get better picture of data
ggplot(data = airbnb_df, mapping=aes(x = minimum_nights)) + geom_histogram(binwidth=50) + coord_cartesian(ylim = c(0,50))

sum(airbnb_df$minimum_nights>IQR(airbnb_df$minimum_nights)+quantile(airbnb_df$minimum_nights, 0.75))
# 2,140 - too many to drop
sum(airbnb_df$minimum_nights>14)
# Instead drop greater than 14 nights (304)
airbnb_df <- airbnb_df[!airbnb_df$minimum_nights > 14,]
ggplot(data = airbnb_df, mapping=aes(x = minimum_nights)) + geom_histogram(binwidth=0.5)


boxplot(airbnb_df$maximum_nights)
ggplot(data = airbnb_df, mapping=aes(x = maximum_nights)) + geom_histogram(binwidth = 300000000) + coord_cartesian(ylim = c(0,50))
max_nights <- airbnb_df %>% select(maximum_nights)
max_nights <- max_nights %>% arrange(desc(maximum_nights))
View(max_nights)
remove(max_nights)
# Use 1125 as cut off
airbnb_df$maximum_nights[airbnb_df$maximum_nights > 1125] <- 1125
ggplot(data = airbnb_df, mapping=aes(x = maximum_nights)) + geom_histogram(binwidth = 50)

ggplot(data = airbnb_df, mapping=aes(x = review_scores_rating)) + geom_histogram(binwidth=10)
# Get better picture by zooming in on higher scores
ggplot(data = airbnb_df[airbnb_df$review_scores_rating>75,], mapping=aes(x = review_scores_rating)) + geom_histogram(binwidth=1)

# Save plots to variables in order to plot together
ggp1 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_accuracy)) + geom_bar()
ggp2 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_cleanliness)) + geom_bar()
ggp3 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_checkin)) + geom_bar()
ggp4 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_communication)) + geom_bar()
ggp5 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_location)) + geom_bar()
ggp6 <- ggplot(data = airbnb_df, mapping=aes(x = review_scores_value)) + geom_bar()
grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ggp6, ncol = 2)
remove(ggp1, ggp2, ggp3, ggp4, ggp5, ggp6)


ggplot(data = airbnb_df, mapping=aes(x = Price_BRL)) + geom_histogram(binwidth=30)

ggplot(airbnb_df, aes(x=Price_BRL, y=review_scores_rating)) +geom_point()

# Remove unreadable x axis text from plot
ggplot(airbnb_df, aes(x=neighbourhood_cleansed, y=review_scores_rating)) +geom_point() + theme(axis.text.x = element_blank())

ggplot(airbnb_df, aes(x=neighbourhood_cleansed, y=Price_BRL)) +geom_point() + theme(axis.text.x = element_blank())

# Data frame of just numeric values in order to check correlations
numeric_columns <- airbnb_df[,sapply(airbnb_df, is.numeric)]
correlation_df <- data.frame(cor(numeric_columns, use="complete.obs"))
View(correlation_df)

# Too large to analyse, focus on subset:
smaller_corr_df <- correlation_df %>% select(review_scores_rating, Price_BRL)
smaller_corr_df <- smaller_corr_df %>% arrange(desc(review_scores_rating))
View(smaller_corr_df)
smaller_corr_df <- smaller_corr_df %>% arrange(desc(Price_BRL))
View(smaller_corr_df)
remove(correlation_df, smaller_corr_df)

pairs(airbnb_df[, c("Price_BRL", "review_scores_rating", "bedrooms", "host_is_superhost")])

# Legend covers up plot in RStudio. Instead, create avriable and save to disk
densityplot <- ggplot(data = airbnb_df, mapping = aes(x = Price_BRL, y = ..density..)) + geom_freqpoly(mapping = aes(color = neighbourhood_cleansed), binwidth = 20)
ggsave(here("ggdensity.png"), width=20, height=6, dpi=150)

neighbourhood_summary <- airbnb_df %>% group_by(neighbourhood_cleansed) %>% summarise(n=n(), average=mean(Price_BRL))
neighbourhood_summary <- neighbourhood_summary %>% arrange(desc(average))
View(neighbourhood_summary)

write.csv(airbnb_df, here("cleaned_listings.csv"), row.names = FALSE)
