# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)

# Load the data
data <- read.csv("training_1600000_processed_noemoticon.csv")

# Remove the time zone abbreviation and parse the date
data <- data %>%
  mutate(X3 = str_remove(X3, " PDT")) %>%  # Remove 'PDT' or adjust based on the actual time zone in your data
  mutate(X3 = as.POSIXct(X3, format = "%a %b %d %H:%M:%S %Y"))

head(data)
# Check for missing values
missing_vals <- sapply(data, function(x) sum(is.na(x)))
print(missing_vals)

# Remove rows with missing values if necessary
data <- na.omit(data)

# Sample the data if it's too large to process
set.seed(123)
sampled_data <- data %>% sample_n(100000)

# Basic statistics
summary(sampled_data)

# EDA - Tweet Frequency Over Time
sampled_data %>%
  mutate(date = as.Date(X3)) %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  labs(title = "Number of Tweets Over Time", x = "Date", y = "Tweet Count")

# Sentiment Analysis (assuming X1 is sentiment)
sentiment_distribution <- table(sampled_data$X1)
barplot(sentiment_distribution, main="Sentiment Distribution", xlab="Sentiment", ylab="Count")

# Count the number of tweets per user
user_activity <- sampled_data %>%
  group_by(X5) %>%
  summarise(tweet_count = n()) %>%
  arrange(desc(tweet_count))

# Plot the top 10 most active users
top_users <- user_activity %>%
  top_n(10, tweet_count)

ggplot(top_users, aes(x = reorder(X5, tweet_count), y = tweet_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Active Users", x = "User", y = "Number of Tweets")



# Extract hour from timestamp
sampled_data <- sampled_data %>%
  mutate(hour = hour(X3))

# Plot tweet counts by hour of the day
ggplot(sampled_data, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Tweets by Hour of Day", x = "Hour of Day", y = "Number of Tweets")


# Extract hashtags from tweets
sampled_data <- sampled_data %>%
  mutate(hashtags = str_extract_all(X6, "#\\S+"))

# Unnest the hashtags for analysis
hashtags <- sampled_data %>%
  unnest(hashtags) %>%
  filter(!is.na(hashtags))

# Count the most common hashtags
hashtag_counts <- hashtags %>%
  group_by(hashtags) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the top 10 most common hashtags
top_hashtags <- hashtag_counts %>%
  top_n(10, count)

ggplot(top_hashtags, aes(x = reorder(hashtags, count), y = count)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Most Common Hashtags", x = "Hashtag", y = "Count")


# Chi-square test for sentiment distribution
observed <- table(sampled_data$X1)
expected <- rep(mean(observed), length(observed))
chisq.test(observed, p = expected/sum(expected))

# Group by time periods (e.g., by day or month)
time_series <- sampled_data %>%
  mutate(date = as.Date(X3)) %>%
  group_by(date, X1) %>%
  summarise(count = n())

# Convert to a wide format for easier analysis
time_series_wide <- time_series %>%
  pivot_wider(names_from = X1, values_from = count, values_fill = 0)

# ANOVA to see if there's a significant difference in sentiment over time
aov_results <- aov(count ~ date + X1, data = time_series)
summary(aov_results)
