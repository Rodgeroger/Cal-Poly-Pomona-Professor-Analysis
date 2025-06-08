# Load necessary libraries
library(tidyverse)
library(lubridate)

# Read the dataset
reviews <- read.csv("/Users/rodri/Downloads/clean_reviews.csv")

# Convert Review_Date to Date type
reviews <- reviews %>%
  mutate(Review_Date = ymd(Review_Date))

# Check structure and missing values
glimpse(reviews)
sum(is.na(reviews))

## Summary Statistics
# Overall Quality and Difficulty per Department
dept_summary <- reviews %>%
  group_by(Department) %>%
  summarise(
    Count = n(),
    Avg_Quality = round(mean(Quality, na.rm = TRUE), 1),
    Avg_Difficulty = round(mean(Difficulty, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Count))

print(dept_summary)

## Time Trend of Reviews
reviews_by_month <- reviews %>%
  mutate(Month = floor_date(Review_Date, "month")) %>%
  group_by(Month) %>%
  summarise(
    Avg_Quality = round(mean(Quality, na.rm = TRUE), 1),
    Avg_Difficulty = round(mean(Difficulty, na.rm = TRUE), 1),
    Reviews = n()
  )

ggplot(reviews_by_month, aes(x = Month)) +
  geom_line(aes(y = Avg_Quality, color = "Quality"), size = 1) +
  geom_line(aes(y = Avg_Difficulty, color = "Difficulty"), size = 1) +
  scale_color_manual(values = c("Quality" = "blue", "Difficulty" = "red")) +
  labs(title = "Monthly Trends: Quality vs Difficulty",
       x = "Month", y = "Score", color = "Metric") +
  theme_minimal()


## Professor-Level Analysis
top_professors <- reviews %>%
  group_by(Professor_Name) %>%
  summarise(
    Reviews = n(),
    Avg_Quality = round(mean(Quality, na.rm = TRUE), 1),
    Avg_Difficulty = round(mean(Difficulty, na.rm = TRUE), 1),
    Total_Thumbs = sum(Thumbs_Up + Thumbs_Down)
  ) %>%
  filter(Reviews >= 20) %>%
  arrange(desc(Avg_Quality))

head(top_professors, 10)

# Thumbs Up vs Down Visualization
ggplot(reviews, aes(x = Thumbs_Up, y = Thumbs_Down)) +
  geom_point(alpha = 0.4) +
  labs(title = "Thumbs Up vs Thumbs Down Distribution",
       x = "Thumbs Up", y = "Thumbs Down") +
  theme_minimal()

# Word Frequency from Comments (Optional)
library(tidytext)
library(textdata)

# Tokenize and clean
word_freq <- reviews %>%
  unnest_tokens(word, Comment) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE)

head(word_freq, 20)
