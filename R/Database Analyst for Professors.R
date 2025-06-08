# Load required libraries
library(tidyverse)

# Read the dataset
rmp <- read_csv("/Users/rodri/Downloads/clean_ratemyprofessors.csv")

## Clean and Prepare
# Check and impute NA values in Would_Take_Again if needed
rmp <- rmp %>%
  mutate(Would_Take_Again = if_else(is.na(Would_Take_Again), 0, Would_Take_Again))

# Departmental Summary
dept_summary <- rmp %>%
  group_by(Department) %>%
  summarise(
    Professors = n(),
    Avg_Rating = round(mean(Avg_Rating, na.rm = TRUE), 1),
    Avg_Difficulty = round(mean(Avg_Difficulty, na.rm = TRUE), 1),
    Would_Take_Again = round(mean(Would_Take_Again, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Professors))

print(dept_summary)

# Top Rated Professors
top_professors <- rmp %>%
  filter(Total_Ratings >= 30) %>%
  arrange(desc(Avg_Rating)) %>%
  select(Professor_Name, Department, Avg_Rating, Total_Ratings, Would_Take_Again)

head(top_professors, 10)

# Visualization
# Ratings vs Difficulty
ggplot(rmp, aes(x = Avg_Difficulty, y = Avg_Rating)) +
  geom_point(aes(size = Total_Ratings, color = Would_Take_Again), alpha = 0.7) +
  scale_color_gradient(low = "red", high = "green") +
  labs(title = "Rating vs Difficulty",
       x = "Average Difficulty", y = "Average Rating", color = "% Would Take Again") +
  theme_minimal()

# Histogram of Average Ratings
ggplot(rmp, aes(x = Avg_Rating)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Average Ratings", x = "Average Rating", y = "Frequency") +
  theme_minimal()
