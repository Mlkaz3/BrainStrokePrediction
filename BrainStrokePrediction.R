# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("C:/Users/User/Downloads/brain_stroke_data.csv")
print(data)

# to preview o data columns/ variables
str(data)


# 1. Histogram of the Age column
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +  # Improved color scheme
  stat_bin(binwidth = 5, aes(label = after_stat(count)), geom = "text", vjust = -0.5, color = "black") +  # Updated deprecated notation
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal(base_size = 14) +  # Adjusted base text size for clarity
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "darkblue"),  # Centered and styled title
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80")  # Softer gridlines
  )


# 2. Boxplot of BMI levels by Sex
# Calculate summary statistics for BMI
summary_stats <- data %>%
  group_by(gender) %>%
  summarise(
    Min = min(bmi, na.rm = TRUE),
    Q1 = quantile(bmi, 0.25, na.rm = TRUE),
    Median = median(bmi, na.rm = TRUE),
    Q3 = quantile(bmi, 0.75, na.rm = TRUE),
    Max = max(bmi, na.rm = TRUE),
    .groups = 'drop'
  )

# Plot with ggplot2
ggplot(data, aes(x = factor(gender), y = bmi, fill = factor(gender))) +
  geom_boxplot() +
  geom_text(data = summary_stats, aes(x = factor(gender), y = Min, label = paste("Min:", round(Min, 1))), vjust = -1) +
  geom_text(data = summary_stats, aes(x = factor(gender), y = Q1, label = paste("Q1:", round(Q1, 1))), vjust = -1) +
  geom_text(data = summary_stats, aes(x = factor(gender), y = Median, label = paste("Median:", round(Median, 1))), vjust = -1) +
  geom_text(data = summary_stats, aes(x = factor(gender), y = Q3, label = paste("Q3:", round(Q3, 1))), vjust = -1) +
  geom_text(data = summary_stats, aes(x = factor(gender), y = Max, label = paste("Max:", round(Max, 1))), vjust = -1) +
  labs(title = "BMI Levels by Gender", x = "Gender", y = "BMI") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()




# 3. Scatter plot of age vs average glucose level/ age vs bmi 

ggplot(data, aes(x = age, y = avg_glucose_level)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "Age vs Average Glucose Level", x = "Age (years)", y = "Average Glucose Level") +
  theme_minimal()


ggplot(data, aes(x = age, y = bmi)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "Age vs BMI", x = "Age (years)", y = "BMI") +
  theme_minimal()






# 4. Barplot of Work Type
library(ggplot2)
library(dplyr)

# Calculate counts for each work type
count_data <- data %>%
  group_by(work_type) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(data, aes(x = factor(work_type))) +
  geom_bar(fill = "orange", color = "black") +
  geom_text(data = count_data, aes(x = factor(work_type), y = Count, label = Count), vjust = -0.5) +
  labs(title = "Work Type Distribution", x = "Work Type", y = "Count") +
  theme_minimal()




# 5. Pie chart of the Target variable
# Calculate counts for each stroke status
stroke_counts <- data %>%
  count(stroke) %>%
  rename(Count = n)

# Create pie chart with ggplot2
ggplot(stroke_counts, aes(x = "", y = Count, fill = factor(stroke))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Stroke Status Distribution", fill = "Stroke Status") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "darkblue"),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white")




# 6. Histogram Grid of BMI by Target (Stroke)
# Create a refined and colorful grid of histograms for BMI
ggplot(data, aes(x = bmi, fill = factor(stroke))) + 
  geom_histogram(binwidth = 1, color = "white", alpha = 0.7, position = "identity") +
  facet_wrap(~stroke) +
  scale_fill_manual(values = c("lightcoral", "lightseagreen")) +  # Color palette for stroke status
  theme_minimal(base_size = 14) +  # Adjust text size for clarity
  labs(title = "Distribution of BMI by Stroke Status", x = "BMI", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "darkblue"),  # Center and style the title
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),  # Softer gridlines
    strip.background = element_rect(fill = "lightgrey", color = "black"),  # Style facet labels
    strip.text = element_text(face = "bold")  # Bold facet labels
  ) 




# 7. Box Plot
ggplot(data, aes(x = as.factor(work_type), fill = as.factor(gender))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("lightgreen", "#D8BFD8")) +  # Light green and light purple
  labs(title = "Distribution of Work Type by Gender",
       x = "Work Type",
       y = "Count",
       fill = "Gender") +
  theme_minimal()



# 8. Pie Chart: Distribution of Gender by Stroke 
# Prepare the data for the pie chart
pie_data <- data %>%
  group_by(stroke, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(stroke) %>%
  mutate(percentage = count / sum(count) * 100)

# Create a pie chart with labels
ggplot(pie_data, aes(x = "", y = percentage, fill = as.factor(gender))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~stroke) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(count = ", count, ")")), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Distribution of Gender by Stroke Status",
       x = "",
       y = "Percentage",
       fill = "Gender (0 = Female, 1 = Male)") +
  theme_void() +
  theme(legend.position = "right")








##################

library(ggplot2)
library(dplyr)

# Calculate counts for each work type by stroke status
count_data <- data %>%
  group_by(work_type, stroke) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(count_data, aes(x = factor(work_type), y = Count, fill = factor(stroke))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  labs(title = "Work Type Distribution by Stroke Status", x = "Work Type", y = "Count", fill = "Stroke Status") +
  theme_minimal()


##################




library(ggplot2)
library(dplyr)

# Calculate counts for each work type by stroke status
count_data <- data %>%
  group_by(work_type, stroke) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(count_data, aes(x = factor(work_type), y = Count, fill = factor(work_type))) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ stroke, labeller = labeller(stroke = c("0" = "No Stroke", "1" = "Stroke"))) +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Work Type Distribution by Stroke Status", x = "Work Type", y = "Count") +
  theme_minimal()
