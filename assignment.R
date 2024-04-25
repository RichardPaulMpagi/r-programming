# Load necessary libraries
library(dplyr)

# Create Online Sales Data
online_sales <- data.frame(
  customer_id = c(1, 2, 3, 4, 5),
  product_purchased = c("A", "B", "C", "D", "E"),
  transaction_timestamp = as.POSIXct(c("2024-04-01 08:00:00", "2024-04-01 10:00:00", "2024-04-02 09:00:00", "2024-04-02 11:00:00", "2024-04-03 08:00:00")),
  order_value = c(100, 150, 80, 200, 120)
)

# Create In-Store Sales Data
instore_sales <- data.frame(
  customer_id = c(6, 7, 8, 9, 10),
  product_purchased = c("A", "B", "C", "D", "E"),
  transaction_timestamp = as.POSIXct(c("2024-04-01 08:00:00", "2024-04-01 10:00:00", "2024-04-02 09:00:00", "2024-04-02 11:00:00", "2024-04-03 08:00:00")),
  store_location = c("Store1", "Store2", "Store1", "Store3", "Store2")
)

# Create Customer Demographic Data
customer_demographics <- data.frame(
  customer_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  age = c(25, 30, 35, 40, 45, 30, 35, 40, 45, 50),
  gender = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F"),
  income_level = c("Low", "Medium", "High", "Low", "High", "Medium", "Low", "High", "Medium", "Low"),
  location = c("City1", "City2", "City3", "City1", "City2", "City1", "City2", "City3", "City1", "City2")
)

# Create Product Inventory Data
product_inventory <- data.frame(
  product_purchased = c("A", "B", "C", "D", "E"),
  stock_level = c(100, 150, 200, 100, 120),
  SKU = c("SKU1", "SKU2", "SKU3", "SKU4", "SKU5"),
  product_category = c("Category1", "Category2", "Category3", "Category1", "Category2")
)

# Create Marketing Campaign Data
marketing_campaign <- data.frame(
  campaign_type = c("Email", "Social Media", "Online Ads"),
  duration = c(7, 10, 5),
  target_audience = c("All", "Female", "Male"),
  outcomes = c("Increased sales by 10%", "Increased engagement by 20%", "Reached new customer segment")
)

# Merge data from all sources
merged_data <- online_sales %>%
  full_join(instore_sales, by = c("customer_id", "product_purchased", "transaction_timestamp")) %>%
  full_join(customer_demographics, by = "customer_id") %>%
  full_join(product_inventory, by = "product_purchased") %>%
  left_join(marketing_campaign, by = character())

# Print merged data
print(merged_data)

#task 3
# Check for missing values
missing_values <- sapply(merged_data, function(x) sum(is.na(x)))
print(missing_values)

# Remove duplicates
cleaned_data <- merged_data %>%
  distinct()

# Standardize formats if needed
# For example, converting character fields to factors
cleaned_data$gender <- as.factor(cleaned_data$gender)
cleaned_data$income_level <- as.factor(cleaned_data$income_level)
cleaned_data$location <- as.factor(cleaned_data$location)
cleaned_data$product_category <- as.factor(cleaned_data$product_category)
cleaned_data$campaign_type <- as.factor(cleaned_data$campaign_type)
cleaned_data$target_audience <- as.factor(cleaned_data$target_audience)

# Print cleaned data
print(cleaned_data)

#task 4
# Merge sales data with customer demographic data
merged_sales_demographic <- merge(online_sales, customer_demographics, by = "customer_id", all.x = TRUE)

# Print merged data
print(merged_sales_demographic)

#task 5
# Load necessary libraries
library(plotrix)

# Summary statistics
summary(merged_sales_demographic)

# Histogram of customer age
hist(merged_sales_demographic$age, col = "blue", xlab = "Age", main = "Histogram of Customer Age")

# Boxplot of order value by gender
boxplot(merged_sales_demographic$order_value ~ merged_sales_demographic$gender, col = "blue", xlab = "Gender", ylab = "Order Value", main = "Order Value by Gender")

# Scatterplot of order value vs. age
plot(merged_sales_demographic$age, merged_sales_demographic$order_value, 
     xlab = "Age", ylab = "Order Value", main = "Scatterplot of Order Value vs. Age")

# Boxplot of order value by gender
boxplot(merged_sales_demographic$order_value ~ merged_sales_demographic$age, merged_data,main = "boxplot of Order Value vs. Age", xlab = "Age", ylab = "Order Value")

# Correlation matrix
correlation_matrix <- cor(merged_sales_demographic[, c("age", "order_value")])
print(correlation_matrix)

#task 6
# Check column names
expected_columns <- c("customer_id", "product_purchased", "transaction_timestamp", 
                      "order_value", "age", "gender", "income_level", "location")
if(all(expected_columns %in% colnames(merged_sales_demographic))) {
  print("All expected columns are present.")
} else {
  print("Some expected columns are missing.")
}

# Check for any unexpected values
# For example, check unique values in gender and product_category columns
unique_genders <- unique(merged_sales_demographic$gender)
print(unique_genders)

unique_product_categories <- unique(merged_sales_demographic$product_category)
print(unique_product_categories)

# Check for data integrity and consistency
# For example, ensure that order_value is non-negative
if(all(merged_sales_demographic$order_value >= 0)) {
  print("Order values are non-negative.")
} else {
  print("Some order values are negative. Check data integrity.")
}

# Check for consistency between age and income_level
income_levels <- unique(merged_sales_demographic$income_level)
for(level in income_levels) {
  cat("Summary statistics for age where income level is", level, ":\n")
  print(summary(merged_sales_demographic$age[merged_sales_demographic$income_level == level]))
}

