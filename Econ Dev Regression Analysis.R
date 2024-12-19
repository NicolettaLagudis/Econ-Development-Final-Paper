#Load necessary library
install.packages("dplyr")
library(dplyr)
library(ggplot2)

#Pull Data
fintech_data <- read.csv("/Users/nicolettalagudis/Downloads/regression_data_complete.csv")

# Inspect the dataset 
head(fintech_data) 
summary(fintech_data)

#Organize column names
colnames(fintech_data) <- c("country", "year", "fintech_growth", "cash_pref", "bank_deposits", "vc_avail", "fintech_firms") 

#check column names
head(fintech_data) 
print(fintech_data)

# Check  to confirm no NAs 
sum(is.na(fintech_data$fintech_growth))

# Check for NAs in key regression columns for the US subset
us_data <- fintech_data %>% filter(country == "United States")
sum(is.na(us_data$fintech_growth))
sum(is.na(us_data$cash_pref))
sum(is.na(us_data$bank_deposits))
sum(is.na(us_data$vc_avail))
sum(is.na(us_data$fintech_firms))

print(na_rows)

# Check the structure of the data frame
str(us_data)
str(fintech_data)  

# Function to convert character numbers with parentheses to numeric
convert_to_numeric <- function(x) {
  x <- gsub("\\(([^)]+)\\)", "-\\1", x)  # Replace (value) with -value
  x <- gsub(",", "", x)                  # Remove commas if present
  as.numeric(x)                          # Convert to numeric
}

# Apply the cleaning function to the relevant columns
fintech_data$vc_avail <- clean_numeric(fintech_data$vc_avail)
fintech_data$fintech_firms <- clean_numeric(fintech_data$fintech_firms)

# Run regression model for Japan
japan_model <- lm(fintech_growth ~ cash_pref + bank_deposits + vc_avail + fintech_firms, 
                  data = fintech_data %>% filter(country == "Japan"))

# Summary of the Japan model
summary(japan_model)

# Run regression model for the United States
us_model <- lm(fintech_growth ~ cash_pref + bank_deposits + vc_avail + fintech_firms, 
               data = fintech_data %>% filter(country == "United States"))

# Summary of the US model
summary(us_model)

# Create dummy variables for Japan and United States
fintech_data$Japan_Dummy <- ifelse(trimws(fintech_data$country) == "Japan", 1, 0)
fintech_data$UnitedStates_Dummy <- ifelse(trimws(fintech_data$country) == "United States", 1, 0)

# Cross-check the counts of the dummy variables
table(fintech_data$Japan_Dummy)
table(fintech_data$UnitedStates_Dummy)

# Interaction model using Japan_Dummy
interaction_model <- lm(fintech_growth ~ cash_pref + bank_deposits + vc_avail * Japan_Dummy + fintech_firms, 
                        data = fintech_data)

# Summary of the interaction model
summary(interaction_model)

#Install Stargazer
install.packages("stargazer")
library(stargazer)

# Generate the regression table using stargazer
stargazer(japan_model, us_model, interaction_model,
          keep.stat = c("n", "rsq"), 
          type = "text",
          title = "Regression Results for Fintech Growth",
          digits = 2,
          out = "regression_results.txt")  # To save as a text file

