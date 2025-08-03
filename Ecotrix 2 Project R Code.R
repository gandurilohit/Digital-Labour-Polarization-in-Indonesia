# Load libraries
library(readxl)
library(dplyr)
library(fixest)  # for fixed effects regression

# Read dataset
data <- read_excel("C:/Users/Lohit/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Create required variables
data <- data %>%
  mutate(
    share_nonroutine = weighted_nonroutine / (weighted_nonroutine + weighted_routine),
    share_cognitive = weighted_cognitive / (weighted_cognitive + weighted_manual),
    log_ratio_nonroutine = log(weighted_nonroutine / weighted_routine),
    log_ratio_cognitive = log(weighted_cognitive / weighted_manual),
    tech_intensity = share_computer,  # replace with the actual column if different
    #male_female_ratio = (weighted_male / weighted_female),
    interaction1 = dci * tech_intensity,
    interaction2 = dci * share_female
  )

# Run the regression model
model_nonroutine <- feols(
  log_ratio_nonroutine ~ dci + tech_intensity + interaction1 + share_female + interaction2 +
    share_college + GDP + share_urban + Gini | Province_code + industry_code + year,
  data = data, cluster = ~Province_code
)

model_cognitive <- feols(
  log_ratio_cognitive ~ dci + tech_intensity + interaction1 + share_female + interaction2 +
    share_college + GDP + share_urban + Gini | Province_code + industry_code + year,
  data = data, cluster = ~Province_code
)

# Summary of results
etable(model_nonroutine, model_cognitive)





# Load packages
library(readxl)
library(dplyr)
library(fixest)

# Load the dataset
data <- read_excel("C:/Users/Lohit/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Define tech-intensity bins
data <- data %>%
  mutate(
    industry_tech_bin = case_when(
      industry_code %in% c(10,11,12,13) ~ "High-Tech",
      industry_code %in% c(3,4,5,15,16,17) ~ "Medium-Tech",
      industry_code %in% c(1,2,6,7,8,9) ~ "Low-Tech",
      TRUE ~ "Other"
    ),
    industry_tech_bin = factor(industry_tech_bin, levels = c("Low-Tech", "Medium-Tech", "High-Tech"))
  )


# Optional: lag dci by province-year if needed
library(data.table)
data <- data.table(data)
setorder(data, Province_code, year)
data[, lagged_dci := shift(dci, n = 1, type = "lag"), by = .(Province_code)]

# Generate log outcome variables and key regressors
data <- data %>%
  mutate(
    log_ratio_nonroutine = log(weighted_nonroutine / weighted_routine),
    log_ratio_cognitive = log(weighted_cognitive / weighted_manual),
    interaction2 = lagged_dci * share_female
  )

# Run improved models with binned industry effect and more fixed effects
model_nonroutine <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

model_cognitive <- feols(
  log_ratio_cognitive ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

# Display regression table
etable(model_nonroutine)

# Remove rows where lagged_dci is NA (first year for each province)
data <- data[!is.na(lagged_dci)]

# Run regressions using lagged DCI
model_nonroutine_lag <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

model_cognitive_lag <- feols(
  log_ratio_cognitive ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

# Display regression results
etable(model_nonroutine_lag, model_cognitive_lag)

# Load plotting library
library(ggplot2)

# Extract coefficients
coefs <- coef(model_nonroutine_lag)

# Compute marginal effects for each bin
marginal_effects <- data.frame(
  Industry = c("Low-Tech", "Medium-Tech", "High-Tech"),
  Effect = c(
    coefs["lagged_dci"],
    coefs["lagged_dci"] + coefs["lagged_dci:industry_tech_binMedium-Tech"],
    coefs["lagged_dci"] + coefs["lagged_dci:industry_tech_binHigh-Tech"]
  ),
  SE = c(
    se(model_nonroutine_lag)["lagged_dci"],
    sqrt(se(model_nonroutine_lag)["lagged_dci"]^2 + se(model_nonroutine_lag)["lagged_dci:industry_tech_binMedium-Tech"]^2),
    sqrt(se(model_nonroutine_lag)["lagged_dci"]^2 + se(model_nonroutine_lag)["lagged_dci:industry_tech_binHigh-Tech"]^2)
  )
)

# Plot
ggplot(marginal_effects, aes(x = Industry, y = Effect)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Effect - 1.96*SE, ymax = Effect + 1.96*SE), width = 0.2) +
  labs(title = "Marginal Effect of Lagged DCI on Skilled Labor Share by Industry Tech Intensity",
       y = "Effect of Lagged DCI", x = "Industry Technology Bin") +
  theme_minimal()


# Robustness Checks
library(ggplot2)
library(dplyr)


data_plot <- data %>%
  group_by(year, industry_tech_bin) %>%
  summarise(mean_share = mean(share_nonroutine_to_routine, na.rm = TRUE))

data <- data %>%
  mutate(
    share_nonroutine_to_routine = weighted_nonroutine / (weighted_nonroutine + weighted_routine)
  )

data[, share_nonroutine_to_routine := weighted_nonroutine / (weighted_nonroutine + weighted_routine)]


ggplot(data_plot, aes(x = year, y = mean_share, color = industry_tech_bin)) +
  geom_line(size = 1.2) +
  labs(title = "Trend in Nonroutine Job Share by Industry Tech Bin",
       y = "Average Nonroutine Share", x = "Year", color = "Tech Bin") +
  theme_minimal()


ggplot(data, aes(x = dci, y = share_nonroutine_to_routine)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  labs(title = "Digital Competitiveness vs Nonroutine Job Share by Year", y = "Share of Nonroutine to Routine", x = "DCI")


library(ggplot2)

ggplot(data, aes(x = dci, y = share_nonroutine_to_routine)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  labs(title = "Digital Competitiveness vs Nonroutine Job Share by Year",
       x = "Digital Competitiveness Index (DCI)",
       y = "Share of Nonroutine Workers") +
  theme_minimal()



library(dplyr)

data %>%
  summarise(
    obs = n(),
    mean_dci = mean(dci, na.rm = TRUE),
    sd_dci = sd(dci, na.rm = TRUE),
    mean_share_nonroutine = mean(share_nonroutine, na.rm = TRUE),
    sd_share_nonroutine = sd(share_nonroutine, na.rm = TRUE),
    mean_log_ratio_nonroutine = mean(log_ratio_nonroutine, na.rm = TRUE),
    sd_log_ratio_nonroutine = sd(log_ratio_nonroutine, na.rm = TRUE)
  )


data <- data %>%
  mutate(
    log_ratio_nonroutine = ifelse(weighted_nonroutine > 0 & weighted_routine > 0,
                                  log(weighted_nonroutine / weighted_routine),
                                  NA)
  )

data %>%
  group_by(industry_tech_bin) %>%
  summarise(
    obs = n(),
    mean_dci = mean(dci, na.rm = TRUE),
    mean_share_nonroutine = mean(share_nonroutine, na.rm = TRUE),
    mean_log_ratio_nonroutine = mean(log_ratio_nonroutine, na.rm = TRUE)
  )


data %>%
  group_by(industry_tech_bin, year) %>%
  summarise(
    obs = n(),
    mean_dci = mean(dci, na.rm = TRUE),
    mean_share_nonroutine = mean(share_nonroutine, na.rm = TRUE),
    mean_log_ratio_nonroutine = mean(log_ratio_nonroutine, na.rm = TRUE)
  ) %>%
  arrange(industry_tech_bin, year)


data[, share_nonroutine := weighted_nonroutine / (weighted_nonroutine + weighted_routine)]
data[, share_cognitive := weighted_cognitive / (weighted_cognitive + weighted_manual)]

feols(share_nonroutine ~ dci * industry_tech_bin + share_female + interaction2 + share_college + GDP + share_urban + Gini | Province_code + industry_code^year, cluster = ~Province_code, data = data)


data_trim <- data[Province_code != 31]  # 31 = DKI Jakarta

feols(log_ratio_nonroutine ~ dci * industry_tech_bin + share_female + interaction2 + share_college + GDP + share_urban + Gini | Province_code + industry_code^year, cluster = ~Province_code, data = data_trim)


Gender_data <- read.csv("C:/Users/Lohit/Downloads/Merged_Gender_Final_Indonesia_Panel_data.csv")
View(Gender_data)

# Load data.table if not already loaded
library(data.table)
library(fixest)
library(dplyr)


# Make sure Gender_data is a data.table
Gender_data <- as.data.table(Gender_data)

Gender_data[, log_ratio_nonroutine_male := 
              ifelse(weighted_nonroutine_m > 0 & weighted_routine_m > 0,
                     log(weighted_nonroutine_m / weighted_routine_m),
                     NA)]

Gender_data[, log_ratio_nonroutine_female := 
              ifelse(weighted_nonroutine_f > 0 & weighted_routine_f > 0,
                     log(weighted_nonroutine_f / weighted_routine_f),
                     NA)]

Gender_data[, weighted_male := ifelse(!is.na(total_weight_all) & !is.na(weighted_female),
                                      total_weight_all - weighted_female,
                                      NA)]

Gender_data[, log_ratio_nonroutine := ifelse(!is.na(weighted_nonroutine) & !is.na(weighted_routine),
                                      log(weighted_nonroutine / weighted_routine),
                                      NA)]


# Define tech-intensity bins
Gender_data <- Gender_data %>%
  mutate(
    industry_tech_bin = case_when(
      industry_code %in% c(10,11,12,13) ~ "High-Tech",
      industry_code %in% c(3,4,5,15,16,17) ~ "Medium-Tech",
      industry_code %in% c(1,2,6,7,8,9) ~ "Low-Tech",
      TRUE ~ "Other"
    ),
    industry_tech_bin = factor(industry_tech_bin, levels = c("Low-Tech", "Medium-Tech", "High-Tech"))
  )

# Optional: lag dci by province-year if needed
library(data.table)
Gender_data <- data.table(Gender_data)
setorder(Gender_data, province_code, year)
Gender_data[, lagged_dci := shift(dci, n = 1, type = "lag"), by = .(province_code)]


Gender_data_male <- Gender_data[weighted_male > 0]
Gender_data_female <- Gender_data[weighted_female > 0]

# Male sample regression
model_male <- feols(
  log_ratio_nonroutine_male ~ lagged_dci * industry_tech_bin + share_college + GDP + share_urban + Gini |
    province_code + industry_code^year,
  cluster = ~province_code,
  data = Gender_data_male
)

# Female sample regression
model_female <- feols(
  log_ratio_nonroutine_female ~ lagged_dci * industry_tech_bin + share_college + GDP + share_urban + Gini |
    province_code + industry_code^year,
  cluster = ~province_code,
  data = Gender_data_female
)

# Compare results
etable(model_male, model_female, headers = c("Male", "Female"))


library(data.table)
Gender_data <- as.data.table(Gender_data)

# Step 1: Find median
female_median <- median(Gender_data$share_female, na.rm = TRUE)

# Step 2: Create subsets
Gender_data_female_heavy <- Gender_data[share_female > female_median]
Gender_data_male_heavy <- Gender_data[share_female <= female_median]

# Female-Heavy Provinces Regression
model_female_heavy <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_college + GDP + share_urban + Gini |
    province_code + industry_code^year,
  cluster = ~province_code,
  data = Gender_data_female_heavy
)

# Male-Heavy Provinces Regression
model_male_heavy <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_college + GDP + share_urban + Gini |
    province_code + industry_code^year,
  cluster = ~province_code,
  data = Gender_data_male_heavy
)

etable(model_female_heavy, model_male_heavy, headers = c("Female-Heavy", "Male-Heavy"))

library(ggplot2)
library(broom)  # for tidy(model)

# Step 1: Extract coefficients and standard errors
coef_male <- tidy(model_male)
coef_female <- tidy(model_female)

# Step 2: Focus on key variables you want to compare
coef_male <- coef_male %>%
  filter(term %in% c("lagged_dci", "lagged_dci:industry_tech_binMedium-Tech", "lagged_dci:industry_tech_binHigh-Tech")) %>%
  mutate(Group = "Male")

coef_female <- coef_female %>%
  filter(term %in% c("lagged_dci", "lagged_dci:industry_tech_binMedium-Tech", "lagged_dci:industry_tech_binHigh-Tech")) %>%
  mutate(Group = "Female")

# Step 3: Combine both into one dataset
coef_combined <- bind_rows(coef_male, coef_female)

# Step 4: Clean variable names for plotting
coef_combined <- coef_combined %>%
  mutate(term_clean = case_when(
    term == "lagged_dci" ~ "Lagged DCI (Low-Tech)",
    term == "lagged_dci:industry_tech_binMedium-Tech" ~ "Lagged DCI × Medium-Tech",
    term == "lagged_dci:industry_tech_binHigh-Tech" ~ "Lagged DCI × High-Tech"
  ))

# Step 5: Plot
ggplot(coef_combined, aes(x = term_clean, y = estimate, fill = Group)) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                position = position_dodge(0.6), width = 0.2) +
  labs(title = "Impact of Digital Competitiveness on Skilled Employment",
       x = "Variable",
       y = "Coefficient Estimate (with 95% CI)",
       fill = "Gender Group") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Calculate share of nonroutine workers for males and females
Gender_data <- Gender_data %>%
  mutate(
    share_nonroutine_male = weighted_nonroutine_m / (weighted_nonroutine_m + weighted_routine_m),
    share_nonroutine_female = weighted_nonroutine_f / (weighted_nonroutine_f + weighted_routine_f)
  )

# Step 2: Aggregate average shares by year
trend_data <- Gender_data %>%
  group_by(year) %>%
  summarise(
    mean_share_male = mean(share_nonroutine_male, na.rm = TRUE),
    mean_share_female = mean(share_nonroutine_female, na.rm = TRUE)
  )

# Step 3: Reshape data to long format for ggplot
trend_data_long <- trend_data %>%
  pivot_longer(
    cols = c(mean_share_male, mean_share_female),
    names_to = "Gender",
    values_to = "Mean_Share"
  ) %>%
  mutate(
    Gender = case_when(
      Gender == "mean_share_male" ~ "Male",
      Gender == "mean_share_female" ~ "Female"
    )
  )

# Step 4: Plot
ggplot(trend_data_long, aes(x = year, y = Mean_Share, color = Gender)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  labs(title = "Trend of Nonroutine Employment Share (2020–2024)",
       x = "Year",
       y = "Average Share of Nonroutine Workers",
       color = "Gender") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("steelblue", "firebrick"))



# Load packages
library(readxl)
library(dplyr)
library(fixest)
library(data.table)

# Load the dataset
data <- read_excel("C:/Users/Lohit/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Drop 2024 data
data <- data %>%
  filter(year != 2024)

# Define tech-intensity bins
data <- data %>%
  mutate(
    industry_tech_bin = case_when(
      industry_code %in% c(10,11,12,13) ~ "High-Tech",
      industry_code %in% c(3,4,5,15,16,17) ~ "Medium-Tech",
      industry_code %in% c(1,2,6,7,8,9) ~ "Low-Tech",
      TRUE ~ "Other"
    ),
    industry_tech_bin = factor(industry_tech_bin, levels = c("Low-Tech", "Medium-Tech", "High-Tech"))
  )

# Generate log outcome variables and key regressors
data <- data %>%
  mutate(
    log_ratio_nonroutine = log(weighted_nonroutine / weighted_routine),
    log_ratio_cognitive = log(weighted_cognitive / weighted_manual),
    interaction2 = dci * share_female
  )

# Prepare for lagged DCI
data <- as.data.table(data)
setorder(data, Province_code, year)
data[, lagged_dci := shift(dci, n = 1, type = "lag"), by = .(Province_code)]

# Run regressions (without 2024)
model_nonroutine <- feols(
  log_ratio_nonroutine ~ dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

model_cognitive <- feols(
  log_ratio_cognitive ~ dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

# Display regression table
etable(model_nonroutine, model_cognitive)



# Subsample Low-Tech
data_lowtech <- data[industry_tech_bin == "Low-Tech"]

# Subsample Medium-Tech
data_medtech <- data[industry_tech_bin == "Medium-Tech"]

# Subsample High-Tech
data_hightech <- data[industry_tech_bin == "High-Tech"]

# Low-Tech regression
model_lowtech <- feols(
  log_ratio_nonroutine ~ lagged_dci + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data_lowtech
)

# Medium-Tech regression
model_medtech <- feols(
  log_ratio_nonroutine ~ lagged_dci + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data_medtech
)

# High-Tech regression
model_hightech <- feols(
  log_ratio_nonroutine ~ lagged_dci + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data_hightech
)

etable(model_lowtech, model_medtech, model_hightech,
       headers = c("Low-Tech", "Medium-Tech", "High-Tech"))


# Robustness Checks

model_share_nonroutine <- feols(
  share_nonroutine_to_routine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)
etable(model_share_nonroutine)


data[, urban_high := as.integer(share_urban > median(share_urban, na.rm = TRUE))]

model_urban_interaction <- feols(
  log_ratio_nonroutine ~ lagged_dci * urban_high + industry_tech_bin + share_female + interaction2 +
    share_college + GDP + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)
etable(model_urban_interaction)


model_excl_jakarta <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data[Province_code != 31]
)
etable(model_excl_jakarta)


data[, asin_share_nonroutine := asin(sqrt(share_nonroutine_to_routine))]

model_asin <- feols(
  asin_share_nonroutine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)
etable(model_asin)



library(ggplot2)

data_plot <- data %>%
  group_by(year, industry_tech_bin) %>%
  summarise(mean_share = mean(share_nonroutine_to_routine, na.rm = TRUE))

ggplot(data_plot, aes(x = year, y = mean_share, color = industry_tech_bin)) +
  geom_line(size = 1.2) +
  labs(title = "Trend in Nonroutine Job Share by Industry Tech Bin",
       y = "Average Nonroutine Share", x = "Year", color = "Tech Bin") +
  theme_minimal()



ggplot(data, aes(x = dci, y = share_nonroutine_to_routine)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  labs(title = "Digital Competitiveness vs Nonroutine Job Share by Year")


# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)


# Assuming your dataset is called Gender_data
# and you have these variables: industry_tech_bin, weighted_nonroutine_m, weighted_routine_m, weighted_nonroutine_f, weighted_routine_f

# Step 1: Calculate nonroutine share for males and females
Gender_data <- Gender_data %>%
  mutate(
    nonroutine_share_male = weighted_nonroutine_m / (weighted_nonroutine_m + weighted_routine_m),
    nonroutine_share_female = weighted_nonroutine_f / (weighted_nonroutine_f + weighted_routine_f)
  )

# Step 2: Aggregate (take mean) by industry and gender
industry_gender_summary <- Gender_data %>%
  group_by(industry_tech_bin) %>%
  summarise(
    mean_nonroutine_male = mean(nonroutine_share_male, na.rm = TRUE),
    mean_nonroutine_female = mean(nonroutine_share_female, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(mean_nonroutine_male, mean_nonroutine_female),
    names_to = "Gender",
    values_to = "Mean_Nonroutine_Share"
  ) %>%
  mutate(
    Gender = case_when(
      Gender == "mean_nonroutine_male" ~ "Male",
      Gender == "mean_nonroutine_female" ~ "Female"
    )
  )

# Step 3: Plot
ggplot(industry_gender_summary, aes(x = industry_tech_bin, y = Mean_Nonroutine_Share, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Nonroutine Job Share by Industry and Gender",
       x = "Industry Technology Bin",
       y = "Mean Nonroutine Share") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "pink"))


# Load libraries
library(fixest)
library(dplyr)

# Step 1: Create nonroutine shares if not already done
Gender_data <- Gender_data %>%
  mutate(
    nonroutine_share_male = weighted_nonroutine_m / (weighted_nonroutine_m + weighted_routine_m),
    nonroutine_share_female = weighted_nonroutine_f / (weighted_nonroutine_f + weighted_routine_f)
  )

# Step 2: Loop through combinations
industry_levels <- c("Low-Tech", "Medium-Tech", "High-Tech")
gender_levels <- c("Male", "Female")

# Step 3: Run regressions
models_list <- list()

for (industry in industry_levels) {
  for (gender in gender_levels) {
    
    # Subset data
    subset_data <- Gender_data %>%
      filter(industry_tech_bin == industry)
    
    if (gender == "Male") {
      y_var <- "log_ratio_nonroutine_male"
    } else {
      y_var <- "log_ratio_nonroutine_female"
    }
    
    # Run regression
    model <- feols(
      as.formula(paste0(y_var, " ~ lagged_dci + share_college + GDP + share_urban + Gini | province_code + industry_code^year")),
      cluster = ~province_code,
      data = subset_data
    )
    
    # Save model
    models_list[[paste(industry, gender, sep = "_")]] <- model
  }
}

# Step 4: Summarize the results
etable(models_list, headers = names(models_list))


# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Calculate nonroutine share for males and females if not done yet
Gender_data <- Gender_data %>%
  mutate(
    nonroutine_share_male = weighted_nonroutine_m / (weighted_nonroutine_m + weighted_routine_m),
    nonroutine_share_female = weighted_nonroutine_f / (weighted_nonroutine_f + weighted_routine_f)
  )

# Step 2: Create a long format data for easier summary
Gender_long <- Gender_data %>%
  pivot_longer(
    cols = c(nonroutine_share_male, nonroutine_share_female),
    names_to = "Gender",
    values_to = "Nonroutine_Share"
  ) %>%
  mutate(
    Gender = case_when(
      Gender == "nonroutine_share_male" ~ "Male",
      Gender == "nonroutine_share_female" ~ "Female"
    )
  )

# Step 3: Summarize Descriptive Statistics
descriptive_stats <- Gender_long %>%
  group_by(industry_tech_bin, Gender) %>%
  summarise(
    Mean_Nonroutine_Share = mean(Nonroutine_Share, na.rm = TRUE),
    SD_Nonroutine_Share = sd(Nonroutine_Share, na.rm = TRUE),
    Observations = n()
  ) %>%
  arrange(industry_tech_bin, Gender)

# Step 4: View the descriptive table
print(descriptive_stats)



# Diagnostic Tests

# Load libraries
library(readxl)
library(dplyr)
library(fixest)  # for fixed effects regression

# Read dataset
data <- read_excel("C:/Users/Babek Ahmedli/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Create required variables
data <- data %>%
  mutate(
    share_nonroutine = weighted_nonroutine / (weighted_nonroutine + weighted_routine),
    share_cognitive = weighted_cognitive / (weighted_cognitive + weighted_manual),
    log_ratio_nonroutine = log(weighted_nonroutine / weighted_routine),
    log_ratio_cognitive = log(weighted_cognitive / weighted_manual),
    tech_intensity = share_computer,  # replace with the actual column if different
    #male_female_ratio = (weighted_male / weighted_female),
    interaction1 = dci * tech_intensity,
    interaction2 = dci * share_female
  )

# Run the regression model
model_nonroutine <- feols(
  log_ratio_nonroutine ~ dci + tech_intensity + interaction1 + share_female + interaction2 +
    share_college + GDP + share_urban + Gini | Province_code + industry_code + year,
  data = data, cluster = ~Province_code
)

model_cognitive <- feols(
  log_ratio_cognitive ~ dci + tech_intensity + interaction1 + share_female + interaction2 +
    share_college + GDP + share_urban + Gini | Province_code + industry_code + year,
  data = data, cluster = ~Province_code
)

# Summary of results
etable(model_nonroutine, model_cognitive)





# Load packages
library(readxl)
library(dplyr)
library(fixest)

# Load the dataset
data <- read_excel("C:/Users/Babek Ahmedli/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Define tech-intensity bins
data <- data %>%
  mutate(
    industry_tech_bin = case_when(
      industry_code %in% c(10,11,12,13) ~ "High-Tech",
      industry_code %in% c(3,4,5,15,16,17) ~ "Medium-Tech",
      industry_code %in% c(1,2,6,7,8,9) ~ "Low-Tech",
      TRUE ~ "Other"
    ),
    industry_tech_bin = factor(industry_tech_bin, levels = c("Low-Tech", "Medium-Tech", "High-Tech"))
  )

# Generate log outcome variables and key regressors
data <- data %>%
  mutate(
    log_ratio_nonroutine = log(weighted_nonroutine / weighted_routine),
    log_ratio_cognitive = log(weighted_cognitive / weighted_manual),
    interaction2 = dci * share_female
  )

# Optional: lag dci by province-year if needed
library(data.table)
data <- data.table(data)
setorder(data, Province_code, year)
data[, lagged_dci := shift(dci, n = 1, type = "lag"), by = .(Province_code)]

# Run improved models with binned industry effect and more fixed effects
model_nonroutine <- feols(
  log_ratio_nonroutine ~ dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

model_cognitive <- feols(
  log_ratio_cognitive ~ dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

# Display regression table
etable(model_nonroutine, model_cognitive)

# Remove rows where lagged_dci is NA (first year for each province)
data <- data[!is.na(lagged_dci)]

# Run regressions using lagged DCI
model_nonroutine_lag <- feols(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

model_cognitive_lag <- feols(
  log_ratio_cognitive ~ lagged_dci * industry_tech_bin + share_female + interaction2 +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  cluster = ~Province_code,
  data = data
)

# Display regression results
etable(model_nonroutine_lag, model_cognitive_lag)

# Load plotting library
library(ggplot2)

# Extract coefficients
coefs <- coef(model_nonroutine_lag)

# Compute marginal effects for each bin
marginal_effects <- data.frame(
  Industry = c("Low-Tech", "Medium-Tech", "High-Tech"),
  Effect = c(
    coefs["lagged_dci"],
    coefs["lagged_dci"] + coefs["lagged_dci:industry_tech_binMedium-Tech"],
    coefs["lagged_dci"] + coefs["lagged_dci:industry_tech_binHigh-Tech"]
  ),
  SE = c(
    se(model_nonroutine_lag)["lagged_dci"],
    sqrt(se(model_nonroutine_lag)["lagged_dci"]^2 + se(model_nonroutine_lag)["lagged_dci:industry_tech_binMedium-Tech"]^2),
    sqrt(se(model_nonroutine_lag)["lagged_dci"]^2 + se(model_nonroutine_lag)["lagged_dci:industry_tech_binHigh-Tech"]^2)
  )
)

# Plot
ggplot(marginal_effects, aes(x = Industry, y = Effect)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Effect - 1.96*SE, ymax = Effect + 1.96*SE), width = 0.2) +
  labs(title = "Marginal Effect of Lagged DCI on Skilled Labor Share by Industry Tech Intensity",
       y = "Effect of lagged_dci", x = "Industry Technology Bin") +
  theme_minimal()


# Robustness Checks
library(ggplot2)
library(dplyr)


data_plot <- data %>%
  group_by(year, industry_tech_bin) %>%
  summarise(mean_share = mean(share_nonroutine, na.rm = TRUE))

ggplot(data_plot, aes(x = year, y = mean_share, color = industry_tech_bin)) +
  geom_line(size = 1.2) +
  labs(title = "Trend in Nonroutine Job Share by Industry Tech Bin",
       y = "Average Nonroutine Share", x = "Year", color = "Tech Bin") +
  theme_minimal()


ggplot(data, aes(x = dci, y = share_nonroutine)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  labs(title = "Digital Competitiveness vs Nonroutine Job Share by Year")




# Load required libraries
library(readxl)
library(dplyr)
library(lmtest)

# Load your dataset
data <- read_excel("C:/Users/Babek Ahmedli/Downloads/Merged_Final_Indonesia_Panel data.xlsx", sheet = "Sheet 1")

# Prepare necessary variables safely
data <- data %>%
  arrange(Province_code, year) %>%
  group_by(Province_code) %>%
  mutate(
    lagged_dci = lag(dci, 1),
    valid_nonroutine = weighted_nonroutine > 0 & weighted_routine > 0,
    log_ratio_nonroutine = ifelse(valid_nonroutine, log(weighted_nonroutine / weighted_routine), NA_real_),
    interaction2 = dci * share_female,
    industry_tech_bin = case_when(
      industry_code %in% c(10,11,12,13) ~ "High-Tech",
      industry_code %in% c(3,4,5,15,16,17) ~ "Medium-Tech",
      industry_code %in% c(1,2,6,7,8,9) ~ "Low-Tech",
      TRUE ~ "Other"
    ),
    industry_tech_bin = factor(industry_tech_bin, levels = c("Low-Tech", "Medium-Tech", "High-Tech"))
  ) %>%
  ungroup()

# Filter rows with complete data for pooled OLS
data_reset <- data %>%
  filter(
    !is.na(log_ratio_nonroutine),
    !is.na(lagged_dci),
    !is.na(interaction2),
    !is.na(share_female),
    !is.na(share_college),
    !is.na(GDP),
    !is.na(share_urban),
    !is.na(Gini)
  )

# Run pooled OLS model
reset_model <- lm(
  log_ratio_nonroutine ~ lagged_dci * industry_tech_bin +
    share_female + interaction2 + share_college + GDP + share_urban + Gini,
  data = data_reset
)

# Run Ramsey RESET test (2nd and 3rd power of fitted values)
reset_result <- resettest(reset_model, power = 2:3, type = "fitted")

# View results
print(reset_result)

data <- data %>%
  mutate(
    lagged_dci_sq = lagged_dci^2,
    interaction2_sq = interaction2^2
  )
model_fix_corrected <- feols(
  log_ratio_nonroutine ~ lagged_dci + lagged_dci_sq +
    interaction2 + interaction2_sq + share_female +
    share_college + GDP + share_urban + Gini |
    Province_code + industry_code^year,
  data = data,
  cluster = ~Province_code
)

summary(model_fix_corrected)













