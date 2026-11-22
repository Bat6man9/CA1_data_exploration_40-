# BANK MARKETING - DATA EXPLORATION AND PREPARATION IN R
# ==========================================================
# Youssef Sabry 2023318 | CCT College Dublin
# Topic: customer characteristics, campaign history and term-deposit subscription
# https://doi.org/10.24432/C5K306 (accessed 26 October 2026)

# STEP 1: LOAD THE BANK MARKETING DATASET

# bank-full.csv in the working directory. This file uses semicolons.
bank <- read.csv("bank-full.csv", sep = ";", stringsAsFactors = FALSE)

# Load the packages
library(dplyr)                                  # Count records within categories
library(ggplot2)                                # Plot distributions and relationships

# STEP 2: INSPECT THE ORIGINAL DATA

# Display summaries before changing any values.
head(bank)                                      # Show the first six records
dim(bank)                                       # Check the number of rows and columns
str(bank)                                       # Examine the type of each variable
summary(bank)                                   # Display numerical and categorical summaries
colSums(is.na(bank))                             # Count literal missing values in each column
sum(duplicated(bank))                            # Count exact duplicate rows: zero in this source
# No duplicate rows need removal. This does not prove that every row is a unique customer.
# Check literal missing cells separately from coded unknown levels.
sum(is.na(bank))
count(bank, y)

# Creating a subset of data
bank_tiny <- bank[1:5, c(1, 2, 6, 12, 14, 15, 16, 17)]
bank_tiny

# Check categorical values and missing-information codes
our_table <- table(bank$job)
our_table
our_table <- table(bank$education)
our_table
our_table <- table(bank$contact)
our_table
our_table <- table(bank$poutcome)
our_table
our_table <- table(bank$pdays == -1)
our_table

# STEP 3: PREPARE THE DATA

# Prepare a separate copy so the original data can be checked later.
# The dictionary states that pdays = -1 means no previous contact.
bank_clean <- bank
bank_clean$previously_contacted <- ifelse(bank_clean$pdays == -1, "no", "yes")
bank_clean$pdays_since_contact <- ifelse(bank_clean$pdays == -1, NA, bank_clean$pdays)

# Separate absence of prior contact from the result of a previous campaign
# An unknown result after a real prior contact is different from no prior contact.
bank_clean$history_group <- ifelse(bank_clean$pdays == -1, "No prior contact",
                                   bank_clean$poutcome)
count(bank_clean, history_group, y)

# Retain indicators of missing information for audit and later exploration
# A missing prior result after real contact differs from no prior contact.
bank_clean$job_unknown_flag <- ifelse(bank_clean$job == "unknown", 1, 0)
bank_clean$contact_unknown_flag <- ifelse(bank_clean$contact == "unknown", 1, 0)
bank_clean$prior_result_unknown_flag <- ifelse(bank_clean$pdays != -1 &
                                                 bank_clean$poutcome == "unknown", 1, 0)
count(bank_clean, job_unknown_flag)
count(bank_clean, contact_unknown_flag)
count(bank_clean, previously_contacted, prior_result_unknown_flag)
sum(is.na(bank_clean$pdays_since_contact))
# These audit flags do not impute unknown values or change the original columns.
# They are not added to the numerical PCA input set.

# Check the relationship between missing campaign outcomes and previous contact
# Count combinations instead of assuming that every unknown outcome is an error.
count(bank_clean, previously_contacted, poutcome)
count(bank_clean, previously_contacted, previous)

# Analyse the missing-information codes actually present in this source.
# Keep unknown categorical levels explicitly; no evidence supports imputing a job.
# Do not remove legitimate negative balances or large values just for being unusual.
# There is no customer identifier, so matching records alone cannot prove duplication.

#=============================================

# STEP 4: EXPLORE DISTRIBUTIONS AND RELATIONSHIPS

# Explore the numerical distributions.
hist(bank_clean$age, breaks = 30, col = "blue", border = "black",
     xlab = "Age (years)", ylab = "Counts", main = "Distribution of customer age")
hist(bank_clean$balance, breaks = 60, col = "blue", border = "black",
     xlab = "Average yearly balance (EUR)", ylab = "Counts",
     main = "Distribution of average yearly balance")

# Question 1: how does account balance vary with subscription outcome?
balance_plot <- ggplot(bank_clean, aes(x = y, y = balance)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Balance by subscription: full range",
       x = "Subscribed to term deposit", y = "Average yearly balance (EUR)") +
  theme_minimal()
print(balance_plot)

# Also inspect the central range without removing any original observations.
# The preceding full-range plot remains essential for seeing the extreme values.
balance_zoom <- ggplot(bank_clean, aes(x = y, y = balance)) +
  geom_boxplot(fill = "steelblue") +
  coord_cartesian(ylim = c(-1000, 5000)) +
  labs(title = "Balance by subscription: central view",
       x = "Subscribed to term deposit", y = "Average yearly balance (EUR)") +
  theme_minimal()
print(balance_zoom)

# Question 2: how do age and account balance vary together?
age_balance_plot <- ggplot(bank_clean, aes(x = age, y = balance)) +
  geom_point(colour = "steelblue", alpha = 0.1) +
  labs(title = "Age and account balance",
       x = "Age (years)", y = "Average yearly balance (EUR)") +
  theme_minimal()
print(age_balance_plot)

# Question 3: which combinations of previous contact and outcome occur?
count(bank_clean, previously_contacted, y)
count(bank_clean, job, y)

# Compare rates within groups, retaining each group's full denominator.
# sum(y == "yes") counts subscriptions; nrow() counts all records in the group.
# Divide the subscription count by that denominator, then multiply by 100.
not_contacted <- bank_clean[bank_clean$previously_contacted == "no", ]
contacted <- bank_clean[bank_clean$previously_contacted == "yes", ]
table(not_contacted$y)
table(contacted$y)
100 * sum(not_contacted$y == "yes") / nrow(not_contacted)
100 * sum(contacted$y == "yes") / nrow(contacted)

# These are observational associations, not causal effects or predictions.
# Call duration is only known after the call; it is not a pre-call predictor.
# Class imbalance is retained for EDA: synthetic data are prohibited by the brief.

# Investigate unknown outcomes among previously contacted records
unknown_previous <- contacted[contacted$poutcome == "unknown", ]
unknown_previous
# Keep these five as unknown; no evidence supports guessing their outcome.

balance_q1 <- quantile(bank_clean$balance, probs = .25)
balance_q3 <- quantile(bank_clean$balance, probs = .75)
balance_iqr <- balance_q3 - balance_q1
balance_lower <- balance_q1 - 1.5 * balance_iqr
balance_upper <- balance_q3 + 1.5 * balance_iqr
balance_q1
balance_q3
balance_lower
balance_upper
bank_clean$balance_iqr_flag <- ifelse(bank_clean$balance < balance_lower |
                                        bank_clean$balance > balance_upper, 1, 0)
table(bank_clean$balance_iqr_flag)
table(bank_clean$balance < 0)
# A flagged value is unusual, not automatically wrong. Retain it in bank_clean.

# Compare distributions by outcome, rather than relying on the pooled mean
no_subscription <- bank_clean[bank_clean$y == "no", ]
yes_subscription <- bank_clean[bank_clean$y == "yes", ]
summary(no_subscription$balance)
summary(yes_subscription$balance)

# Sensitivity: does the largest previous-contact count dominate the mean?
max(bank_clean$previous)
without_max_previous <- bank_clean[bank_clean$previous < max(bank_clean$previous), ]
mean(bank_clean$previous)
mean(without_max_previous$previous)
100 * sum(bank_clean$y == "yes") / nrow(bank_clean)
100 * sum(without_max_previous$y == "yes") / nrow(without_max_previous)
# This subset is diagnostic only; it does not replace the complete analysis.

# Explore contact-history association within a campaign-count stratum
# Compare records with one contact in the current campaign.
# This does not control all potential confounders or demonstrate causation.
once_not_contacted <- not_contacted[not_contacted$campaign == 1, ]
once_contacted <- contacted[contacted$campaign == 1, ]
table(once_not_contacted$y)
table(once_contacted$y)
100 * sum(once_not_contacted$y == "yes") / nrow(once_not_contacted)
100 * sum(once_contacted$y == "yes") / nrow(once_contacted)

# Compare subscription percentages using the denominator within each group
history_none <- bank_clean[bank_clean$history_group == "No prior contact", ]
history_failure <- bank_clean[bank_clean$history_group == "failure", ]
history_other <- bank_clean[bank_clean$history_group == "other", ]
history_success <- bank_clean[bank_clean$history_group == "success", ]
history_unknown <- bank_clean[bank_clean$history_group == "unknown", ]
rate_none <- 100 * sum(history_none$y == "yes") / nrow(history_none)
rate_failure <- 100 * sum(history_failure$y == "yes") / nrow(history_failure)
rate_other <- 100 * sum(history_other$y == "yes") / nrow(history_other)
rate_success <- 100 * sum(history_success$y == "yes") / nrow(history_success)
rate_unknown <- 100 * sum(history_unknown$y == "yes") / nrow(history_unknown)

history_summary <- count(bank_clean, history_group)
history_summary$subscription_percent <- 0
history_summary[history_summary$history_group == "No prior contact", "subscription_percent"] <- rate_none
history_summary[history_summary$history_group == "failure", "subscription_percent"] <- rate_failure
history_summary[history_summary$history_group == "other", "subscription_percent"] <- rate_other
history_summary[history_summary$history_group == "success", "subscription_percent"] <- rate_success
history_summary[history_summary$history_group == "unknown", "subscription_percent"] <- rate_unknown
history_summary
# The unknown-prior-result group has only five records: do not overinterpret its rate.
# Show the four substantial groups; the five unknown records stay in the table.
history_plot_data <- history_summary[history_summary$history_group != "unknown", ]
history_rate_plot <- ggplot(history_plot_data,
                            aes(x = history_group, y = subscription_percent)) +
  geom_point(size = 3, colour = "steelblue") +
  coord_cartesian(ylim = c(0, 70)) +
  labs(title = "Subscription by campaign history",
       x = "Previous campaign outcome", y = "Subscription (%)",
       subtitle = "Five unknown prior outcomes omitted from this figure only") +
  theme_minimal()
print(history_rate_plot)

# Recheck the pooled association after setting prior-success records aside
contacted_not_success <- contacted[contacted$poutcome != "success", ]
table(contacted_not_success$y)
100 * sum(contacted_not_success$y == "yes") / nrow(contacted_not_success)
# This is a descriptive subgroup comparison, not a corrected causal estimate.

# Check unknown current-contact information against calendar month
# Associations with collection conditions can make blanket imputation misleading.
count(bank_clean, month, contact)
known_contact <- bank_clean[bank_clean$contact != "unknown", ]
unknown_contact <- bank_clean[bank_clean$contact == "unknown", ]
table(known_contact$y)
table(unknown_contact$y)
100 * sum(known_contact$y == "yes") / nrow(known_contact)
100 * sum(unknown_contact$y == "yes") / nrow(unknown_contact)
# Counts show how missingness varies; they do not establish its mechanism as MAR/MNAR.

# Assess outcome rates with and without statistically unusual balances
# Keep the full data as the main analysis; this is only a sensitivity comparison.
balance_central <- bank_clean[bank_clean$balance_iqr_flag == 0, ]
balance_flagged <- bank_clean[bank_clean$balance_iqr_flag == 1, ]
count(bank_clean, balance_iqr_flag, y)
100 * sum(balance_central$y == "yes") / nrow(balance_central)
100 * sum(balance_flagged$y == "yes") / nrow(balance_flagged)
# Automatically removing outliers would change the composition of the sample.

# Visualise categorical relationships using counts and proportions
# The count view shows sample sizes; the separate rate plot permits comparisons.
history_counts <- count(bank_clean, history_group, y)
history_counts
# Use explicit outcome labels in a copy; original y values are preserved.
visual_data <- bank_clean
visual_data$subscription <- ifelse(visual_data$y == "yes", "Subscribed", "Not subscribed")
history_count_plot <- ggplot(visual_data,
                             aes(x = history_group, y = subscription)) +
  geom_count(colour = "steelblue") +
  labs(title = "Campaign history and subscription: record counts",
       x = "Previous campaign outcome", y = "Subscription outcome", size = "Records") +
  theme_minimal()
print(history_count_plot)

#====================================================

# STEP 5: COMPARE SCALING METHODS

# Compare scaling methods before choosing numerical inputs.
# Keep these diagnostic copies separate from the baseline and original columns.
normalizeMM <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
scaling_compare <- bank_clean
scaling_compare$balance_minmax <- normalizeMM(scaling_compare$balance)
scaling_compare$balance_robust <- (scaling_compare$balance -
                                     median(scaling_compare$balance)) /
  (quantile(scaling_compare$balance, probs = .75) -
     quantile(scaling_compare$balance, probs = .25))
summary(scaling_compare$balance_minmax)
summary(scaling_compare$balance_robust)
quantile(scaling_compare$balance_minmax, probs = c(.25, .50, .75))
quantile(scaling_compare$balance_robust, probs = c(.25, .50, .75))
# Min-max accepts negative balances; it subtracts the observed minimum.
# Robust scaling changes the centre and scale but does not delete the long tail.

# Check the denominator before attempting robust scaling of previous contacts
previous_iqr <- quantile(bank_clean$previous, probs = .75) -
  quantile(bank_clean$previous, probs = .25)
previous_iqr
# The denominator is zero here. Do not divide previous by its IQR.
# Keep standardisation as the baseline; no PCA decomposition is performed here.

# Sensitivity to the single largest previous-contact count
# Refit scaling on a diagnostic subset only; retain all rows in the main analysis.
previous_minmax <- normalizeMM(bank_clean$previous)
previous_minmax_without_max <- normalizeMM(without_max_previous$previous)
previous_scaled_without_max <- scale(without_max_previous$previous)
summary(previous_minmax)
summary(previous_minmax_without_max)
summary(previous_scaled_without_max)
max(without_max_previous$previous)
# Compare the mapped value for one previous contact under each fitted range.
previous_minmax[bank_clean$previous == 1][1]
previous_minmax_without_max[without_max_previous$previous == 1][1]
# Linear rescaling alone does not remove skewness. If inputs are standardised
# again before PCA, positive linear rescalings give the same standardised inputs.
# Removing a row changes the fitted data; that is a different sensitivity question.



# STEP 6: INVESTIGATE CAMPAIGN AND EDUCATION PATTERNS

# Examine the contact-information pattern within its two dominant months.
# This describes recorded months, not years or a causal adjustment.
may_known <- known_contact[known_contact$month == "may", ]
may_unknown <- unknown_contact[unknown_contact$month == "may", ]
june_known <- known_contact[known_contact$month == "jun", ]
june_unknown <- unknown_contact[unknown_contact$month == "jun", ]
table(may_known$y)
table(may_unknown$y)
table(june_known$y)
table(june_unknown$y)
rate_may_known <- 100 * sum(may_known$y == "yes") / nrow(may_known)
rate_may_unknown <- 100 * sum(may_unknown$y == "yes") / nrow(may_unknown)
rate_june_known <- 100 * sum(june_known$y == "yes") / nrow(june_known)
rate_june_unknown <- 100 * sum(june_unknown$y == "yes") / nrow(june_unknown)
rate_may_known
rate_may_unknown
rate_june_known
rate_june_unknown

month_contact <- bank_clean[bank_clean$month == "may" | bank_clean$month == "jun", ]
month_contact$contact_group <- ifelse(month_contact$month == "may",
                                      ifelse(month_contact$contact == "unknown", "May\nunknown", "May\nknown"),
                                      ifelse(month_contact$contact == "unknown", "June\nunknown", "June\nknown"))
month_rates <- count(month_contact, contact_group)
month_rates$subscription_percent <- 0
month_rates[month_rates$contact_group == "May\nknown", "subscription_percent"] <- rate_may_known
month_rates[month_rates$contact_group == "May\nunknown", "subscription_percent"] <- rate_may_unknown
month_rates[month_rates$contact_group == "June\nknown", "subscription_percent"] <- rate_june_known
month_rates[month_rates$contact_group == "June\nunknown", "subscription_percent"] <- rate_june_unknown
month_rates
month_rate_plot <- ggplot(month_rates, aes(x = contact_group, y = subscription_percent)) +
  geom_point(size = 3, colour = "steelblue") +
  coord_cartesian(ylim = c(0, 50)) +
  labs(title = "Contact information within May and June",
       x = "Recorded month and contact information", y = "Subscription (%)") +
  theme_minimal()
print(month_rate_plot)
# Stratifying by month alone cannot establish why information is missing or
# whether contact method causes a subscription. Do not infer a particular year.

# Inspect the full ranges of the numerical inputs together
# Equal panels compare distribution shapes, not units or variance magnitudes.
# Keep the long tails visible before considering PCA sensitivity.
par(mfrow = c(2, 2))
hist(bank_clean$age, breaks = 30, col = "blue", border = "black",
     xlab = "Age (years)", ylab = "Counts", main = "Age: full range")
hist(bank_clean$duration, breaks = 60, col = "blue", border = "black",
     xlab = "Call duration (seconds)", ylab = "Counts",
     main = "Duration: full range")
hist(bank_clean$campaign, breaks = 60, col = "blue", border = "black",
     xlab = "Current campaign contacts", ylab = "Counts",
     main = "Campaign: full range")
hist(bank_clean$previous, breaks = 60, col = "blue", border = "black",
     xlab = "Prior campaign contacts", ylab = "Counts",
     main = "Previous: full range")
par(mfrow = c(1, 1))
summary(bank_clean[, c("age", "balance", "duration", "campaign", "previous")])
sum(bank_clean$duration == 0)
sum(bank_clean$campaign == 1)
sum(bank_clean$previous == 0)
# Zero duration does not establish an invalid record. Investigate before deletion.
# Duration is retrospective. More contacts cannot be interpreted as causing a
# different outcome from these observational distributions.

# Question 4: how does subscription vary across recorded education categories?
# Keep unknown separate. Rates use the full denominator in each category.
education_primary <- bank_clean[bank_clean$education == "primary", ]
education_secondary <- bank_clean[bank_clean$education == "secondary", ]
education_tertiary <- bank_clean[bank_clean$education == "tertiary", ]
education_unknown <- bank_clean[bank_clean$education == "unknown", ]
count(bank_clean, education, y)
rate_primary <- 100 * sum(education_primary$y == "yes") / nrow(education_primary)
rate_secondary <- 100 * sum(education_secondary$y == "yes") / nrow(education_secondary)
rate_tertiary <- 100 * sum(education_tertiary$y == "yes") / nrow(education_tertiary)
rate_education_unknown <- 100 * sum(education_unknown$y == "yes") / nrow(education_unknown)
education_summary <- count(bank_clean, education)
education_summary$subscription_percent <- 0
education_summary[education_summary$education == "primary", "subscription_percent"] <- rate_primary
education_summary[education_summary$education == "secondary", "subscription_percent"] <- rate_secondary
education_summary[education_summary$education == "tertiary", "subscription_percent"] <- rate_tertiary
education_summary[education_summary$education == "unknown", "subscription_percent"] <- rate_education_unknown
education_summary
education_rate_plot <- ggplot(education_summary,
                              aes(x = education, y = subscription_percent)) +
  geom_point(size = 3, colour = "steelblue") +
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "Subscription by recorded education",
       x = "Recorded education category", y = "Subscription (%)") +
  theme_minimal()
print(education_rate_plot)
# These are record-level associations, not the effect of education.
# Unknown is missing information, not an intermediate education qualification.

# Check the primary-tertiary comparison without any prior campaign contact
# This subset removes prior-success records but does not control other differences.
primary_no_prior <- education_primary[education_primary$pdays == -1, ]
tertiary_no_prior <- education_tertiary[education_tertiary$pdays == -1, ]
table(primary_no_prior$y)
table(tertiary_no_prior$y)
rate_primary_no_prior <- 100 * sum(primary_no_prior$y == "yes") / nrow(primary_no_prior)
rate_tertiary_no_prior <- 100 * sum(tertiary_no_prior$y == "yes") / nrow(tertiary_no_prior)
rate_tertiary - rate_primary
rate_tertiary_no_prior - rate_primary_no_prior
# The contrast can persist within a broad stratum while still being confounded.
# Secondary remains only the coding reference; it is not a statistical control.

# Does the education contrast persist within age bands when there is no prior contact?
# Compare only the two education groups in the earlier contrast. Use broad bands
# to keep group sizes readable, and retain the other records in bank_clean.
age_comparison <- bank_clean[bank_clean$pdays == -1 &
                               (bank_clean$education == "primary" |
                                  bank_clean$education == "tertiary"), ]
age_comparison$age_band <- ifelse(age_comparison$age < 30, "18-29",
                                  ifelse(age_comparison$age < 45, "30-44",
                                         ifelse(age_comparison$age < 60, "45-59", "60+")))
# Count all records in each age and education group first.
# Keep a separate subscription count so the numerator and denominator are visible.
age_rates <- count(age_comparison, age_band, education)
age_rates$subscribed <- 0

primary_18_29 <- age_comparison[age_comparison$age_band == "18-29" &
                                  age_comparison$education == "primary", ]
age_rates[age_rates$age_band == "18-29" & age_rates$education == "primary",
          "subscribed"] <- sum(primary_18_29$y == "yes")

primary_30_44 <- age_comparison[age_comparison$age_band == "30-44" &
                                  age_comparison$education == "primary", ]
age_rates[age_rates$age_band == "30-44" & age_rates$education == "primary",
          "subscribed"] <- sum(primary_30_44$y == "yes")

primary_45_59 <- age_comparison[age_comparison$age_band == "45-59" &
                                  age_comparison$education == "primary", ]
age_rates[age_rates$age_band == "45-59" & age_rates$education == "primary",
          "subscribed"] <- sum(primary_45_59$y == "yes")

primary_60_plus <- age_comparison[age_comparison$age_band == "60+" &
                                    age_comparison$education == "primary", ]
age_rates[age_rates$age_band == "60+" & age_rates$education == "primary",
          "subscribed"] <- sum(primary_60_plus$y == "yes")

tertiary_18_29 <- age_comparison[age_comparison$age_band == "18-29" &
                                   age_comparison$education == "tertiary", ]
age_rates[age_rates$age_band == "18-29" & age_rates$education == "tertiary",
          "subscribed"] <- sum(tertiary_18_29$y == "yes")

tertiary_30_44 <- age_comparison[age_comparison$age_band == "30-44" &
                                   age_comparison$education == "tertiary", ]
age_rates[age_rates$age_band == "30-44" & age_rates$education == "tertiary",
          "subscribed"] <- sum(tertiary_30_44$y == "yes")

tertiary_45_59 <- age_comparison[age_comparison$age_band == "45-59" &
                                   age_comparison$education == "tertiary", ]
age_rates[age_rates$age_band == "45-59" & age_rates$education == "tertiary",
          "subscribed"] <- sum(tertiary_45_59$y == "yes")

tertiary_60_plus <- age_comparison[age_comparison$age_band == "60+" &
                                     age_comparison$education == "tertiary", ]
age_rates[age_rates$age_band == "60+" & age_rates$education == "tertiary",
          "subscribed"] <- sum(tertiary_60_plus$y == "yes")

age_rates$subscription_percent <- 100 * age_rates$subscribed / age_rates$n
age_rates

primary_age_rates <- age_rates$subscription_percent[age_rates$education == "primary"]
tertiary_age_rates <- age_rates$subscription_percent[age_rates$education == "tertiary"]
primary_band <- age_rates$age_band[age_rates$education == "primary"]
tertiary_band <- age_rates$age_band[age_rates$education == "tertiary"]
names(primary_age_rates) <- primary_band
names(tertiary_age_rates) <- tertiary_band
age_education_gap <- tertiary_age_rates[primary_band] - primary_age_rates
age_education_gap
# These descriptive bands still combine different ages and record characteristics.
# A consistent direction across bands cannot establish a causal education effect.

age_education_plot <- ggplot(age_rates, aes(x = age_band, y = subscription_percent,
                                            colour = education)) +
  geom_point(size = 3) +
  coord_cartesian(ylim = c(0, 35)) +
  labs(title = "Subscription by education within age bands",
       subtitle = "Records with no prior campaign contact",
       x = "Age band (years)", y = "Subscribed (%)", colour = "Education") +
  theme_minimal()
print(age_education_plot)



# STEP 7: ENCODE CATEGORICAL VARIABLES

# Encode a complete prepared copy after creating all preparation fields.
# Secondary is the reference. Unknown has its own flag rather than being imputed.
bank_encoded <- bank_clean
bank_encoded$education_primary <- ifelse(bank_encoded$education == "primary", 1, 0)
bank_encoded$education_tertiary <- ifelse(bank_encoded$education == "tertiary", 1, 0)
bank_encoded$education_unknown <- ifelse(bank_encoded$education == "unknown", 1, 0)
bank_encoded$housing_flag <- ifelse(bank_encoded$housing == "yes", 1, 0)
bank_encoded$loan_flag <- ifelse(bank_encoded$loan == "yes", 1, 0)
bank_encoded$default_flag <- ifelse(bank_encoded$default == "yes", 1, 0)

count(bank_encoded, education, education_primary, education_tertiary,
      education_unknown)
count(bank_encoded, housing, housing_flag)
count(bank_encoded, loan, loan_flag)
count(bank_encoded, default, default_flag)
# Original categories remain for audit; do not use both raw and dummy versions
# as numeric PCA inputs. y is never an input feature here.




