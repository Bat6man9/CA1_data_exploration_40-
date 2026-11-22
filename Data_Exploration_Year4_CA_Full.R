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

