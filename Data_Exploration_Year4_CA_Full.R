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


