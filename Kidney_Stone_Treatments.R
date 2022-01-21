############################################################
# KIDNEY STONE TREATMENT PREDICTION

# Clear Everything
rm(list = ls())

# Set working directory
setwd("G:/My Drive/Projects-Self/KidneyStones")

# Get working directory
getwd()

# Get data
data <- read.csv("kidney_stone_data.csv")

# Examine data
head(data)
tail(data)
str(data)
summary(data)
colnames(data)


## CROSS TABULATION with CHI-SQ TESTS ======================

# need descr package for crosstab() function
library(descr)

# need expss package for apply_labels
library(expss)


# Seperate large and small stones ---------------------
size_filter <- data$stone_size == "large"
small_stones_data <- data[!size_filter,]
large_stones_data <- data[size_filter,]


# ONLY SMALL STONES -----------------------------------

# Use apply_labels 
small_stones_data <- apply_labels(small_stones_data,
                     treatment = "Treatment",
                     treatment = c("A" = "A",
                                   "B" = "B"),
                     success = "Success",
                     success = c("Yes" = 1,
                                 "No" = 0))

# Chi sq test - Does treatment type have significant effect on success for small stones?
crosstab(small_stones_data$success,
         small_stones_data$treatment,
         prop.r = F,
         prop.c = T,
         chisq = T)

# ONLY LARGE STONES -----------------------------------

# Use apply_labels 
large_stones_data <- apply_labels(large_stones_data,
                     treatment = "Treatment",
                     treatment = c("A" = "A",
                                   "B" = "B"),
                     success = "Success",
                     success = c("Yes" = 1,
                                 "No" = 0))

# Chi sq test - Does treatment type have significant effect on success for large stones?
crosstab(large_stones_data$success,
         large_stones_data$treatment,
         prop.r = F,
         prop.c = T,
         chisq = T)




# ALL SIZES TOGETHER -----------------------------------

# Use apply_labels 
data <- apply_labels(data,
                     treatment = "Treatment",
                     treatment = c("A" = "A",
                                   "B" = "B"),
                     success = "Success",
                     success = c("Yes" = 1,
                                 "No" = 0))

# Chi sq test - Does treatment type have significant effect on success?
crosstab(data$success,
         data$treatment,
         prop.r = F,
         prop.c = T,
         chisq = T)


# STONE SIZE AS PREDICTOR ------------------------------


# Use apply_labels
data <- apply_labels(data,
                     stone_size = "Stone Size",
                     success = "Success",
                     success = c("Yes" = 1,
                                 "No" = 0))

# Chi sq test - Does stone size have significant effect on success?
crosstab(data$success,
         data$stone_size,
         prop.r = F,
         prop.c = T,
         chisq = T)



# LOGISTIC REGRESSION ==============================

# Turn two binary nominal variables into dummy variables

dataset <- data.frame(Success = data$success,
                      isTreatmentA = ifelse(data$treatment == "A", 1, 0),
                      isLargeStone = ifelse(data$stone_size == "large", 1, 0))



logit_full_model <- glm(formula = Success ~ .,
                        family = binomial,
                        data = dataset)
summary(logit_full_model)

dataset$interaction <- dataset$isTreatmentA * dataset$isLargeStone







