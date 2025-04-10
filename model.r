# Load necessary libraries
library(stats)
library(base)
library(graphics)
library(dplyr)
library(caTools)
library(car)
library(caret)
library(carData)
library(lattice)
library(randomForest)
library(ranger)

# Load Dataset
df <- read.csv("AmesHousing.csv")

# Feature Selection: High-Correlation Factors
cor_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
cor_target <- cor_matrix["SalePrice", ]
high_corr_features <- names(cor_target[abs(cor_target) > 0.4]) # Select highly correlated features

# Ensure important categorical variables are included
selected_features <- c("SalePrice", "Gr.Liv.Area", "Lot.Area", "Overall.Qual", "Bldg.Type", 
                       "House.Style", "Exter.Qual", "Bsmt.Cond", "Bedroom.AbvGr", "Garage.Cars", 
                       "Year.Built", "Full.Bath", "Kitchen.Qual")

# Filter only selected columns
selected_features <- selected_features[selected_features %in% colnames(df)]
df <- df %>% select(all_of(selected_features)) %>% na.omit()

# Convert categorical features to factors
df <- df %>%
  mutate(
    Bldg.Type = as.factor(Bldg.Type),
    House.Style = as.factor(House.Style),
    Exter.Qual = factor(Exter.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
    Bsmt.Cond = factor(Bsmt.Cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
    Kitchen.Qual = factor(Kitchen.Qual, levels = c("Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  )
df <- na.omit(df)
# Train-Test Split
set.seed(42)
trainIndex <- createDataPartition(df$SalePrice, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Train Model with Tuned Parameters
model <- randomForest(SalePrice ~ ., data = trainData, ntree = 300, mtry = 5, importance = TRUE)

# Define the tuning grid for Random Forest
tune_grid <- expand.grid(
  mtry = c(3, 5, 7),          # Number of variables randomly sampled at each split
  splitrule = c("variance"),  # Regression uses "variance"
  min.node.size = c(5, 10, 15) # Minimum observations per terminal node
)

# Train the model using caret's train() function with cross-validation
control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
rf_tuned <- train(
  SalePrice ~ ., 
  data = trainData, 
  method = "ranger",   # Fast implementation of Random Forest
  trControl = control, 
  tuneGrid = tune_grid,
  importance = "impurity"
)

# Define the best hyperparameters
best_tune <- rf_tuned$bestTune

# Saving the model
saveRDS(model, "rf_tuned.rds")
