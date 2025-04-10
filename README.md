🏡 House Revaluation App
A comprehensive Shiny web application that estimates house prices based on key property features, using machine learning models for accurate predictions.This app allows users to input property features such as location, square footage, number of bedrooms, and more to predict house prices. It provides an interactive interface, data preprocessing pipeline, and model deployment, making it an excellent tool for understanding housing price dynamics.

Model URL : https://aditya-kumar-roy.shinyapps.io/HousePredX/

🔍 Key Features:
Interactive User Interface (UI): Built using Shiny to collect user input for house features (e.g., area, number of rooms, neighborhood).
Data Preprocessing Pipeline: Clean the raw dataset by handling missing values, encoding categorical variables, scaling numerical features, and splitting data into training and test sets.
Modeling: Developed using machine learning algorithms (e.g., linear regression, random forests) to predict house prices.
Model Evaluation: Metrics such as R-squared, RMSE, and MAE to evaluate model performance.
Data Visualization: Visual representations of house price trends and correlations between features using ggplot2.

🛠 Technologies & Tools:
R Programming for data science tasks.
Shiny for creating the interactive web application.
Caret and randomForest for machine learning model training.
ggplot2 for data visualization.
AmesHousing dataset, containing historical housing data to train the model.

📊 Data Preprocessing & Model Pipeline:
Loading Data: The dataset is loaded from a CSV file, which contains features like sale price, square footage, number of bedrooms, and more.
Data Cleaning: Handling missing values using imputation or removal of rows/columns.
Encoding categorical features like neighborhood, house type, etc., into numerical values using one-hot encoding.
Feature scaling to standardize numerical values, ensuring all features contribute equally to the model.
Feature Engineering: Creation of new features based on domain knowledge, such as price per square foot.
Data Splitting: The dataset is split into training and testing sets, with 80% for training and 20% for testing.

💻 Machine Learning Model:
Model Selection: Multiple algorithms (e.g., Linear Regression, Random Forests) are tested to predict house prices.
Training: The chosen model is trained on the training set using caret package for cross-validation and hyperparameter tuning.
Evaluation: The model's performance is evaluated using metrics like R-squared, RMSE, and MAE on the test set to ensure the accuracy and robustness of predictions.

📈 Data Visualization:
The app provides dynamic charts and graphs that showcase the relationship between different property features and their impact on price. These visualizations help users understand market trends, correlations, and patterns in the housing data.

Model Deployment:
Model is deployed at shinyapps.io server and anyone with internet access can use it.
