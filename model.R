# Split the data into training and testing sets for conventional
train_indices_c <- sample(nrow(df_filtered_c), 0.8 * nrow(df_filtered_c))
train_data_c <- df_filtered_c[train_indices, ]
test_data_c <- df_filtered_c[-train_indices, ]

# Build the linear model
model_trained_c <- lm(Total.Volume ~ AveragePrice + region + Month + year, data = train_data_c)

predictions <- predict(model, newdata = test_data_c)

# Split the data into training and testing sets for organic
train_indices_o <- sample(nrow(df_filtered_o), 0.8 * nrow(df_filtered_o))
train_data_o <- df_filtered_o[train_indices, ]
test_data_o <- df_filtered_o[-train_indices, ]

# Build the linear model
model_trained_o <- lm(Total.Volume ~ AveragePrice + region + Month + year, data = train_data_o)

predictions <- predict(model, newdata = test_data_o)
