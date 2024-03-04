# Function to perform linear regression
linear_regression <- function(x, y) {
  # Number of observations
  n <- length(x)
  
  # Mean of x and y
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # Calculate the sum of squares
  SS_xy <- sum(x * y) - n * mean_x * mean_y
  SS_xx <- sum(x^2) - n * mean_x^2
  
  # Calculate the coefficients (slope and intercept)
  slope <- SS_xy / SS_xx
  intercept <- mean_y - slope * mean_x
  
  # Return the coefficients
  return(list(slope = slope, intercept = intercept))
}

# Function to predict using linear regression
predict_linear <- function(x, slope, intercept) {
  return(slope * x + intercept)
}

# Generate some sample data
set.seed(123)
x <- 1:100
y <- 2 * x + rnorm(100, mean = 0, sd = 10)

# Perform linear regression
coefficients <- linear_regression(x, y)

# Print coefficients
cat("Coefficients:\n")
cat("Intercept:", coefficients$intercept, "\n")
cat("Slope:", coefficients$slope, "\n")

# Predict using the model
predictions <- predict_linear(x, coefficients$slope, coefficients$intercept)

# Plot the data and the regression line
plot(x, y, main = "Linear Regression", xlab = "X", ylab = "Y")
abline(coefficients$intercept, coefficients$slope, col = "red")
