# Load the mtcars dataset
data(mtcars)

# Fit a linear regression model
model <- lm(mpg ~ wt + drat, data = mtcars)

# Extract parameter estimates
parameters <- coef(model)[-1]  # Exclude the intercept

# Extract 95% confidence intervals
conf_intervals <- confint(model)[-1, ]  # Exclude the intercept row

# Combine parameter estimates and confidence intervals
estimates_and_intervals <- cbind(parameters, conf_intervals)

# Print parameter estimates and confidence intervals
print(estimates_and_intervals)

# Create a barplot with error bars
barplot(parameters, 
        ylim = range(c(0, c(parameters, conf_intervals))), 
        main = "Parameter Estimates with 95% Confidence Intervals",
        ylab = "Estimates", col = "skyblue", names.arg = names(parameters))

# Add error bars
arrows(x0 = 1:length(parameters), y0 = estimates_and_intervals[, 2], 
       y1 = estimates_and_intervals[, 3], angle = 90, code = 3, 
       length = 0.1, lwd = 2, col = "black")

library(mixOmics)

data(breast.TCGA)
X1 <- t(breast.TCGA$data.train$mirna)[1:60, c(1,2,3,141,142,143)]
X2 <- t(breast.TCGA$data.train$mrna)[1:60, c(1,2,3,141,142,143)]
X3 <- t(breast.TCGA$data.train$protein)[1:60, c(1,2,3,141,142,143)]


X1 = as.data.frame(X1)
X2 = as.data.frame(X2)
X3 = as.data.frame(X3)

stacked_df1 <- data.frame(omics1 = unlist(X1))
stacked_df2 <- data.frame(omics2 = unlist(X2))
stacked_df3 <- data.frame(omics3 = unlist(X3))

rownames(stacked_df1) <- NULL
rownames(stacked_df2) <- NULL
rownames(stacked_df3) <- NULL
cor(merged_df$omics1, merged_df$omics2)
merged_df <- cbind(stacked_df1, stacked_df2, stacked_df3)

# Fit a linear regression model
model <- lm(omics3 ~ omics1 + omics2, data = merged_df)

# Extract parameter estimates
parameters <- coef(model)[-1]  # Exclude the intercept

# Extract 95% confidence intervals
conf_intervals <- confint(model)[-1, ]  # Exclude the intercept row

# Combine parameter estimates and confidence intervals
estimates_and_intervals <- cbind(parameters, conf_intervals)

# Print parameter estimates and confidence intervals
print(estimates_and_intervals)

# Create a barplot with error bars
barplot(parameters, 
        ylim = range(c(0, c(parameters, conf_intervals))), 
        main = "Parameter Estimates with 95% Confidence Intervals",
        ylab = "Estimates", col = "skyblue", names.arg = names(parameters))

# Add error bars
arrows(x0 = 1:length(parameters), y0 = estimates_and_intervals[, 2], 
       y1 = estimates_and_intervals[, 3], angle = 90, code = 3, 
       length = 0.1, lwd = 2, col = "black")

########################### mean calculation ################################
library(mixOmics)

data(breast.TCGA)
X1 <- t(breast.TCGA$data.train$mirna)[1:60, c(1,2,3,141,142,143)]
X2 <- t(breast.TCGA$data.train$mrna)[1:60, c(1,2,3,141,142,143)]
X3 <- t(breast.TCGA$data.train$protein)[1:60, c(1,2,3,141,142,143)]


X1 = as.data.frame(X1)
X2 = as.data.frame(X2)
X3 = as.data.frame(X3)

# Calculate column means
means_X1 <- colMeans(X1)
means_X2 <- colMeans(X2)
means_X3 <- colMeans(X3)

# Create a dataframe of means
means_X1 <- data.frame(omics1 = means_X1)
means_X2 <- data.frame(omics2 = means_X2)
means_X3 <- data.frame(omics3 = means_X3)

rownames(means_X1) <- NULL
rownames(means_X2) <- NULL
rownames(means_X3) <- NULL

merged_df <- cbind(means_X1, means_X2, means_X3)

# Fit a linear regression model
model <- lm(omics3 ~ omics1 + omics2, data = merged_df)

# Extract parameter estimates
parameters <- coef(model)[-1]  # Exclude the intercept

# Extract 95% confidence intervals
conf_intervals <- confint(model)[-1, ]  # Exclude the intercept row

# Combine parameter estimates and confidence intervals
estimates_and_intervals <- cbind(parameters, conf_intervals)

# Print parameter estimates and confidence intervals
print(estimates_and_intervals)

# Create a barplot with error bars
barplot(parameters, 
        ylim = range(c(0, c(parameters, conf_intervals))), 
        main = "Parameter Estimates with 95% Confidence Intervals",
        ylab = "Estimates", col = "skyblue", names.arg = names(parameters))

# Add error bars
arrows(x0 = 1:length(parameters), y0 = estimates_and_intervals[, 2], 
       y1 = estimates_and_intervals[, 3], angle = 90, code = 3, 
       length = 0.1, lwd = 2, col = "black")


