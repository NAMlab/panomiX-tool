library(mixOmics)
library(tidyverse)

data(breast.TCGA)
X1 <- t(breast.TCGA$data.train$mirna)[1:60, c(1, 2, 3, 4, 5, 6, 61, 62, 63, 141, 142, 143)]
X2 <- t(breast.TCGA$data.train$mrna)[1:60, c(1, 2, 3, 4, 5, 6, 61, 62, 63, 141, 142, 143)]
X3 <- t(breast.TCGA$data.train$protein)[1:60, c(1, 2, 3, 4, 5, 6, 61, 62, 63, 141, 142, 143)]

# rename column
colnames(X1) <- c("Control.C1", "Control.C2", "Control.C3", "Control.C1", "Control.C2", "Control.C3", "Treated.A1", "Treated.A2", "Treated.A3", "Treated.A1", "Treated.A2", "Treated.A3")
colnames(X2) <- c("Control.C1", "Control.C2", "Control.C3", "Control.C1", "Control.C2", "Control.C3", "Treated.A1", "Treated.A2", "Treated.A3", "Treated.A1", "Treated.A2", "Treated.A3")
colnames(X3) <- c("Control.C1", "Control.C2", "Control.C3", "Control.C1", "Control.C2", "Control.C3", "Treated.A1", "Treated.A2", "Treated.A3", "Treated.A1", "Treated.A2", "Treated.A3")

rownames(X1) <- NULL
rownames(X2) <- NULL
rownames(X3) <- NULL

meta_data <- data.frame(
  ID = c("Control.C1", "Control.C2", "Control.C3", "Control.C1", "Control.C2", "Control.C3", "Treated.A1", "Treated.A2", "Treated.A3", "Treated.A1", "Treated.A2", "Treated.A3"),
  Genotype = c("Moneymaker", "Moneymaker", "Moneymaker", "Saladette", "Saladette", "Saladette", "Moneymaker", "Moneymaker", "Moneymaker", "Saladette", "Saladette", "Saladette"),
  time = c(rep("C", 6), rep("TA", 6))
)

# Control data

# Control Condition for selecting rows based on values
condition <- meta_data$time == "C"
# Select rows based on the condition and all columns
control <- meta_data[condition, ]

# Genotype1 based selection

Genotype1 <- control$Genotype == "Moneymaker"
# Select rows based on the condition and all columns
Moneymaker <- control[Genotype1, c('ID')]
# Convert the "ID" column to a character vector
Moneymaker_C0 <- X1[, as.character(Moneymaker)]

# Convert Moneymaker_C0 to a data frame
Moneymaker_C0 <- data.frame(Moneymaker_C0)
# Add Treatment column with values "Control"
Moneymaker_C0$Treatment <- "Control"
# Reshape the data
CTR_Moneymaker_C0 <- Moneymaker_C0 %>%
  gather(key = "Variable", value = "Score", -Treatment)
# Create a new column "Genotype" with the value "Moneymaker"
CTR_Moneymaker_C0$Genotype <- "Moneymaker"

# Control Genotype2 based selection
Genotype2 <- control$Genotype == "Saladette"
# Select rows based on the condition and all columns
Saladette <- control[Genotype2, c('ID')]
# Convert the "ID" column to a character vector
Saladette_C0 <- X1[, as.character(Saladette)]

# Convert Saladette_C0 to a data frame
Saladette_C0 <- data.frame(Saladette_C0)
# Add Treatment column with values "Control"
Saladette_C0$Treatment <- "Control"
# Reshape the data
CTR_Saladette_C0 <- Saladette_C0 %>%
  gather(key = "Variable", value = "Score", -Treatment)
# Create a new column "Genotype" with the value "Saladette"
CTR_Saladette_C0$Genotype <- "Saladette"

# Treated data

# treated Condition for selecting rows based on values
condition <- meta_data$time == "TA"
# Select rows based on the condition and all columns
treated <- meta_data[condition, ]

# Genotype1 based selection

Genotype1 <- treated$Genotype == "Moneymaker"
# Select rows based on the condition and all columns
Moneymaker <- treated[Genotype1, c('ID')]
# Convert the "ID" column to a character vector
Moneymaker_T <- X1[, as.character(Moneymaker)]

# Convert Moneymaker_T to a data frame
Moneymaker_T <- data.frame(Moneymaker_T)
# Add Treatment column with values "Control"
Moneymaker_T$Treatment <- "Treated"
# Reshape the data
Moneymaker_T <- Moneymaker_T %>%
  gather(key = "Variable", value = "Score", -Treatment)
# Create a new column "Genotype" with the value "Moneymaker"
Moneymaker_T$Genotype <- "Moneymaker"

# Treated Genotype2 based selection
Genotype2 <- treated$Genotype == "Saladette"
# Select rows based on the condition and all columns
Saladette <- treated[Genotype2, c('ID')]
# Convert the "ID" column to a character vector
Saladette_T <- X1[, as.character(Saladette)]

# Convert Saladette_T to a data frame
Saladette_T <- data.frame(Saladette_T)
# Add Treatment column with values "Control"
Saladette_T$Treatment <- "Treated"
# Reshape the data
Saladette_T <- Saladette_T %>%
  gather(key = "Variable", value = "Score", -Treatment)
# Create a new column "Genotype" with the value "Saladette"
Saladette_T$Genotype <- "Saladette"

#merging all tables
merged_df1 <- merge(CTR_Moneymaker_C0, CTR_Saladette_C0, all = TRUE)
merged_df2 <- merge(Moneymaker_T, Saladette_T, all = TRUE)

#merging INTO FINAL tables
CTR_TR_Geno <- merge(merged_df1, merged_df2, all = TRUE)

# Input directory and data
my_data <- CTR_TR_Geno
# Step 3: Run the two-way ANOVA using the "aov" function
my_model <- aov(Score ~ Treatment + Genotype + Treatment:Genotype, data = my_data)
# Step 4: Check the ANOVA table to see if there are significant effects for each factor and their interaction
summary(my_model)


# Check for balanced design
table(my_data$Treatment, my_data$Genotype)

# Check assumptions
qqnorm(resid(my_model))
qqline(resid(my_model))
# Conduct post-hoc tests (e.g., Tukey's HSD)
TukeyHSD(my_model)

# Calculate effect sizes
eta_squared(my_model)

# Create interaction plot
interaction.plot(x.factor = my_data$Treatment, trace.factor = my_data$Genotype, response = my_data$Score)

# Report results
summary(my_model)

# Extract sums of squares
anova_table <- anova(my_model)
ss_total <- sum(anova_table$Sum[1])
ss_treatment <- anova_table$Sum[2]
ss_genotype <- anova_table$Sum[3]
ss_interaction <- anova_table$Sum[4]

# Calculate partial eta-squared
eta_squared_treatment <- ss_treatment / ss_total
eta_squared_genotype <- ss_genotype / ss_total
eta_squared_interaction <- ss_interaction / ss_total

# Output results
eta_squared_results <- data.frame(
  Factor = c("Treatment", "Genotype", "Interaction"),
  Partial_Eta_Squared = c(eta_squared_treatment, eta_squared_genotype, eta_squared_interaction)
)

