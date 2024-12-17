
# Define as a reactive value
harm_data <- reactiveVal(NULL)
combined_pca_data <- reactiveVal(NULL)
pca1_data <- reactiveVal(NULL)
pca2_data <- reactiveVal(NULL)
pca3_data <- reactiveVal(NULL)
anova_data <- reactiveVal(NULL)

# New added
observeEvent(input$dh_choosedataset, {
  dh_inFile1 <- input$dh_file1
  dh_inFile2 <- input$dh_file2
  dh_inFile3 <- input$dh_file3
  dh_meta <- input$meta_file
  dh_datasetSelected <- input$dh_choosedataset
  
  if (dh_datasetSelected == "Upload-data") {
    if (is.null(dh_inFile1) || is.null(dh_inFile2) || is.null(dh_inFile3) || is.null(dh_meta)) {
      # Add a notification to add data
      showModal(modalDialog(
        title = "No Data Selected",
        "Please select all three datasets and meta data or upload your own data files.",
        easyClose = TRUE
      ))
      harm_data(NULL)
    } else {
      harmfile1 <- read.csv(dh_inFile1$datapath, row.names = 1, check.names = FALSE)
      harmfile2 <- read.csv(dh_inFile2$datapath, row.names = 1, check.names = FALSE)
      harmfile3 <- read.csv(dh_inFile3$datapath, row.names = 1, check.names = FALSE)
      meta_data <- read.csv(dh_meta$datapath, check.names = FALSE)
      rownames(harmfile1) <- NULL
      rownames(harmfile2) <- NULL
      rownames(harmfile3) <- NULL
      
      # Step 2: Find the common columns across all three data frames
      common_columns <- Reduce(intersect, list(colnames(harmfile1), colnames(harmfile2), colnames(harmfile3)))
      
      # Step 3: Subset each data frame based on the common columns
      harmfile1 <- harmfile1[, common_columns]
      harmfile2 <- harmfile2[, common_columns]
      harmfile3 <- harmfile3[, common_columns]
      
      # Step 1: Subset the meta_data to include only rows where the ID matches the common_columns
      meta_data <- meta_data[meta_data$ID %in% common_columns, ]
      
      # Step 2: Ensure that the order of rows in meta_data matches the order of common_columns
      meta_data <- meta_data[match(common_columns, meta_data$ID), ]
      
      # Assuming omics is a function to handle multi-block data
      harm_data(list(harmfile1 = harmfile1, harmfile2 = harmfile2, harmfile3 = harmfile3, meta = meta_data))
    }
  } else if (dh_datasetSelected == "Example-data") {
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
    harm_data(list(harmfile1 = X1, harmfile2 = X2, harmfile3 = X3, meta = meta_data))
  }
})

observeEvent(input$run, {
  harm_all <- harm_data()
  
  if (!is.null(harm_all)) {
    
    df1 <- harm_all$harmfile1
    df2 <- harm_all$harmfile2
    df3 <- harm_all$harmfile3
    meta_data <- harm_all$meta
    
    df1_scaled <- t(scale(t(df1), center = TRUE, scale = TRUE))
    df2_scaled <- t(scale(t(df2), center = TRUE, scale = TRUE))
    df3_scaled <- t(scale(t(df3), center = TRUE, scale = TRUE))
    
    # Calculate the sum of squares for each matrix
    sum_of_squares_df1_scaled  <- sum(df1_scaled^2)
    sum_of_squares_df2_scaled  <- sum(df2_scaled^2)
    sum_of_squares_df3_scaled  <- sum(df3_scaled^2)
    
    
    # Calculate the scaling factor and set all datasets to 1000 sum of squares
    scaling_factor <- sqrt(1000 / sum_of_squares_df1_scaled)
    df1_scaled_sqrt = df1_scaled * scaling_factor
    
    scaling_factor <- sqrt(1000 / sum_of_squares_df2_scaled)
    df2_scaled_sqrt = df2_scaled * scaling_factor
    
    scaling_factor <- sqrt(1000 / sum_of_squares_df3_scaled)
    df3_scaled_sqrt = df3_scaled * scaling_factor
    
    sum_of_squares_df1_scaled  <- sum(df1_scaled_sqrt^2)
    sum_of_squares_df2_scaled  <- sum(df2_scaled_sqrt^2)
    sum_of_squares_df3_scaled  <- sum(df3_scaled_sqrt^2)
    
    # Concatenate
    combined_data <- rbind(df1_scaled_sqrt, df2_scaled_sqrt, df3_scaled_sqrt)
    scaled_data_r <- t(combined_data)
    
    f1 = function(x, pca_scores_x, eigenvectors_x){
      eigenvectors_x[,1:ncol(eigenvectors_x) != x] = 0
      projected_data_x = pca_scores_x %*% t(eigenvectors_x)
      return(sum(projected_data_x^2))
    }
    
    f2 = function(x, pca_scores_x, eigenvectors_x){
      eigenvectors_x[,1:ncol(eigenvectors_x) != x] = 0
      projected_data_x = pca_scores_x %*% t(eigenvectors_x)
      return(c(
        "omics3" = sum(projected_data_x[,datasets == "omics3"]^2),
        "omics2" = sum(projected_data_x[,datasets == "omics2"]^2),
        "omics1" = sum(projected_data_x[,datasets == "omics1"]^2)
      )/sum(projected_data_x^2)
      )
    }
    
    # Calculate the covariance matrix of the scaled data:
    cov_matrix_r <- cov(scaled_data_r)
    # Compute the eigenvectors and eigenvalues of the covariance matrix:
    num_eigen <- 10
    eigen_result_r = irlba(cov_matrix_r, nv = num_eigen)
    # eigen_result_r <- eigen(cov_matrix_r) # thim might take some time
    eigenvalues_r <- eigen_result_r$d
    eigenvectors_r <- eigen_result_r$v
    # Project the scaled data onto the principal components (eigenvectors):
    pca_scores_r <- scaled_data_r %*% eigenvectors_r
    # Calculate explained variance:
    explained_variance_r <- eigenvalues_r / sum(eigenvalues_r)
    cumulative_variance_r <- cumsum(explained_variance_r)
    # Reconstruct projected data using the first 10 PCs (just for fun)
    projected_data_r = pca_scores_r %*% t(eigenvectors_r)
    
    # make a vector of data types, T = transcripts, P = proteins, M = metabolites
    datasets_r = c(rep("omics1", nrow(df1)), rep("omics2", nrow(df2)), rep("omics3", nrow(df3)) )
    datasets = c(rep("omics1", nrow(df1)), rep("omics2", nrow(df2)), rep("omics3", nrow(df3)) )
    
    # Extract scores and explained variance for each individual dataset
    eigenvectors_M_r = eigenvectors_r
    eigenvectors_M_r[datasets_r != "omics3", ] = 0
    pca_scores_M_r= scaled_data_r %*% eigenvectors_M_r
    explained_variance_M_r = sapply(1:ncol(pca_scores_M_r), function(x)   f1(x,pca_scores_M_r,eigenvectors_M_r)/sum(scaled_data_r^2))
    
    eigenvectors_P_r = eigenvectors_r
    eigenvectors_P_r[datasets_r != "omics2", ] = 0
    pca_scores_P_r= scaled_data_r %*% eigenvectors_P_r
    explained_variance_P_r = sapply(1:ncol(pca_scores_P_r), function(x)   f1(x,pca_scores_P_r,eigenvectors_P_r)/sum(scaled_data_r^2))
    
    eigenvectors_T_r = eigenvectors_r
    eigenvectors_T_r[datasets_r != "omics1", ] = 0
    pca_scores_T_r= scaled_data_r %*% eigenvectors_T_r
    explained_variance_T_r = sapply(1:ncol(pca_scores_T_r), function(x)   f1(x,pca_scores_T_r,eigenvectors_T_r)/sum(scaled_data_r^2))
    
    # Calculate explained variance for the integrated data and proportion of variance contribution of each omic dataset
    explained_variance_Z_r = sapply(1:ncol(pca_scores_r), function(x)   f2(x,pca_scores_r,eigenvectors_r))
    explained_variance_Z_prop_r = t(explained_variance_Z_r)/explained_variance_r
    
    # combined PCA data
    sample_names = colnames(df1)
    scores = as.data.frame(pca_scores_r)
    colnames(scores) = paste("PC", 1:10, sep = "")
    tmp = meta_data
    scores$Sample <- sample_names
    scores$Treatment <- tmp[,2]
    scores$Stage <- tmp[, 3]
    # combined list
    combined_pca_data(list(scores = scores, explained_variance_Z_r = explained_variance_Z_r, explained_variance_r = explained_variance_r))
    
    # PCA1 data
    pca_result1 = prcomp(t(df1_scaled_sqrt), scale = F, center = F)
    scores_T = as.data.frame(pca_result1$x)
    scores_T$Sample <- sample_names
    scores_T$Treatment <- tmp[,2]
    scores_T$Stage <- tmp[,3]
    # PCA1 list
    pca1_data(list(pca_result1 = pca_result1, scores_T = scores_T))
    
    # PCA2 data
    pca_result2 = prcomp(t(df2_scaled_sqrt), scale = F, center = F)
    scores_P = as.data.frame(pca_result2$x)
    scores_P$Sample <- sample_names
    scores_P$Treatment <- tmp[,2]
    scores_P$Stage <- tmp[,3]
    # PCA2 list
    pca2_data(list(pca_result2 = pca_result2, scores_P = scores_P))
    
    # PCA3 data
    pca_result3 = prcomp(t(df3_scaled_sqrt), scale = F, center = F)
    scores_M = as.data.frame(pca_result3$x)
    scores_M$Sample <- sample_names
    scores_M$Treatment <- tmp[,2]
    scores_M$Stage <- tmp[,3]
    # PCA3 list
    pca3_data(list(pca_result3 = pca_result3, scores_M = scores_M))
  }
})

observeEvent(input$run_anova, {
  harm_all <- harm_data()
  
  if (!is.null(harm_all)) {
    
    X1 <- harm_all$harmfile1
    X2 <- harm_all$harmfile2
    X3 <- harm_all$harmfile3
    meta_data <- harm_all$meta
    
    #Rename columns
    names(meta_data)[1] <- "ID"
    names(meta_data)[2] <- "condition1"
    names(meta_data)[3] <- "condition2"
    
    # Control data
    # Step 1: Find the unique values in the condition1 column
    unique_times <- unique(meta_data$condition2)
    if (!(length(unique_times) > 1)) {
      # Control condition for selecting rows based on values
      condition <- meta_data$condition2 == unique_times[1]
      
      # Select rows based on the condition and all columns
      control <- meta_data[condition, ]
      
      # Genotype1 based selection
      # Step 2: Find the unique values in the condition1 column
      unique_genotypes <- unique(control$condition1)
      
      Genotype1 <- control$condition1 == unique_genotypes[1]
      # Select rows based on the condition and all columns
      Moneymaker <- control[Genotype1, c('ID')]
      # Convert the "ID" column to a character vector
      Moneymaker_C0 <- X1[, as.character(Moneymaker)]
      
      # Convert Moneymaker_C0 to a data frame
      Moneymaker_C0 <- data.frame(Moneymaker_C0)
      # Add condition1 column with values "Control"
      Moneymaker_C0$condition2 <- unique_times[1]
      # Reshape the data
      CTR_Moneymaker_C0 <- Moneymaker_C0 %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Moneymaker"
      CTR_Moneymaker_C0$condition1 <- unique_genotypes[1]
      
      # Control Genotype2 based selection
      Genotype2 <- control$condition1 == unique_genotypes[2]
      # Select rows based on the condition and all columns
      Saladette <- control[Genotype2, c('ID')]
      # Convert the "ID" column to a character vector
      Saladette_C0 <- X1[, as.character(Saladette)]
      
      # Convert Saladette_C0 to a data frame
      Saladette_C0 <- data.frame(Saladette_C0)
      # Add condition1 column with values "Control"
      Saladette_C0$condition2 <- unique_times[1]
      # Reshape the data
      CTR_Saladette_C0 <- Saladette_C0 %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Saladette"
      CTR_Saladette_C0$condition1 <- unique_genotypes[2]
      merged_df1 <- merge(CTR_Moneymaker_C0, CTR_Saladette_C0, all = TRUE)
      
      # Input directory and data
      my_data <- merged_df1
      # Step 3: Run the two-way ANOVA using the "aov" function
      #my_model <- aov(Score ~ condition1 + condition2:condition1, data = my_data)
      # Run one-way ANOVA
      my_model <- aov(Score ~ condition1, data = my_data)
      
      # Step 4: Check the ANOVA table to see if there are significant effects for each factor and their interaction
      a = summary(my_model)
      anova_t = a[[1]]
      # list output
      anova_data(anova_t)
      
    } else if (length(unique_times) >= 2) {
      # Control condition for selecting rows based on values
      condition <- meta_data$condition2 == unique_times[1]
      
      # Select rows based on the condition and all columns
      control <- meta_data[condition, ]
      
      # Genotype1 based selection
      # Step 2: Find the unique values in the condition1 column
      unique_genotypes <- unique(control$condition1)
      
      Genotype1 <- control$condition1 == unique_genotypes[1]
      # Select rows based on the condition and all columns
      Moneymaker <- control[Genotype1, c('ID')]
      # Convert the "ID" column to a character vector
      Moneymaker_C0 <- X1[, as.character(Moneymaker)]
      
      # Convert Moneymaker_C0 to a data frame
      Moneymaker_C0 <- data.frame(Moneymaker_C0)
      # Add condition1 column with values "Control"
      Moneymaker_C0$condition2 <- unique_times[1]
      # Reshape the data
      CTR_Moneymaker_C0 <- Moneymaker_C0 %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Moneymaker"
      CTR_Moneymaker_C0$condition1 <- unique_genotypes[1]
      
      # Control Genotype2 based selection
      Genotype2 <- control$condition1 == unique_genotypes[2]
      # Select rows based on the condition and all columns
      Saladette <- control[Genotype2, c('ID')]
      # Convert the "ID" column to a character vector
      Saladette_C0 <- X1[, as.character(Saladette)]
      
      # Convert Saladette_C0 to a data frame
      Saladette_C0 <- data.frame(Saladette_C0)
      # Add condition1 column with values "Control"
      Saladette_C0$condition2 <- unique_times[1]
      # Reshape the data
      CTR_Saladette_C0 <- Saladette_C0 %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Saladette"
      CTR_Saladette_C0$condition1 <- unique_genotypes[2]
      
      # Treated data
      # treated condition for selecting rows based on values
      condition <- meta_data$condition2 == unique_times[2]
      # Select rows based on the condition and all columns
      treated <- meta_data[condition, ]
      
      # Genotype1 based selection
      # Step 2: Find the unique values in the condition1 column
      unique_genotypes <- unique(treated$condition1)
      
      Genotype1 <- treated$condition1 == unique_genotypes[1]
      # Select rows based on the condition and all columns
      Moneymaker <- treated[Genotype1, c('ID')]
      # Convert the "ID" column to a character vector
      Moneymaker_T <- X1[, as.character(Moneymaker)]
      
      # Convert Moneymaker_T to a data frame
      Moneymaker_T <- data.frame(Moneymaker_T)
      # Add condition1 column with values "Control"
      Moneymaker_T$condition2 <- unique_times[2]
      # Reshape the data
      Moneymaker_T <- Moneymaker_T %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Moneymaker"
      Moneymaker_T$condition1 <- unique_genotypes[1]
      
      # Treated Genotype2 based selection
      Genotype2 <- treated$condition1 == unique_genotypes[2]
      # Select rows based on the condition and all columns
      Saladette <- treated[Genotype2, c('ID')]
      # Convert the "ID" column to a character vector
      Saladette_T <- X1[, as.character(Saladette)]
      
      # Convert Saladette_T to a data frame
      Saladette_T <- data.frame(Saladette_T)
      # Add condition1 column with values "Control"
      Saladette_T$condition2 <- unique_times[2]
      # Reshape the data
      Saladette_T <- Saladette_T %>%
        gather(key = "Variable", value = "Score", -condition2)
      # Create a new column "condition1" with the value "Saladette"
      Saladette_T$condition1 <- unique_genotypes[2]
      
      #merging all tables
      merged_df1 <- merge(CTR_Moneymaker_C0, CTR_Saladette_C0, all = TRUE)
      merged_df2 <- merge(Moneymaker_T, Saladette_T, all = TRUE)
      
      #merging INTO FINAL tables
      CTR_TR_Geno <- merge(merged_df1, merged_df2, all = TRUE)
      
      # Input directory and data
      my_data <- CTR_TR_Geno
      # Step 3: Run the two-way ANOVA using the "aov" function
      my_model <- aov(Score ~ condition2 + condition1 + condition2:condition1, data = my_data)
      # Step 4: Check the ANOVA table to see if there are significant effects for each factor and their interaction
      a = summary(my_model)
      anova_t = a[[1]]
      # list output
      anova_data(anova_t)
    }
  }
})

output$anova_table <- renderDT({
  anova_t = anova_data()
  if (input$run_anova) {
    datatable(anova_t)
  }
})

output$pca_plot <- renderPlotly({
  combined_pca_data_all <- combined_pca_data()
  if (!is.null(combined_pca_data_all)) {
    
    scores <- combined_pca_data_all$scores
    explained_variance_Z_r <- combined_pca_data_all$explained_variance_Z_r
    explained_variance_r <- combined_pca_data_all$explained_variance_r
    
    ggplot(scores, aes(x = PC1, y = PC2, shape = Stage)) +
      geom_point(size = 4) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      #stat_ellipse(geom = "polygon", alpha = 0.1, level = 0.2) +
      #geom_point(aes(color = Treatment), size = 4) +
      geom_point(aes(color = Treatment), size = 6) +
      scale_color_viridis(discrete = TRUE, option = "A") + # Use viridis palette
      theme_classic() +
      theme(legend.position = "right",
            axis.title = element_text(size = 9),
            plot.title = element_text(size = 9),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 9)) +
      labs(title = "Integrated multi-Omics data",
           x = paste("(PC1:", round(explained_variance_r[1] * 100, 2), "% ",
                     "omics1:", round(explained_variance_Z_r[3,1] * 100, 2), "%",
                     "omics2:", round(explained_variance_Z_r[2,1] * 100, 2), "% ",
                     "omics3:", round(explained_variance_Z_r[1,1] * 100, 2), "% ",
                     ")" ),
           y = paste("(PC2:", round(explained_variance_r[2] * 100, 2), "% ",
                     "omics1:", round(explained_variance_Z_r[3,2] * 100, 2), "%",
                     "omics2:", round(explained_variance_Z_r[2,2] * 100, 2), "% ",
                     "omics3:", round(explained_variance_Z_r[1,2] * 100, 2), "% ",
                     ")" ),
           color = "Treatment",
           shape = "Stage")
    
  } else {
    output$error_notification <- renderUI({
      tagList(
        div("Error: No data available for plotting"),
        br(),
        actionButton("reload_data", "Reload Data", icon = icon("refresh"))
      )
    })
    return(NULL)
  }
})

output$pca1_plot <- renderPlotly({
  pca1_data_all <- pca1_data()
  pca2_data_all <- pca2_data()
  pca3_data_all <- pca3_data()
  
  if (!is.null(pca1_data_all) && !is.null(pca2_data_all) && !is.null(pca3_data_all)) {
    scores_T <- pca1_data_all$scores_T
    pca_result1 <- pca1_data_all$pca_result1
    
    p1 <- ggplot(scores_T, aes(x = PC1, y = PC2, shape = Stage)) +
      geom_point(size = 4) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      #stat_ellipse(geom = "polygon", alpha = 0.1, level = 0.2) +
      #geom_point(aes(color = Treatment), size = 4) +
      geom_point(aes(color = Treatment), size = 6) +
      scale_color_viridis(discrete = TRUE, option = "A") + # Use viridis palette
      theme_classic() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            plot.title = element_text(size = 9)) +
      labs(x = paste("PC1 (", round(pca_result1$sdev[1]^2 / sum(pca_result1$sdev^2) * 100, 2), "%)", sep = ""),
           y = paste("PC2 (", round(pca_result1$sdev[2]^2 / sum(pca_result1$sdev^2) * 100, 2), "%)", sep = ""),
           color = "Treatment",
           shape = "Stage")
    
    scores_P <- pca2_data_all$scores_P
    pca_result2 <- pca2_data_all$pca_result2
    
    p2 <- ggplot(scores_P, aes(x = PC1, y = PC2, shape = Stage)) +
      geom_point(size = 4) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      #stat_ellipse(geom = "polygon", alpha = 0.1, level = 0.2) +
      #geom_point(aes(color = Treatment), size = 4) +
      geom_point(aes(color = Treatment), size = 6) +
      scale_color_viridis(discrete = TRUE, option = "A") + # Use viridis palette
      theme_classic() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            plot.title = element_text(size = 9)) +
      labs(x = paste("PC1 (", round(pca_result2$sdev[1]^2 / sum(pca_result2$sdev^2) * 100, 2), "%)", sep = ""),
           y = paste("PC2 (", round(pca_result2$sdev[2]^2 / sum(pca_result2$sdev^2) * 100, 2), "%)", sep = ""),
           color = "Treatment",
           shape = "Stage")
    
    scores_M <- pca3_data_all$scores_M
    pca_result3 <- pca3_data_all$pca_result3
    
    p3 <- ggplot(scores_M, aes(x = PC1, y = PC2, shape = Stage)) +
      geom_point(size = 4) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      #stat_ellipse(geom = "polygon", alpha = 0.1, level = 0.2) +
      #geom_point(aes(color = Treatment), size = 4) +
      geom_point(aes(color = Treatment), size = 6) +
      scale_color_viridis(discrete = TRUE, option = "A") + # Use viridis palette
      theme_classic() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            plot.title = element_text(size = 9)) +
      labs(x = paste("PC1 (", round(pca_result3$sdev[1]^2 / sum(pca_result3$sdev^2) * 100, 2), "%)", sep = ""),
           y = paste("PC2 (", round(pca_result3$sdev[2]^2 / sum(pca_result3$sdev^2) * 100, 2), "%)", sep = ""),
           color = "Treatment",
           shape = "Stage")
    
    # Create a grid layout of p1, p2, and p3
    subplot(list(p1, p2, p3), nrows = 1, margin = 0.06, shareX = TRUE,
            titleY = TRUE)%>%  
      layout(annotations = list( 
        list(x = 0.13 , y = 1.08, text = "omics1", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.5 , y = 1.08, text = "omics2", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.9 , y = 1.08, text = "omics3", showarrow = F, xref='paper', yref='paper')) 
      )
    
  } else {
    output$error_notification <- renderUI({
      tagList(
        div("Error: No data available for plotting"),
        br(),
        actionButton("reload_data", "Reload Data", icon = icon("refresh"))
      )
    })
    return(NULL)
  }
})