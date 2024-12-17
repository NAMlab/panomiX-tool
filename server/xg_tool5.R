# Reactive values for storing hyperparameters
# A queue of notification IDs
xgids <- character(0)
hyperparams <- reactiveValues(max_depth = 3, nrounds = 10, eta = 1, 
                              gamma = 0, colsample_bytree = 0.9,
                              min_child_weight = 1, subsample = 0.7)

# Define xgb_train as a reactive value
xgb_model_optimized <- reactiveVal(NULL)

loadedDataset <- reactive({
  inFile1 <- input$file1
  inFile2 <- input$file2
  inFile3 <- input$file3
  inFile4 <- input$file4
  target <- input$selected_target
  datasetSelected <- input$choosedataset
  repSelected <- input$choosereplicate
  
  if (datasetSelected == "Upload-data") {
    if (repSelected == "Replicate") {
      # Check if all necessary files are provided for replicate case
      if (is.null(inFile1) || is.null(inFile2) || is.null(inFile4)) {
        showModal(modalDialog(
          title = "Missing Files",
          "Please upload all required files for replication.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      # Read the files and process them for replication
      feature_file1 <- read.csv(inFile1$datapath, row.names = 1, check.names = FALSE)
      #feature_file1 <- round(feature_file1, 3)
      feature_file1 <- as.data.frame(t(feature_file1))
      target_file2 <- read.csv(inFile2$datapath, row.names = 1, check.names = FALSE)
      target_file2 <- as.data.frame(t(target_file2))
      target_file5 <- target_file2[, c(target), drop = FALSE]
      
      meta <- read.csv(inFile4$datapath, row.names = 1, check.names = FALSE)
      
      # Merging the datasets based on common row names
      merged_data <- merge(feature_file1, target_file5, by = "row.names", all = FALSE)
      # Set row names from the 'Row.names' column
      rownames(merged_data) <- merged_data$Row.names
      merged_data <- subset(merged_data, select = -`Row.names`)
      
      # Generate new names
      unique_times <- unique(meta$Time)
      time_dict <- list()
      for (time in unique_times) {
        time_dict[[time]] <- c()
      }
      
      counter <- setNames(rep(1, length(unique_times)), unique_times)
      new_names <- c()
      
      for (time in meta$Time) {
        new_name <- paste0("A", counter[time])
        new_names <- c(new_names, new_name)
        counter[time] <- counter[time] + 1
      }
      
      # Add new column to DataFrame
      meta$Group <- new_names
      
      # Filter rows where condition
      meta <- subset(meta, Condition == "Control")
      # Merging the datasets based on common row names
      merged_data <- merge(merged_data, meta, by = "row.names", all = FALSE)
      
      data <- subset(merged_data, select = -c(`Row.names`, `Time`, `Date`, `Condition`))
      
      return(data) # Return the merged data frame
    } else {
      # Check if both files are provided for non-replicate "Upload-data"
      if (is.null(inFile1) || is.null(inFile2)) {
        showModal(modalDialog(
          title = "No Data Selected",
          "Please select both datasets to proceed.",
          easyClose = TRUE
        ))
        return(NULL) # Return NULL to signify that the dataset is not loaded
      }
      
      # Read both files and process them for non-replicate case
      feature_file1 <- read.csv(inFile1$datapath, row.names = 1, check.names = FALSE)
      #feature_file1 <- round(feature_file1, 3)
      feature_file1 <- as.data.frame(t(feature_file1))
      target_file2 <- read.csv(inFile2$datapath, row.names = 1, check.names = FALSE)
      target_file2 <- as.data.frame(t(target_file2))
      target_file5 <- target_file2[, c(target), drop = FALSE]
      
      # Merging the datasets based on common row names
      merged_data <- merge(feature_file1, target_file5, by = "row.names", all = FALSE)
      data <- subset(merged_data, select = -`Row.names`)
      
      return(data) # Return the merged data frame
    }
  } else if (datasetSelected == "Example-data") {
    # Return the example dataset
    return(iris[, 1:4])
  } else {
    return(NULL) # Return NULL if the dataset selection is not recognized
  }
})

trainXGB <- function (data, 
                      target,
                      nrounds, 
                      max_depth,
                      cv,
                      p) {
  
  # Create an index for splitting
  #index <- createDataPartition(data$target, p = 0.8, list = FALSE)
  set.seed(123)
  splitIndex <- sample(1:nrow(data), p * nrow(data))
  
  # Create training and test sets
  train_data <- data[splitIndex, ]
  test_data <- data[-splitIndex, ]
  # Convert all columns to numeric for train_data
  train_data[] <- lapply(train_data, as.numeric)
  # Convert all columns to numeric for test_data
  test_data[] <- lapply(test_data, as.numeric)
  
  # Find the index of the target column in the data frames
  target_index <- which(names(train_data) == target)
  #change
  #scale it
  train_scaled <- scale(train_data[, -target_index])  # Scaling only absorbance columns
  
  # Fit scaling parameters from the training data
  scaling_params <- attr(train_scaled, "scaled:scale")
  scaling_means <- attr(train_scaled, "scaled:center")
  
  # Replace NaN and Inf with 0 using mutate_all
  train_scaled <- as.data.frame(train_scaled) %>%
    mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
  
  # Scale the test data using the training parameters
  test_scaled <- scale(test_data[, -target_index], center = scaling_means, scale = scaling_params)
  
  # Replace NaN and Inf with 0 using mutate_all
  test_scaled <- as.data.frame(test_scaled) %>%
    mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
  
  # Create the xgb.DMatrix objects, excluding the target column
  dtrain <- xgb.DMatrix(data = as.matrix(train_scaled), label = train_data[[target]])
  dtest <- xgb.DMatrix(data = as.matrix(test_scaled), label = test_data[[target]])
  
  shap_test_data <- test_scaled # change
  
  label_test = test_data[[target]]
  label = train_data[[target]] # new for the error stack overflow
  
  ## Model selection using CV
  ##Search Grid
  
  hyper_grid <- expand.grid(
    max_depth = max_depth,
    nrounds = nrounds,
    eta = seq(0.1, 1, by = 0.1),
    gamma = seq(0, 0.6, by = 0.2),
    colsample_bytree = seq(0.8, 1, by = 0.1),
    min_child_weight = c(1, 5),
    subsample = c(0.7, 0.8)
  )
  
  #hyper_grid <- expand.grid(
  #max_depth = seq(3, 5, by = 1),
  #nrounds = 50,
  #eta = seq(0.1, 1, by = 0.2),  # Reduced granularity
  #gamma = seq(0, 0.6, by = 0.3),  # Reduced granularity
  #colsample_bytree = c(0.5, 0.8),  # Reduced granularity
  #min_child_weight = c(1, 5),
  #subsample = c(0.5, 0.8)
  #)
  
  #ctrl <- trainControl(method = "cv", number = cv)
  ctrl <- trainControl(method = "LOOCV")
  
  xgb_train <- train(
    train_scaled, #change
    label,
    method = "xgbTree",
    trControl = ctrl,
    tuneGrid = hyper_grid,
    verbose = FALSE,
    verbosity = 0,
    metric = "RMSE"  # Specify an appropriate default metric
  )
  
  # Get the best hyperparameters
  best_params <- xgb_train$bestTune
  
  # Update reactive values with the best hyperparameters
  hyperparams$max_depth <- best_params$max_depth
  hyperparams$nrounds <- best_params$nrounds
  hyperparams$eta <- best_params$eta
  hyperparams$gamma <- best_params$gamma
  hyperparams$colsample_bytree <- best_params$colsample_bytree
  hyperparams$min_child_weight <- best_params$min_child_weight
  hyperparams$subsample <- best_params$subsample
  
  # Make predictions using the XGBoost model with optimized hyperparameters
  xgb_model <- xgboost(data = dtrain,
                       max.depth = isolate(hyperparams$max_depth),
                       nrounds = isolate(hyperparams$nrounds),
                       eta = isolate(hyperparams$eta),
                       gamma = isolate(hyperparams$gamma),
                       colsample_bytree = isolate(hyperparams$colsample_bytree),
                       min_child_weight = isolate(hyperparams$min_child_weight),
                       subsample = isolate(hyperparams$subsample),
                       objective = "reg:squarederror",
                       eval_metric = "rmse")
  
  
  predictions <- predict(xgb_model, dtest)
  
  RSQUARE = function(y_actual,y_predict){
    cor(y_actual,y_predict)^2
  }
  LR_R = RSQUARE(label_test,predictions)
  print(LR_R)
  
  xgb_model_optimized(list(model = xgb_model, predictions = predictions, test = shap_test_data, label_test = label_test,
                           best_params = best_params, train_data = train_data, test_data = test_data))
  
  return(xgb_model_optimized)
}

trainXGB_rep <- function (data, 
                          target,
                          nrounds, 
                          max_depth,
                          cv,
                          p) {
  
  # Seeds for different splits
  seeds <- c(012, 123, 456, 789)
  
  best_params_all <- list()
  train_data_all <- list()
  test_data_all <- list()
  xgb_model_all <- list()
  predictions_all <- list()
  shap_test_data_all <- list()
  label_test_all <- list()
  
  r_squared_values <- c()
  
  best_r_squared <- -Inf
  best_split_index <- NULL
  
  for (i in 1:4) {
    set.seed(seeds[i])
    
    groups <- unique(data$Group)
    shuffled_groups <- sample(groups)
    
    # Determine split point
    split_index <- floor(length(shuffled_groups) * p)
    
    # Split groups into training and testing
    train_groups <- shuffled_groups[1:split_index]
    test_groups <- shuffled_groups[(split_index + 1):length(shuffled_groups)]
    
    # Create training and testing sets
    train_data <- data[data$Group %in% train_groups, ]
    test_data <- data[data$Group %in% test_groups, ]
    
    # Remove the 'Group' column
    train_data$Group <- NULL
    test_data$Group <- NULL
    
    # Convert all columns to numeric for train_data and test_data
    train_data[] <- lapply(train_data, as.numeric)
    test_data[] <- lapply(test_data, as.numeric)
    
    target_index <- which(names(train_data) == target)
    #change
    #scale it
    train_scaled <- scale(train_data[, -target_index])  # Scaling only absorbance columns
    
    # Fit scaling parameters from the training data
    scaling_params <- attr(train_scaled, "scaled:scale")
    scaling_means <- attr(train_scaled, "scaled:center")
    
    # Replace NaN and Inf with 0 using mutate_all
    train_scaled <- as.data.frame(train_scaled) %>%
      mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
    
    # Scale the test data using the training parameters
    test_scaled <- scale(test_data[, -target_index], center = scaling_means, scale = scaling_params)
    
    # Replace NaN and Inf with 0 using mutate_all
    test_scaled <- as.data.frame(test_scaled) %>%
      mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
    
    # Create the xgb.DMatrix objects, excluding the target column
    dtrain <- xgb.DMatrix(data = as.matrix(train_scaled), label = train_data[[target]])
    dtest <- xgb.DMatrix(data = as.matrix(test_scaled), label = test_data[[target]])
    
    shap_test_data <- test_scaled # change
    
    label_test = test_data[[target]]
    label = train_data[[target]] # new for the error stack overflow
    
    ## Model selection using CV
    ##Search Grid
    hyper_grid <- expand.grid(
      max_depth = max_depth,
      nrounds = nrounds,
      eta = seq(0.1, 1, by = 0.1),
      gamma = seq(0, 0.6, by = 0.2),
      colsample_bytree = seq(0.8, 1, by = 0.1),
      min_child_weight = c(1, 5),
      subsample = c(0.7, 0.8)
    )
    
    #hyper_grid <- expand.grid(
    #max_depth = seq(3, 5, by = 1),
    #nrounds = 50,
    #eta = seq(0.1, 1, by = 0.2),  # Reduced granularity
    #gamma = seq(0, 0.6, by = 0.3),  # Reduced granularity
    #colsample_bytree = c(0.5, 0.8),  # Reduced granularity
    #min_child_weight = c(1, 5),
    #subsample = c(0.5, 0.8)
    #)
    
    ctrl <- trainControl(method = "cv", number = cv)
    #ctrl <- trainControl(method = "LOOCV")
    
    xgb_train <- train(
      train_scaled, #change
      label,
      method = "xgbTree",
      trControl = ctrl,
      tuneGrid = hyper_grid,
      verbose = FALSE,
      verbosity = 0,
      metric = "RMSE"  # Specify an appropriate default metric
    )
    
    # Get the best hyperparameters
    best_params <- xgb_train$bestTune
    
    # Update reactive values with the best hyperparameters
    hyperparams$max_depth <- best_params$max_depth
    hyperparams$nrounds <- best_params$nrounds
    hyperparams$eta <- best_params$eta
    hyperparams$gamma <- best_params$gamma
    hyperparams$colsample_bytree <- best_params$colsample_bytree
    hyperparams$min_child_weight <- best_params$min_child_weight
    hyperparams$subsample <- best_params$subsample
    
    # Make predictions using the XGBoost model with optimized hyperparameters
    xgb_model <- xgboost(data = dtrain,
                         max.depth = isolate(hyperparams$max_depth),
                         nrounds = isolate(hyperparams$nrounds),
                         eta = isolate(hyperparams$eta),
                         gamma = isolate(hyperparams$gamma),
                         colsample_bytree = isolate(hyperparams$colsample_bytree),
                         min_child_weight = isolate(hyperparams$min_child_weight),
                         subsample = isolate(hyperparams$subsample),
                         objective = "reg:squarederror",
                         eval_metric = "rmse")
    
    
    predictions <- predict(xgb_model, dtest)
    
    RSQUARE = function(y_actual,y_predict){
      cor(y_actual,y_predict)^2
    }
    LR_R = RSQUARE(label_test,predictions)
    print(LR_R)
    
    r_squared_values <- c(r_squared_values, LR_R)
    
    # Store the best parameters, train_data, and test_data
    best_params_all[[i]] <- best_params
    train_data_all[[i]] <- train_data
    test_data_all[[i]] <- test_data
    xgb_model_all[[i]] <- xgb_model
    predictions_all[[i]] <- predictions
    shap_test_data_all[[i]] <- shap_test_data
    label_test_all[[i]] <- label_test
    
    # Track the best R-squared value and its corresponding index
    if (LR_R > best_r_squared) {
      best_r_squared <- LR_R
      best_split_index <- i
    }
  }
  
  # Use the best split's parameters and train/test data for Boruta
  best_params <- best_params_all[[best_split_index]]
  train_data <- train_data_all[[best_split_index]]
  test_data <- test_data_all[[best_split_index]]
  xgb_model <- xgb_model_all[[best_split_index]]
  predictions <- predictions_all[[best_split_index]]
  shap_test_data <- shap_test_data_all[[best_split_index]]
  label_test <- label_test_all[[best_split_index]]
  
  xgb_model_optimized(list(model = xgb_model, predictions = predictions, test = shap_test_data, label_test = label_test,
                           best_params = best_params, train_data = train_data, test_data = test_data))
  
  return(xgb_model_optimized)
}

output$target_data <- renderUI({
  inFile2 <- input$file2
  datasetSelected <- input$choosedataset
  
  if (datasetSelected == "Upload-data") {
    if (!is.null(inFile2)) {
      dataset2 <- read.csv(inFile2$datapath, row.names = 1, check.names = FALSE)
      dataset = as.data.frame(t(dataset2))
      default_target <- names(dataset)[1]  # Select the first column as default target
    } else {
      return(NULL)
    }
  }
  
  if (datasetSelected == "Example-data") {
    dataset <- iris[, 1:4]
    default_target <- names(dataset)[1]  # Select the first column as default target
  } 
  
  radioButtons("selected_target", 
               "Target:",
               names(dataset),
               selected = default_target)  # Set default selected target
})

output$distributionPlot <- renderPlotly({
  
  datasetSelected <- input$choosedataset
  inFile2 <- input$file2
  
  if (datasetSelected == "Upload-data") {
    if (!is.null(inFile2)) {
      data_dis2 <- read.csv(inFile2$datapath, row.names = 1, check.names = FALSE)
      data_dis = as.data.frame(t(data_dis2))
    } else {
      return(NULL)
    }
  }
  if (datasetSelected == "Example-data") {
    data_dis <- iris[, 1:4]
  } 
  target <- input$selected_target  # Use a reactive expression to encapsulate input$selected_target
  
  tryCatch({
    gg <- ggplot(data_dis, aes(x = !!sym(target))) +  # Use the encapsulated target variable
      geom_density(fill = "skyblue", color = "blue") +
      labs(title = paste("Distribution of", target))
    p <- ggplotly(gg)
    return(p)
  }, error = function(e) {
    output$error_notification <- renderUI({
      tagList(
        div("Error: No data available for plotting"),
        br(),
        actionButton("reload_data", "Reload Data", icon = icon("refresh"))
      )
    })
    return(NULL)
  })
})

# React to the optimize button click event
observeEvent(input$optimize_btn, {
  
  ###-----------###
  # Record the start time of tuning process
  start_time <- Sys.time()
  # Show notification that tuning process has started
  xgid = showNotification(paste("Optimization process has started at", format(start_time, "%Y-%m-%d %H:%M:%S"), ". Please wait..."), duration = NULL)
  xgids <<- c(xgids, xgid)
  ###-----------###
  
  target <- input$selected_target
  nrounds <- input$nrounds
  max_depth <- input$max_depth
  p <- input$proportion
  
  datasetSelected <- input$choosedataset
  repSelected <- input$choosereplicate
  
  # Start optimization process based on dataset and replicate selection
  if (datasetSelected == "Upload-data") {
    if (repSelected == "Replicate") {
      # Replicate case for Upload-data
      xgb_model_optimized <- trainXGB_rep(loadedDataset(), target, nrounds, max_depth, 3, p)
      #xgb_model_optimized(model)  # Store the result in the reactive value
    } else {
      # Non-replicate case for Upload-data
      xgb_model_optimized <- trainXGB(loadedDataset(), target, nrounds, max_depth, 3, p)
      #xgb_model_optimized(model)  # Store the result in the reactive value
    }
  } else if (datasetSelected == "Example-data") {
    # Example-data case
    xgb_model_optimized <- trainXGB(loadedDataset(), target, nrounds, max_depth, 3, p)
    #xgb_model_optimized(model)  # Store the result in the reactive value
  }
  
  xgb_all <- xgb_model_optimized()
  predictions <- xgb_all$predictions
  
  # Display the prediction result
  output$prediction_text <- renderText({
    paste("Predicted value:", predictions)
  })
  
  # Display the prediction result as a table
  output$prediction_table <- renderDT({
    data.frame(
      Prediction = predictions
    )
  })
})

output$feature_importance <- DT::renderDataTable({ 
  if (input$xgb_btn > 0) {
    xgb_all <- xgb_model_optimized()
    
    if (!is.null(xgb_all)) {
      xgb_model <- xgb_all$model
      df <- xgb.importance(model = xgb_model)
      
      df %>%
        head(15) %>%
        datatable(
          style = 'bootstrap',
          options = list(
            dom = 't',
            ordering = FALSE
          )
        ) %>%
        formatPercentage(c("Gain", "Cover", "Frequency"), digits = 2) %>%
        formatStyle(
          'Gain',
          background = styleColorBar(df$Gain, 'lightgreen'),
          backgroundSize = '90% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Cover',
          background = styleColorBar(df$Cover, 'lightyellow'),
          backgroundSize = '90% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Frequency',
          background = styleColorBar(df$Frequency, 'lightblue'),
          backgroundSize = '90% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    } else {
      DT::datatable(data.frame(), options = list(dom = 't'))
    }
  }
})

output$shap_plot <- renderPlotly({
  if (input$xgb_btn > 0) {
    xgb_all <- xgb_model_optimized()
    if (!is.null(xgb_all)) {
      my_model <- xgb_all$model
      stest_data <- xgb_all$test
      # Convert test_data to matrix
      stest_data <- as.matrix(stest_data)
      contr <- predict(my_model, stest_data, predcontrib = TRUE)
      xgb.ggplot.shap.summary(stest_data, contr, model = my_model)
    } else {
      output$error_notification <- renderUI({
        tagList(
          div("Error: No data available for plotting"),
          br(),
          actionButton("reload_data", "Reload Data", icon = icon("refresh"))
        )
      })
      return(NULL)
    }}
})

output$boruta_plot <- renderPlotly({
  if (input$xgb_btn > 0) {
    xgb_all <- xgb_model_optimized()
    if (!is.null(xgb_all)) {
      my_model <- xgb_all$model
      best_params = xgb_all$best_params 
      train_data = xgb_all$train_data 
      test_data = xgb_all$test_data
      target <- input$selected_target
      
      #change
      # Extract the target variable
      y_train <- train_data[[target]]
      y_test <- test_data[[target]]
      
      #scale it
      target_index <- which(names(train_data) == target)
      
      train_scaled <- scale(train_data[, -target_index])  # Scaling only absorbance columns
      
      # Fit scaling parameters from the training data
      scaling_params <- attr(train_scaled, "scaled:scale")
      scaling_means <- attr(train_scaled, "scaled:center")
      
      # Replace NaN and Inf with 0 using mutate_all
      train_scaled <- as.data.frame(train_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      # Scale the test data using the training parameters
      test_scaled <- scale(test_data[, -target_index], center = scaling_means, scale = scaling_params)
      
      # Replace NaN and Inf with 0 using mutate_all
      test_scaled <- as.data.frame(test_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      # Combine the scaled features and the target variable into a single data frame
      train_combined <- cbind(train_scaled, y_train)
      
      # Optionally, you can rename the target variable column
      colnames(train_combined)[ncol(train_combined)] <- target
      
      set.seed(42)
      boruta_result <- Boruta(as.formula(paste(target, "~ .")), data = train_combined, doTrace = 2) # , getImp = getImpXgboost
      
      # Get the confirmed attributes
      confirmed_attributes <- getSelectedAttributes(boruta_result, withTentative = T) # to include tentative features
      
      # Subset the data with selected features
      X_train_boruta <- train_scaled[, confirmed_attributes, drop = FALSE]
      X_test_boruta <- test_scaled[, confirmed_attributes, drop = FALSE]
      
      # Create the xgb.DMatrix objects, excluding the target column
      dtrain_boruta <- xgb.DMatrix(data = as.matrix(X_train_boruta), label = train_data[[target]])
      dtest_boruta <- xgb.DMatrix(data = as.matrix(X_test_boruta), label = test_data[[target]])
      
      xgb_boruta_model <- xgboost(data = dtrain_boruta,
                                  max.depth = best_params$max_depth,
                                  nrounds = best_params$nrounds,
                                  eta = best_params$eta,
                                  gamma = best_params$gamma,
                                  colsample_bytree = best_params$colsample_bytree,
                                  min_child_weight = best_params$min_child_weight,
                                  subsample = best_params$subsample,
                                  objective = "reg:squarederror",
                                  eval_metric = "rmse")
      
      boruta_predictions <- predict(xgb_boruta_model, dtest_boruta)
      
      RSQUARE = function(y_actual,y_predict){
        cor(y_actual,y_predict)^2
      }
      
      r_squared_boruta_values <- RSQUARE(y_test, boruta_predictions)
      
      # Compute SHAP values and save the SHAP summary plot
      contr <- predict(xgb_boruta_model, as.matrix(X_test_boruta), predcontrib = TRUE)
      b_shap_values <- as.data.frame(contr)
      
      b_shap_values$prediction <- boruta_predictions
      
      xgb.ggplot.shap.summary(as.matrix(X_test_boruta), contr, model = xgb_boruta_model) #change
      
    } else {
      output$error_notification <- renderUI({
        tagList(
          div("Error: No data available for plotting"),
          br(),
          actionButton("reload_data", "Reload Data", icon = icon("refresh"))
        )
      })
      return(NULL)
    }}
})

output$interaction_plot <- renderPlotly({
  if (input$cons_btn > 0) {
    conSelected <- input$constraints
    inFile5 <- input$file5
    xgb_all <- xgb_model_optimized()
    if (conSelected == "Random") {
      my_model <- xgb_all$model
      best_params = xgb_all$best_params 
      train_data = xgb_all$train_data 
      test_data = xgb_all$test_data
      target <- input$selected_target
      
      #change
      # Extract the target variable
      y_train <- train_data[[target]]
      y_test <- test_data[[target]]
      
      #scale it
      target_index <- which(names(train_data) == target)
      
      train_scaled <- scale(train_data[, -target_index])  # Scaling only absorbance columns
      
      # Fit scaling parameters from the training data
      scaling_params <- attr(train_scaled, "scaled:scale")
      scaling_means <- attr(train_scaled, "scaled:center")
      
      # Replace NaN and Inf with 0 using mutate_all
      train_scaled <- as.data.frame(train_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      # Scale the test data using the training parameters
      test_scaled <- scale(test_data[, -target_index], center = scaling_means, scale = scaling_params)
      
      # Replace NaN and Inf with 0 using mutate_all
      test_scaled <- as.data.frame(test_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      train_data = train_scaled 
      test_data = test_scaled #change
      
      test_s<- xgb_all$test
      # Convert test_data to matrix
      test_s <- as.matrix(test_s)
      contr <- predict(my_model, test_s, predcontrib = TRUE)
      
      condata <- xgb.ggplot.shap.summary(test_s, contr, model = my_model)
      condata = condata$data
      
      # Add a column for association based on the conditions
      conlist_association <- condata %>%
        mutate(
          association = case_when(
            shap_value > 0 & feature_value > 0 ~ 1,
            shap_value > 0 & feature_value < 0 ~ -1,
            TRUE ~ NA_real_ # Assign NA for other cases
          )
        )
      
      # Filter and summarize associations by feature
      conlist_summary <- conlist_association %>%
        filter(!is.na(association)) %>% # Exclude rows with NA in association
        group_by(feature) %>%
        summarise(
          final_association = case_when(
            any(association == 1) ~ 1,        # If at least one association is 1
            all(association == -1) ~ -1,     # If all associations are -1
            TRUE ~ 0                         # Optional: for features not meeting the above (default to 0)
          )
        )
      conlist1 <- conlist_summary[,c("feature"), drop = FALSE]
      
      conlist2 <- read.csv(inFile5$datapath, sep = ";", check.names = FALSE)
      conlist2 <- conlist2[,c("feature"), drop = FALSE]
      
      # Combine the data frames
      conlist3 <- rbind(conlist1, conlist2)
      conlist3 <- unique(conlist3)
      
      # Extract the features from conlist3
      features_to_keep <- conlist3$feature
      
      # Subset the train and test data to only keep these features
      X_train_data <- train_data[, features_to_keep, drop = FALSE]
      X_test_data <- test_data[, features_to_keep, drop = FALSE]
      
      # Create the xgb.DMatrix objects, excluding the target column
      dtrain_con <- xgb.DMatrix(data = as.matrix(X_train_data), label = y_train)
      dtest_con <- xgb.DMatrix(data = as.matrix(X_test_data), label = y_test)
      
      xgb_model_con <- xgboost(data = dtrain_con,
                               max.depth = best_params$max_depth,
                               nrounds = best_params$nrounds,
                               eta = best_params$eta,
                               gamma = best_params$gamma,
                               colsample_bytree = best_params$colsample_bytree,
                               min_child_weight = best_params$min_child_weight,
                               subsample = best_params$subsample,
                               objective = "reg:squarederror",
                               eval_metric = "rmse")
      
      # Compute SHAP values and save the SHAP summary plot
      contr_con <- predict(xgb_model_con, as.matrix(X_test_data), predcontrib = TRUE)
      xgb.ggplot.shap.summary(as.matrix(X_test_data), contr_con, model = xgb_model_con)
      
    } else if (conSelected == "Monotonic") {
      my_model <- xgb_all$model
      best_params = xgb_all$best_params 
      train_data = xgb_all$train_data 
      test_data = xgb_all$test_data
      target <- input$selected_target
      
      #change
      # Extract the target variable
      y_train <- train_data[[target]]
      y_test <- test_data[[target]]
      
      #scale it
      target_index <- which(names(train_data) == target)
      
      train_scaled <- scale(train_data[, -target_index])  # Scaling only absorbance columns
      
      # Fit scaling parameters from the training data
      scaling_params <- attr(train_scaled, "scaled:scale")
      scaling_means <- attr(train_scaled, "scaled:center")
      
      # Replace NaN and Inf with 0 using mutate_all
      train_scaled <- as.data.frame(train_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      # Scale the test data using the training parameters
      test_scaled <- scale(test_data[, -target_index], center = scaling_means, scale = scaling_params)
      
      # Replace NaN and Inf with 0 using mutate_all
      test_scaled <- as.data.frame(test_scaled) %>%
        mutate(across(everything(), ~ replace(., is.na(.) | is.infinite(.), 0)))
      
      train_data = train_scaled 
      test_data = test_scaled #change
      
      test_s<- xgb_all$test
      # Convert test_data to matrix
      test_s <- as.matrix(test_s)
      contr <- predict(my_model, test_s, predcontrib = TRUE)
      
      condata <- xgb.ggplot.shap.summary(test_s, contr, model = my_model)
      condata = condata$data
      
      # Add a column for association based on the conditions
      conlist_association <- condata %>%
        mutate(
          association = case_when(
            shap_value > 0 & feature_value > 0 ~ 1,
            shap_value > 0 & feature_value < 0 ~ -1,
            TRUE ~ NA_real_ # Assign NA for other cases
          )
        )
      
      # Filter and summarize associations by feature
      conlist_summary <- conlist_association %>%
        filter(!is.na(association)) %>% # Exclude rows with NA in association
        group_by(feature) %>%
        summarise(
          final_association = case_when(
            any(association == 1) ~ 1,        # If at least one association is 1
            all(association == -1) ~ -1,     # If all associations are -1
            TRUE ~ 0                         # Optional: for features not meeting the above (default to 0)
          )
        )
      conlist1 <- conlist_summary #monotone
      conlist2 <- read.csv(inFile5$datapath, sep = ";", check.names = FALSE)
      
      # Combine the data frames
      conlist3 <- rbind(conlist1, conlist2)
      conlist3 <- unique(conlist3)
      # Convert 'feature' in conlist3 to character first
      conlist3 <- conlist3 %>%
        mutate(feature = as.character(feature))
      
      # Sort column names of train_data lexicographically (from small to big)
      sorted_colnames <- sort(colnames(train_data))
      # Reorder the columns of train_data based on the sorted column names
      train_data <- train_data[, sorted_colnames]
      # Sort column names of train_data lexicographically (from small to big)
      sorted_colnames <- sort(colnames(test_data))
      # Reorder the columns of train_data based on the sorted column names
      test_data <- test_data[, sorted_colnames]
      
      # Derive features_to_keep from the matched training data
      features_to_keep <- as.character(colnames(train_data))
      
      # Only keep rows in conlist3 where features are in features_to_keep
      # Now filter rows based on 'features_to_keep' and sort by 'feature'
      trimmed_conlist3 <- conlist3 %>%
        filter(feature %in% features_to_keep) %>%  # Keep only rows where 'feature' is in 'features_to_keep'
        arrange(feature)  # Sort by 'feature' from smallest to largest
      
      # Extract the features from conlist3
      features_to_keep <- trimmed_conlist3$feature
      
      # Step 3: Extract monotone constraints in the correct order
      aligned_constraints <- trimmed_conlist3$final_association
      
      # Step 4: Create the monotone constraints string
      monotone_constraints <- paste0("(", paste(aligned_constraints, collapse = ","), ")")
      
      # Step 5: Subset train_data and test_data to include only features_to_keep
      X_train_data <- train_data[, features_to_keep, drop = FALSE]
      X_test_data <- test_data[, features_to_keep, drop = FALSE]
      
      # Step 6: Validate alignment
      if (length(aligned_constraints) != ncol(X_train_data)) {
        stop("Mismatch between monotone constraints and training data features.")
      }
      
      # Create the xgb.DMatrix objects, excluding the target column
      dtrain_con <- xgb.DMatrix(data = as.matrix(X_train_data), label = y_train)
      dtest_con <- xgb.DMatrix(data = as.matrix(X_test_data), label = y_test)
      
      xgb_model_con <- xgboost(data = dtrain_con,
                               max.depth = best_params$max_depth,
                               nrounds = best_params$nrounds,
                               eta = best_params$eta,
                               gamma = best_params$gamma,
                               colsample_bytree = best_params$colsample_bytree,
                               min_child_weight = best_params$min_child_weight,
                               subsample = best_params$subsample,
                               monotone_constraints = monotone_constraints,
                               objective = "reg:squarederror",
                               eval_metric = "rmse")
      
      # Compute SHAP values and save the SHAP summary plot
      contr_con <- predict(xgb_model_con, as.matrix(X_test_data), predcontrib = TRUE)
      xgb.ggplot.shap.summary(as.matrix(X_test_data), contr_con, model = xgb_model_con)
      
    } else {
      output$error_notification <- renderUI({
        tagList(
          div("Error: No data available for plotting"),
          br(),
          actionButton("reload_data", "Reload Data", icon = icon("refresh"))
        )
      })
      return(NULL)
    }}
})

output$com_plot <- renderPlotly({
  
  # Remove the oldest notification from the queue
  if (length(xgids) > 0) {
    removeNotification(xgids[1])
    xgids <<- xgids[-1]
  }
  
  xgb_all <- xgb_model_optimized()
  if (!is.null(xgb_all)) {
    predictions <- xgb_all$predictions
    label_test_data <- xgb_all$label_test
    
    # Calculate variance and R-squared
    variance <- var(predictions)
    RSQUARE <- cor(label_test_data, predictions)^2
    
    # Create scatter plot using plot_ly
    plot <- plot_ly() %>%
      #plot_ly(x = label_test_data, y = predictions, type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Original Values"), yaxis = list(title = "Predicted Values"), title = "Original vs. Predicted Values") %>%
      add_trace(x = label_test_data, y = predictions, error_y = list(type = "data", array = sqrt(variance), visible = TRUE), type = "scatter", mode = "markers", name = "Variance") %>%
      add_trace(x = label_test_data, y = label_test_data, type = "scatter", mode = "lines", line = list(color = "red"), name = "Equality line")
    
    # Create text box annotation for R-squared
    annotation <- list(
      x = 1.1,  # Adjust the position of the box as needed
      y = 0.5,
      text = paste("R-squared:", round(RSQUARE, digits = 2)),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(color = "black", size = 12)
    )
    
    # Add the annotation to the plot
    plot <- plot %>% layout(annotations = list(annotation))
    
    # Return the plot
    plot
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

observe({
  updateSliderInput(session, "max_depth", value = hyperparams$max_depth)
  updateSliderInput(session, "nrounds", value = hyperparams$nrounds)
  # Add updates for other sliders as needed
})

output$download <- renderUI({
  if (input$xgb_btn > 0) {
    xgb_all <- xgb_model_optimized()
    if (!is.null(xgb_all)) {
      downloadButton("d_data","Download_features")
    }}
})

output$d_data <- downloadHandler(
  filename = function () {
    paste("feature_importance.csv")
  },
  content = function(file) {
    xgb_all <- xgb_model_optimized()
    xgb_model <- xgb_all$model
    df <- xgb.importance(model = xgb_model)
    write.csv(df, file, row.names = FALSE)
  }
)