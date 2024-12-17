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
  target <- input$selected_target
  datasetSelected <- input$choosedataset
  
  if (datasetSelected == "Upload-data") {
    # Check if both files are provided
    if (is.null(inFile1) || is.null(inFile2)) {
      # Show a modal dialog if any file is missing
      showModal(modalDialog(
        title = "No Data Selected",
        "Please select both datasets to proceed.",
        easyClose = TRUE
      ))
      return(NULL) # Return NULL to signify that the dataset is not loaded
    } else {
      # Read both files and return them
      feature_file1 <- read.csv(inFile1$datapath, row.names = 1, check.names = FALSE)
      feature_file1 = as.data.frame(t(feature_file1))
      target_file2 <- read.csv(inFile2$datapath, row.names = 1, check.names = FALSE)
      target_file2 = as.data.frame(t(target_file2))
      target_file3 = target_file2[, c(target), drop = FALSE]
      # Merging the datasets based on common row names
      merged_data <- merge(feature_file1, target_file3, by = "row.names", all = FALSE)
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
  
  shap_test_data <- test_data[, -which(names(test_data) == target)]
  
  # Find the index of the target column in the data frames
  target_index <- which(names(train_data) == target)
  
  # Create the xgb.DMatrix objects, excluding the target column
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -target_index]), label = train_data[[target]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, -target_index]), label = test_data[[target]])
  
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
  
  ctrl <- trainControl(method = "cv", number = cv)
  
  xgb_train <- train(
    train_data,
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
  
  xgb_model_optimized(list(model = xgb_model, predictions = predictions, test = shap_test_data, label_test = label_test))
  
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
      print(data_dis)
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
  
  xgb_model_optimized <- trainXGB(loadedDataset(),
                                  target,
                                  nrounds,
                                  max_depth,
                                  2,
                                  p)
  
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
    test_data <- xgb_all$test
    # Convert test_data to matrix
    test_data <- as.matrix(test_data)
    contr <- predict(my_model, test_data, predcontrib = TRUE)
    xgb.ggplot.shap.summary(test_data, contr, model = my_model, top_n = 12)
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