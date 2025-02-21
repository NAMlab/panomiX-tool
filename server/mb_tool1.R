# A queue of notification IDs
mbids <- character(0)

# Define xgb_train as a reactive value
omics_data <- reactiveVal(NULL)
omics_ex <- reactiveVal(NULL)
omics_data3 <- reactiveVal(NULL)
cor_data <- reactiveVal(NULL)
latent_score <- reactiveVal(NULL)
estimates_score <- reactiveVal(NULL)

# New added

observeEvent(input$mb_choosedataset, {
  mb_inFile1 <- input$mb_file1
  mb_inFile2 <- input$mb_file2
  mb_inFile3 <- input$mb_file3
  mb_datasetSelected <- input$mb_choosedataset
  
  if (mb_datasetSelected == "Upload-data") {
    if (is.null(mb_inFile1) || is.null(mb_inFile2)) {
      # Add a notification to add data
      showModal(modalDialog(
        title = "Insufficient Data Selected",
        "Please select at least two datasets or upload your own data files.",
        easyClose = TRUE
      ))
      omics_data(NULL)
    } else {
      omicsfile1 <- read.csv(mb_inFile1$datapath, row.names = 1)
      omicsfile2 <- read.csv(mb_inFile2$datapath, row.names = 1)
      # Select common column names
      common_columns <- intersect(colnames(omicsfile1), colnames(omicsfile2))
      if (length(common_columns) == 0) {
        # Notify user if there are no common columns
        showModal(modalDialog(
          title = "No Common Columns",
          "The selected datasets have no common columns.",
          easyClose = TRUE
        ))
        omics_data(NULL)
      } else {
        omicsfile1 <- omicsfile1[, common_columns, drop = FALSE]
        omicsfile2 <- omicsfile2[, common_columns, drop = FALSE]
        if (is.null(mb_inFile3)) {
          omics_data(list(omicsfile1 = omicsfile1, omicsfile2 = omicsfile2))
        } else {
          omicsfile3 <- read.csv(mb_inFile3$datapath, row.names = 1)
          common_columns <- Reduce(intersect, list(colnames(omicsfile1), colnames(omicsfile2), colnames(omicsfile3)))
          omicsfile1 <- omicsfile1[, common_columns, drop = FALSE]
          omicsfile2 <- omicsfile2[, common_columns, drop = FALSE]
          omicsfile3 <- omicsfile3[, common_columns, drop = FALSE]
          omics_data3(list(omicsfile1 = omicsfile1, omicsfile2 = omicsfile2, omicsfile3 = omicsfile3))
        }
      }
    }
  } else if (mb_datasetSelected == "Example-data") {
    data(breast.TCGA)
    X1 <- t(breast.TCGA$data.train$mirna)[1:60, c(1,2,3,141,142,143)]
    X2 <- t(breast.TCGA$data.train$mrna)[1:60, c(1,2,3,141,142,143)]
    X3 <- t(breast.TCGA$data.train$protein)[1:60, c(1,2,3,141,142,143)]
    omics_data3(list(omicsfile1 = X1, omicsfile2 = X2, omicsfile3 = X3))
  } 
})

observeEvent(input$check_btn, {
  
  ###-----------###
  # Record the start time of tuning process
  start_time <- Sys.time()
  # Show notification that tuning process has started
  mbid = showNotification(paste("Checking process has started at", format(start_time, "%Y-%m-%d %H:%M:%S"), ". Please wait..."), duration = NULL)
  mbids <<- c(mbids, mbid)
  ###-----------###
  
  omics_all3 <- omics_data3()
  
  if (!is.null(omics_all3)) {
    X1 <- omics_all3$omicsfile1
    X2 <- omics_all3$omicsfile2
    X3 <- omics_all3$omicsfile3
    
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
    cor_omics = cor(merged_df$omics1, merged_df$omics2)
    print("a")
    print(cor_omics)
    
    # Fit a linear regression model
    model <- lm(omics3 ~ omics1 + omics2, data = merged_df)
    
    # Extract parameter estimates
    parameters <- coef(model)[-1]  # Exclude the intercept
    
    # Extract 95% confidence intervals
    conf_intervals <- confint(model)[-1, ]  # Exclude the intercept row
    
    estimates_score(list(parameters = parameters, conf_intervals = conf_intervals, cor_omics = cor_omics))
  }
  
})

observeEvent(input$tune_btn, {
  
  ###-----------###
  # Record the start time of tuning process
  start_time <- Sys.time()
  # Show notification that tuning process has started
  mbid = showNotification(paste("Tuning process has started at", format(start_time, "%Y-%m-%d %H:%M:%S"), ". Please wait..."), duration = NULL)
  mbids <<- c(mbids, mbid)
  ###-----------###
  
  omics_all3 <- omics_data3()
  
  if (!is.null(omics_all3)) {
    X1 <- scale(t(omics_all3$omicsfile1), center = TRUE, scale = TRUE)
    X2 <- scale(t(omics_all3$omicsfile2), center = TRUE, scale = TRUE)
    X3 <- scale(t(omics_all3$omicsfile3), center = TRUE, scale = TRUE)
    
    X1[is.na(X1) | is.infinite(X1)] <- 0
    X2[is.na(X2) | is.infinite(X2)] <- 0
    X3[is.na(X3) | is.infinite(X3)] <- 0
    
    #folds_num <- min(10, nrow(X1))
    
    # basic model
    components <- input$component
    
    pls1 <- spls(X1, X3, ncomp = components, mode = 'regression')
    pls2 <- spls(X2, X3, ncomp = components, mode = 'regression')
    
    perf.pls1 <- perf(pls1, validation = 'loo') # repeated CV tuning of component count
    
    perf.pls2 <- perf(pls2, validation = 'loo')
    #perf.pls1 <- perf(pls1, validation = 'Mfold',
              #folds = folds_num, progressBar = FALSE, nrepeat = 5) # repeated CV tuning of component count
    
    #perf.pls2 <- perf(pls2, validation = 'Mfold',
                      #folds = folds_num, progressBar = FALSE, nrepeat = 5)
    
    # latent component 
    l1 = plot(perf.pls1, criterion = 'Q2.total', plot = FALSE)
    l2 = plot(perf.pls2, criterion = 'Q2.total', plot = FALSE)
    
    latent_q2 <- data.frame(
      l1_comp = l1$data$comp,
      l1_mean = l1$data$mean,
      l2_mean = l2$data$mean
    )
    
    latent_score(latent_q2)
    
  } else {
    
    #for two omics data
    omics_all <- omics_data()
    
    X1 <- scale(t(omics_all$omicsfile1), center = TRUE, scale = TRUE)
    
    X3 <- scale(t(omics_all$omicsfile2), center = TRUE, scale = TRUE)
    
    X1[is.na(X1) | is.infinite(X1)] <- 0
    X3[is.na(X3) | is.infinite(X3)] <- 0
    
    #folds_num <- min(10, nrow(X1))
    
    components <- input$component
    # basic model
    components <- input$component
    
    pls1 <- spls(X1, X3, ncomp = components, mode = 'regression')
    
    perf.pls1 <- perf(pls1, validation = 'loo') # repeated CV tuning of component count
    
    # latent component 
    l1 = plot(perf.pls1, criterion = 'Q2.total', plot = FALSE)
    
    latent_q2 <- data.frame(
      l1_comp = l1$data$comp,
      l1_mean = l1$data$mean
    )
    
    latent_score(latent_q2)
  }
  
})

observeEvent(input$run_btn, {
  
  ###-----------###
  # Record the start time of tuning process
  start_time <- Sys.time()
  # Show notification that tuning process has started
  mbid = showNotification(paste("Model running has started at", format(start_time, "%Y-%m-%d %H:%M:%S"), ". Please wait for result..."), duration = NULL)
  mbids <<- c(mbids, mbid)
  ###-----------###
  
  omics_all3 <- omics_data3()
  
  if (!is.null(omics_all3)) {
    X1 <- scale(t(omics_all3$omicsfile1), center = TRUE, scale = TRUE)
    X2 <- scale(t(omics_all3$omicsfile2), center = TRUE, scale = TRUE)
    X3 <- scale(t(omics_all3$omicsfile3), center = TRUE, scale = TRUE)
    
    X1[is.na(X1) | is.infinite(X1)] <- 0
    X2[is.na(X2) | is.infinite(X2)] <- 0
    X3[is.na(X3) | is.infinite(X3)] <- 0
    
    X <- list(omics1 = X1, omics2 = X2)
    
    folds_num <- min(10, nrow(X1))
    print(folds_num)
    
    # set range of test values for number of variables to use from X dataframe
    nX1 = ncol(X1)
    nX2 = ncol(X2)
    nX3 = ncol(X3)
    
    mb_components <- input$mb_component
    
    list.keepX1 <- c(seq(10, nX1, 5))
    list.keepX2 <- c(seq(10, nX2, 5))
    list.keepX3 <- c(seq(10, nX3, 5))
    
    set.seed(123)
    tune.spls1 <- tune.spls(X1, X3, ncomp = mb_components,
                            test.keepX = list.keepX1,
                            test.keepY = list.keepX3,
                            validation = 'loo',
                            #nrepeat = 5, folds = folds_num,
                            mode = 'regression', measure = 'cor')
    
    set.seed(12345)
    tune.spls2 <- tune.spls(X2, X3, ncomp = mb_components,
                            test.keepX = list.keepX2,
                            test.keepY = list.keepX3,
                            validation = 'loo',
                            #nrepeat = 5, folds = folds_num,
                            mode = 'regression', measure = 'cor')
    
    
    a1 = (as.numeric(tune.spls1$choice.keepX))[1]
    a2 = (as.numeric(tune.spls2$choice.keepX))[1]
    a3 = (as.numeric(tune.spls2$choice.keepY))[1]
    
    list.keepX = list(omics1 = rep(a1, mb_components), omics2 = rep(a2, mb_components))
    list.keepY <- c(rep(a3, mb_components))
    
    final.mbspls.model = block.spls(X = X, Y = X3,
                                    ncomp =mb_components, keepX = list.keepX,
                                    keepY = list.keepY, design = 'full')
    
    tra = as.data.frame(final.mbspls.model$loadings$omics1)
    pro = as.data.frame(final.mbspls.model$loadings$omics2)
    met = as.data.frame(final.mbspls.model$loadings$Y)
    
    #####
    list.keepX_T = list(omics1 = rep(a1, 2), omics2 = rep(a2,2))
    list.keepY <- c(rep(a3,2))
    
    final.mbspls.model_T = block.spls(X = X, Y = X3,
                                      ncomp =2, keepX = list.keepX_T,
                                      keepY = list.keepY, design = 'full')
    multi_block = plotVar(final.mbspls.model_T, var.names = TRUE,
                          legend = TRUE, cutoff = 0.5,
                          pch = c(0,1,2), plot = FALSE)
    
    multi_block <- data.frame(X = rownames(multi_block), multi_block, row.names = NULL)
    
    
    #####
    tra = subset(tra, !(comp1 == 0))
    
    tra <- data.frame(X = rownames(tra), tra, row.names = NULL)
    
    pro = subset(pro, !(comp1 == 0))
    
    pro <- data.frame(X = rownames(pro), pro, row.names = NULL)
    
    met = subset(met, !(comp1 == 0))
    
    met <- data.frame(X = rownames(met), met, row.names = NULL)
    
    
    #####
    tra_matrix <- as.data.frame(t(X1))
    
    tra_matrix <- data.frame(X = rownames(tra_matrix), tra_matrix, row.names = NULL)
    
    
    pro_matrix <- as.data.frame(t(X2))
    
    pro_matrix <- data.frame(X = rownames(pro_matrix), pro_matrix, row.names = NULL)
    
    
    met_matrix <- as.data.frame(t(X3))
    
    met_matrix <- data.frame(X = rownames(met_matrix), met_matrix, row.names = NULL)
    
    
    #####
    tra_multi_block <- merge(tra, tra_matrix, by = "X")
    pro_multi_block <- merge(pro, pro_matrix, by = "X")
    met_multi_block <- merge(met, met_matrix, by = "X")
    
    #####
    tra_exp<- merge(tra_multi_block, multi_block, by = "X")
    pro_exp<- merge(pro_multi_block, multi_block, by = "X")
    met_exp<- merge(met_multi_block, multi_block, by = "X")
    
    #####
    # Combine data frames using rbind()
    combined_exp <- rbind(tra_exp, pro_exp, met_exp)
    
    # columns_to_exclude
    component_names <- list()
    # Generate component names dynamically and add them to the list
    for (i in 1:mb_components) {
      component_names[[i]] <- paste0("comp", i)
    }
    # Select additional columns to exclude
    additional_cols <- c("X", "x", "y", "pch", "cex", "col", "font", "Overlap")
    # Combine the list of component names with additional columns to exclude
    columns_to_exclude <- c("X", component_names, additional_cols)
    
    # Select rows where x is greater than 0 and y is greater than 0
    positive_cor <- subset(combined_exp, x > 0 & y > 0)
    positive_cor <- positive_cor[, -which(names(positive_cor) %in% columns_to_exclude)]
    positive_cor_melted <- reshape2::melt(positive_cor, id.vars = c("Block", "names"), variable.name = "Column", value.name = "Value")
    
    # Select rows where x is less than 0 and y is less than 0
    negative_cor <- subset(combined_exp, x < 0 & y < 0)
    negative_cor <- negative_cor[, -which(names(negative_cor) %in% columns_to_exclude)]
    negative_cor_melted <- reshape2::melt(negative_cor, id.vars = c("Block", "names"), variable.name = "Column", value.name = "Value")
    
    cor_data(list(positive = positive_cor_melted, negative = negative_cor_melted, combined = combined_exp))
    
  } else {
    
    #for two omics data
    omics_all <- omics_data()
    
    X1 <- scale(t(omics_all$omicsfile1), center = TRUE, scale = TRUE)
    
    X3 <- scale(t(omics_all$omicsfile2), center = TRUE, scale = TRUE)
    
    X <-  X1
    
    mb_components <- input$mb_component
    
    nX1 = ncol(X1)
    nX3 = ncol(X3)
    
    # set range of test values for number of variables to use from X dataframe
    list.keepX1 <- c(seq(10, nX1, 5))
    list.keepX3 <- c(seq(10, nX3, 5))
    
    set.seed(123)
    tune.spls1 <- tune.spls(X1, X3, ncomp = components,
                            test.keepX = list.keepX1,
                            test.keepY = list.keepX3,
                            nrepeat = 5, folds = 6,
                            mode = 'regression', measure = 'cor')
    
    
    a1 = (as.numeric(tune.spls1$choice.keepX))[1]
    a3 = (as.numeric(tune.spls1$choice.keepY))[1]
    
    list.keepX = c(rep(a1, mb_components))
    list.keepY <- c(rep(a3, mb_components))
    
    final.mbspls.model = spls(X = X, Y = X3,
                              ncomp =mb_components, keepX = list.keepX,
                              keepY = list.keepY)
    
    tra = as.data.frame(final.mbspls.model$loadings$X)
    
    met = as.data.frame(final.mbspls.model$loadings$Y)
    
    #####
    list.keepX_T = c(rep(a1, 2))
    list.keepY <- c(rep(a3,2))
    
    final.mbspls.model_T = spls(X = X, Y = X3,
                                ncomp =2, keepX = list.keepX_T,
                                keepY = list.keepY)
    
    multi_block = plotVar(final.mbspls.model_T, var.names = TRUE,
                          legend = TRUE, cutoff = 0.5, plot = FALSE)
    
    multi_block <- data.frame(X = rownames(multi_block), multi_block, row.names = NULL)
    
    #####
    tra = subset(tra, !(comp1 == 0))
    
    tra <- data.frame(X = rownames(tra), tra, row.names = NULL)
    
    met = subset(met, !(comp1 == 0))
    
    met <- data.frame(X = rownames(met), met, row.names = NULL)
    
    #####
    tra_matrix <- as.data.frame(t(X1))
    
    tra_matrix <- data.frame(X = rownames(tra_matrix), tra_matrix, row.names = NULL)
    
    met_matrix <- as.data.frame(t(X3))
    
    met_matrix <- data.frame(X = rownames(met_matrix), met_matrix, row.names = NULL)
    
    #####
    tra_multi_block <- merge(tra, tra_matrix, by = "X")
    
    met_multi_block <- merge(met, met_matrix, by = "X")
    
    #####
    tra_exp<- merge(tra_multi_block, multi_block, by = "X")
    
    met_exp<- merge(met_multi_block, multi_block, by = "X")
    
    #####
    # Combine data frames using rbind()
    combined_exp <- rbind(tra_exp, met_exp)
    
    # columns_to_exclude
    component_names <- list()
    # Generate component names dynamically and add them to the list
    for (i in 1:mb_components) {
      component_names[[i]] <- paste0("comp", i)
    }
    # Select additional columns to exclude
    additional_cols <- c("X", "x", "y", "pch", "cex", "col", "font", "Overlap")
    # Combine the list of component names with additional columns to exclude
    columns_to_exclude <- c("X", component_names, additional_cols)
    
    # Select rows where x is greater than 0 and y is greater than 0
    positive_cor <- subset(combined_exp, x > 0 & y > 0)
    positive_cor <- positive_cor[, -which(names(positive_cor) %in% columns_to_exclude)]
    positive_cor_melted <- reshape2::melt(positive_cor, id.vars = c("Block", "names"), variable.name = "Column", value.name = "Value")
    
    # Select rows where x is less than 0 and y is less than 0
    negative_cor <- subset(combined_exp, x < 0 & y < 0)
    negative_cor <- negative_cor[, -which(names(negative_cor) %in% columns_to_exclude)]
    negative_cor_melted <- reshape2::melt(negative_cor, id.vars = c("Block", "names"), variable.name = "Column", value.name = "Value")
    
    cor_data(list(positive = positive_cor_melted, negative = negative_cor_melted, combined = combined_exp))
  }
})

output$error_notification <- renderUI({
  cor_data_all <- cor_data()
  if (is.null(cor_data_all)) {
    tagList(
      div("Error: No data available for plotting"),
      br(),
      actionButton("reload_data", "Reload Data", icon = icon("refresh"))
    )
  }
})


output$linearityPlot <- renderPlotly({
  
  # Remove the oldest notification from the queue
  if (length(mbids) > 0) {
    removeNotification(mbids[1])
    mbids <<- mbids[-1]
  }
  
  # Data
  estimates_score <- estimates_score()
  parameters <- estimates_score$parameters
  conf_intervals <- estimates_score$conf_intervals
  cor_omics <- estimates_score$cor_omics
  
  # Combine parameter estimates and confidence intervals
  estimates_and_intervals <- cbind(parameters, conf_intervals)
  
  # Check if estimates_and_intervals is not NULL and has at least one row
  if (!is.null(estimates_and_intervals) && nrow(estimates_and_intervals) > 0) {
    
    # Create a plotly barplot with error bars
    plot <- plot_ly(x = names(parameters), y = parameters, type = 'bar', name = 'Estimates', 
                    error_y = list(type = 'data', symmetric = FALSE,
                                   array = conf_intervals[, "97.5 %"] - parameters,
                                   arrayminus = parameters - conf_intervals[, "2.5 %"])) %>%
      layout(title = "Parameter Estimates with 95% Confidence Intervals",
             yaxis = list(title = "Estimates"))
    
    # Create text box annotation for R-squared
    annotation <- list(
      x = 0.5,  # Adjust the position of the box as needed
      y = 1,
      text = paste("Correlation:", round(cor_omics, digits = 2)),
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
        div("Error: No or invalid data available for plotting"),
        br(),
        actionButton("reload_data", "Reload Data", icon = icon("refresh"))
      )
    })
    return(NULL)
  }
})


output$linePlot <- renderPlotly({
  
  # Remove the oldest notification from the queue
  if (length(mbids) > 0) {
    removeNotification(mbids[1])
    mbids <<- mbids[-1]
  }
  
  # Data
  latent_score <- latent_score()
  
  if (!is.null(latent_score)) {
    # Create line plot
    plot_ly() %>%
      add_trace(x = ~latent_score$l1_comp, y = ~latent_score$l1_mean, type = 'scatter', mode = 'lines', name = 'omics1&omics3') %>%
      add_trace(x = ~latent_score$l1_comp, y = ~latent_score$l2_mean, type = 'scatter', mode = 'lines', name = 'omics2&omics3') %>%
      layout(title = "Tuning the number of components", xaxis = list(title = "Latent component"), yaxis = list(title = "Q2_Mean"))
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

output$cor_plot <- renderPlotly({
  
  # Remove the oldest notification from the queue
  if (length(mbids) > 0) {
    removeNotification(mbids[1])
    mbids <<- mbids[-1]
  }
  
  cor_data_all <- cor_data()
  if (!is.null(cor_data_all)) {
    combined <- cor_data_all$combined
    
    # Create a scatter plot
    gg <- ggplot(combined, aes(x = x, y = y, color = Block, label = names)) +
      geom_text(size = 3) +
      labs(title = "Correlation Plot", x = "Component 1", y = "Component 2") +
      scale_color_manual(values = c("blue", "red", "orange")) +
      theme_minimal() +
      theme(axis.title = element_text(size = 11),  # Increase x-axis label size
            axis.text = element_text(size = 11)) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_y_continuous(limits = c(-1, 1))
    
    p <- ggplotly(gg)
    return(p)
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

output$positive_plot <- renderPlotly({
  cor_data_all <- cor_data()
  if (!is.null(cor_data_all)) {
    positive <- cor_data_all$positive
    
    ggplot(positive, aes(x = Column, y = Value, color = Block, group = names)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(x = "Condition", y = "Scaled expression values", title = "Positively correlated") +
      theme_minimal() +
      scale_y_continuous(breaks = pretty(range(-2, 2), n = 11),
                         labels = pretty(range(-2, 2), n = 11)) +
      scale_color_manual(values = c("blue", "red", "orange")) +
      theme(axis.title = element_text(size = 11),
            axis.text = element_text(size = 11))
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

output$negative_plot <- renderPlotly({
  cor_data_all <- cor_data()
  if (!is.null(cor_data_all)) {
    negative <- cor_data_all$negative
    
    # Assuming df is your dataframe and column_name is the name of the column
    
    ggplot(negative, aes(x = Column, y = Value, color = Block, group = names)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(x = "Condition", y = "Scaled expression values", title = "Negatively correlated") +
      theme_minimal() +
      scale_y_continuous(breaks = pretty(range(-2, 2), n = 11),
                         labels = pretty(range(-2, 2), n = 11)) +
      scale_color_manual(values = c("blue", "red", "orange")) +
      theme(axis.title = element_text(size = 11),
            axis.text = element_text(size = 11))
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