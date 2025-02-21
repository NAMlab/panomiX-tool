# Define xgb_train as a reactive value
normalized <- reactiveVal(NULL)
ftir_plot  <- reactiveVal(NULL)

nor_loadedDataset <- reactive({
  nor_inFile1 <- input$nor_file1
  nor_datasetSelected <- input$nor_choosedataset
  
  if (nor_datasetSelected == "Upload-data") {
    if (is.null(nor_inFile1))
      # Add a notification to add data
      showModal(modalDialog(
        title = "No Data Selected",
        "Please select a dataset or upload your own data file.",
        easyClose = TRUE
      ))
    else
      return(read.csv(nor_inFile1$datapath, row.names = 1, check.names = FALSE))
  } else if (nor_datasetSelected == "Example-data") {
    data(breast.TCGA)
    X1 <- as.data.frame(t(breast.TCGA$data.train$mirna)[1:150, c(1,2,3,141,142,143)])
    # rename column
    colnames(X1) <- c("Condition.A1", "Condition.A2", "Condition.A3", "Condition.B1", "Condition.B2", "Condition.B3")
    return(X1)
  }
})

observeEvent(input$tra_btn, {
  nor_all <- nor_loadedDataset()
  if (!is.null(nor_all)) {
    transform_data <- function(data) {
      # Taking log scale
      df <- log(data)
      # Calculate the row-wise average
      df$Average <- rowMeans(df)
      # Remove rows with Average values being infinity
      df_filtered <- df[is.finite(df$Average), ]
      # Subtract each element from the corresponding Average in each row
      df_subtracted <- df_filtered[, -ncol(df_filtered)] - df_filtered$Average
      # Calculate the median of each column
      medians <- apply(df_subtracted, 2, median)
      # Convert the log-transformed values back to the original scale
      median_original <- exp(medians)
      # Divide each column by its corresponding median for each row
      df_scaled <- sweep(data, 2, median_original, FUN = "/")
      return(df_scaled)
    }
    # normalization
    normalized(transform_data(nor_all))
  }
})

observeEvent(input$ftir_btn, {
  nor_all <- nor_loadedDataset()
  if (!is.null(nor_all)) {
    data <- nor_all
    # Add row names as a new column
    data$new_column <- rownames(data)
    # Optionally, reset row names to default numeric indices
    rownames(data) <- NULL
    names(data)[names(data) == "new_column"] <- "Wavenumber"
    data[] <- lapply(data, as.numeric)
    # Extract sample names for correct labeling after transposition
    sample_names <- colnames(data)[colnames(data) != "Wavenumber"]
    # Reshape data for ggplot
    data_melt <- melt(data, id.vars = "Wavenumber")
    
    # Transpose data for baseline correction (baseline package expects rows as spectra)
    absorbance_data <- as.matrix(t(data[, colnames(data) != "Wavenumber"]))
    
    # Perform baseline correction using modpolyfit (polynomial fitting)
    baseline_corrected <- baseline(absorbance_data, method = "modpolyfit", degree = 2)
    
    # Extract corrected spectra
    corrected_spectra <- getCorrected(baseline_corrected)
    
    # Transpose back to original format
    corrected_data <- data.frame(Wavenumber = data$Wavenumber, t(corrected_spectra))
    colnames(corrected_data) <- c("Wavenumber", sample_names)  # Set column names with sample names
    
    # Apply Savitzky-Golay smoothing to each corrected spectrum
    window_size <- 5  # Set window size (must be odd)
    poly_degree <- 2  # Polynomial degree
    
    # Smoothing each column (sample) except the Wavenumber
    smoothed_data <- corrected_data
    for (sample in sample_names) {
      smoothed_data[[sample]] <- sgolayfilt(corrected_data[[sample]], p = poly_degree, n = window_size)
    }
    
    # Reshape smoothed data for ggplot
    smoothed_melt <- melt(smoothed_data, id.vars = "Wavenumber")
    
    # Set 'ID' column as row names
    rownames(smoothed_data) <- smoothed_data$Wavenumber
    
    # Optionally remove the column if you don't need it anymore
    smoothed_data$Wavenumber <- NULL
    
    normalized(smoothed_data)
    ftir_plot(list(melt = data_melt, smoothed = smoothed_melt))
  }
})

# Generate boxplot for original data
output$original_boxplot <- renderPlot({
  nor_data_head = nor_loadedDataset()
  rownames(nor_data_head) <- NULL
  type <- input$type
  # Check if data is available
  if (is.null(nor_data_head)) {
    return(NULL)
  }
  if (type == "Abundance_Plot") {
    num_cols <- ncol(nor_data_head)
    color_palette <- brewer.pal(min(12, num_cols), "Set3")
    
    boxplot(nor_data_head, main = "Example Data", col = color_palette)
  }
})

output$table <- renderDT({
  nor_data_head = nor_loadedDataset()
  type <- input$type
  if (type == "Table") {
    head(nor_data_head, n = 5)
  }
})

output$ftir_raw_plot <- renderPlotly({
  ftir_all <- ftir_plot()
  if (!is.null(ftir_all)) {
    data_melt <- ftir_all$melt
    
    # Create scatter plot using ggplot
    gg_plot <- ggplot(data_melt, aes(x = Wavenumber, y = value, color = variable)) +
      geom_line(linewidth = 1) +
      labs(title = "FTIR Spectra for Different Samples",
           x = "Wavenumber (cm⁻¹)",
           y = "Absorbance") +
      theme_minimal() +
      guides(color = "none")  # Removes only the color legend
    
    # Convert ggplot to plotly
    plot <- ggplotly(gg_plot)
    return(plot)
    
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

output$ftir_smooth_plot <- renderPlotly({
  ftir_all <- ftir_plot()
  if (!is.null(ftir_all)) {
    smoothed_melt <- ftir_all$smoothed
    
    # Create scatter plot using ggplot
    gg_plot <- ggplot(smoothed_melt, aes(x = Wavenumber, y = value, color = variable)) +
      geom_line(linewidth = 1) +
      labs(title = "Baseline_corrected and Smoothed FTIR Spectra for Different Samples",
           x = "Wavenumber (cm⁻¹)",
           y = "Absorbance") +
      theme_minimal() +
      guides(color = "none")  # Removes only the color legend
    
    # Convert ggplot to plotly
    plot <- ggplotly(gg_plot)
    return(plot)
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

output$norm_download <- renderUI({
  norm_data <- normalized()
  if (!is.null(norm_data)) {
    downloadButton("n_data","Normalized_counts")
  }
})

output$n_data <- downloadHandler(
  filename = function () {
    paste("normalized_counts.csv")
  },
  content = function(file) {
    norm_data <- normalized()
    write.csv(norm_data, file)
  }
)