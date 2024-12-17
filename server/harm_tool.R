# Define xgb_train as a reactive value
normalized <- reactiveVal(NULL)

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
      return(read.csv(nor_inFile1$datapath, row.names = 1))
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
    rownames(nor_all) <- NULL
    normalized = transform_data(nor_all)
    normalized(normalized)
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
    write.csv(norm_data, file, row.names = FALSE)
  }
)