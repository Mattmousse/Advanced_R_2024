library(shiny)
library(factoextra)
library(ggplot2)
require(DT)
library(clustMixType)
library(fpc) 
library(dbscan)
library(Rtsne)

# Define UI for data upload app ----
ui <- navbarPage(title = "Page name",
                 
                 tabPanel("Dataset Infos",
                          # Input: Select a file ----
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Output: Data file ----
                          dataTableOutput('contents'),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          verbatimTextOutput("summary")
                 ),
                 
                 tabPanel("Variables Exploration",
                          tabsetPanel(
                            tabPanel("One Variable",
                                     fluidRow(
                                       column(3, 
                                              selectInput('varchoice_1.1', 'Select variable to analyze', "")
                                       ),
                                       column(6,
                                              tags$hr(),
                                              plotOutput('plot1'),
                                              tags$hr()
                                              
                                       ),
                                       column(3, dataTableOutput("summary_table"))
                                     )
                            ),
                            
                            tabPanel("Variables Interaction",
                                     fluidRow(
                                       column(3,
                                              selectInput('varchoice_1.2.1', 'Select first variable to analyze', ""),
                                              selectInput('varchoice_1.2.2', 'Select second variable to analyze', ""),
                                              radioButtons("varchoice_1.2.3",
                                                           label = "Select set of variables",
                                                           choices = c("Two variables", "Feature space", "Embeddings"),
                                                           selected = "Two variables")
                                       ),
                                       column(6, plotOutput('plot2')),
                                       column(3, "")
                                     )
                            )
                          )
                 ),
                 
                 tabPanel("Clustering & Dimensionality Reduction",
                          tabsetPanel(
                            tabPanel("Clustering",
                                     fluidRow(
                                       column(3, 
                                              selectInput('varchoice_2.1', 'Select variable to predict', ""),
                                              radioButtons("cluster_type",
                                                           label = "Which plot to use",
                                                           choices = c("K-means", "Hierarchical clustering", "DB scan"),
                                                           selected = "K-means")
                                       ),
                                       column(6, plotOutput('plot3')),
                                       column(3, tableOutput("confusion_matrix_table"), verbatimTextOutput("test"))
                                     )
                            ),
                            
                            tabPanel("Linear Dimensionality Reduction",
                                     fluidRow(
                                       column(3,
                                              radioButtons("Plot_type_varAnalysis",
                                                           label = "Which plot to use",
                                                           choices = c("Variables", "Individuals", "Variance of PCs"),
                                                           selected = "Variables")
                                       ),
                                       column(6, plotOutput('plot4')),
                                       column(3, "")
                                     )
                            ),
                            tabPanel("Non-Linear Dimensionality Reduction",
                                     fluidRow(
                                       column(3,sliderInput("slider", "Select a value:", min = 1, max = 100, value = 5)),
                                       column(6, plotOutput('plot5')),
                                       column(3, "")
                                     )
                            )
                          )
                 )
                 
)



# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  reactives <- reactiveValues(
    mydata = NULL
  )
  
  observeEvent(input$file1, {
    
    reactives$mydata <- read.csv(file = input$file1$datapath)
    updateSelectInput(session, inputId = 'varchoice_x', label = 'Select the first var', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice_y', label = 'Select the second var', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice1', label = 'Select variable to predict', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice_1.1', label = 'Select variable to analyse', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice_1.2.1', label = 'Select first variable to analyse', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice_1.2.2', label = 'Select second variable to analyse', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'varchoice_2.1', label = 'Select variable to predict', choices  = colnames(reactives$mydata)[1:39])
    
    updateSelectInput(session, inputId = 'varchoice_PCAgroup', label = 'Select variable to group by', choices  = colnames(reactives$mydata))
    updateCheckboxGroupInput(session, inputId = 'category', label = 'Does He/She has :', choices  = colnames(reactives$mydata)[1:39])
    
  })
  
  output$plot1 <- renderPlot({
    p <- ggplot(reactives$mydata, aes_string(x = input$varchoice_1.1)) +
      geom_bar(fill = "skyblue", color = "skyblue") +
      geom_density(aes(y = ..count..), color = "red",fill = "lightblue", alpha = 0.5) +
      labs(x = input$varchoice_1.1, y = "Frequency", title = "Distribution")
    
    return(p)
  })
  
  output$summary_table <- renderDataTable({
    selected_variable <- input$varchoice_1.1
    
    # Subset the selected column from the dataset
    selected_column <- reactives$mydata[, selected_variable]
    
    # Compute the statistics
    stats_summary <- summary(selected_column)
    mean_val <- mean(selected_column)
    median_val <- median(selected_column)
    quartiles <- quantile(selected_column)
    variance_val <- var(selected_column)
    max_val <- max(selected_column)
    min_val <- min(selected_column)
    
    # Create a data frame for the summary table
    summary_table <- data.frame(
      Measure = c("Mean", "Median", "1st Quartile", "3rd Quartile", "Variance", "Maximum", "Minimum"),
      Value = c(mean_val, median_val, quartiles[2], quartiles[4], variance_val, max_val, min_val)
    )
    
    # Return the table
    datatable(summary_table, options = list(paging = FALSE))
  })
  
  output$plot2 <- renderPlot({
    if (input$varchoice_1.2.3 == "Two variables"){
      x_var <- input$varchoice_1.2.1
      y_var <- input$varchoice_1.2.2
      
      p <- ggplot(reactives$mydata, aes_string(x = x_var, y = y_var)) +
        geom_point() +
        labs(x = x_var, y = y_var, title = "Scatterplot of Two Variables")
      
      print(p)  # Ensure the plot is explicitly printed
      return(p)
    }
    
    else if (input$varchoice_1.2.3 == "Feature space" || input$varchoice_1.2.3 == "Embeddings"){
      if (input$varchoice_1.2.3 == "Feature space") {
        df <- reactives$mydata[, 3:39]  # Selecting columns for feature space
      } else {
        df <- reactives$mydata[, 40:500]  # Selecting columns for embeddings
      }
      
      # Compute the correlation matrix
      correlation_matrix <- cor(df)
      
      # Convert correlation matrix to a data frame for ggplot
      correlation_df <- as.data.frame(correlation_matrix)
      correlation_df$rowname <- rownames(correlation_matrix)
      correlation_df <- reshape2::melt(correlation_df, id.vars = "rowname")
      
      # Visualize the correlation matrix as a heatmap
      p <- ggplot(correlation_df, aes(x = rowname, y = variable, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal() +
        labs(title = "Correlation Matrix Heatmap")+
        theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1))
      
      
      print(p)  # Ensure the plot is explicitly printed
      return(p)
    }
  })
  
  output$plot3 <- renderPlot({
    if(input$cluster_type == "K-means"){
      df <- reactives$mydata[, 3:500]
      # Fit K-Means clustering Model
      set.seed(240) # Setting seed 
      kmeans.re <- kmeans(df, centers = 2, nstart = 20)  # Assuming the first column is not used in clustering
      
      # Enhanced clusplot with factoextra
      clus_plot <- fviz_cluster(kmeans.re, data = df[, c("embedding_1", "embedding_2")],
                                geom = "point", stand = FALSE,
                                ellipse.type = "norm",
                                ellipse.level = 0.68,
                                main = "Cluster Visualization",
                                xlab = 'embedding_1',
                                ylab = 'embedding_2')
      clus_plot
    }
    
    else if(input$cluster_type == "Hierarchical clustering"){
      df <- reactives$mydata[, 3:500]
      # Hierarchical clustering
      hc <- hclust(dist(df), method = "ward.D2")
      
      # Visualizing the dendrogram
      fviz_dend(hc, k = 2, cex = 0.6)  # Change k for the number of clusters
    }
    
    else if(input$cluster_type == "DB scan"){
      df <- reactives$mydata[, 3:39]
      # DBSCAN clustering
      db <- dbscan(df, eps = 3, minPts = 5)
      
      # Visualizing the clusters
      fviz_cluster(db, df, geom = "point", stand = FALSE,
                   main = "DBSCAN Clustering",
                   outlier.color = "black")
      
    }
    
  })
  
  output$plot4 <- renderPlot({
    if (input$Plot_type_varAnalysis == "Variables"){
      df = reactives$mydata
      df = df[, -c(1:2)]
      # Select columns starting with 'emb' using grep
      emb_columns <- df[, !grepl("^emb", names(df))]
      res.pca <- prcomp(emb_columns, scale. = TRUE)
      fviz_pca_var(res.pca,
                   col.var = "contrib", # Color by contributions to the PC
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE     # Avoid text overlapping
      )
    }
    else if (input$Plot_type_varAnalysis == "Individuals"){
      df = reactives$mydata
      df = df[, -c(1:2)]
      # Select columns starting with 'emb' using grep
      emb_columns <- df[, !grepl("^emb", names(df))]
      res.pca <- prcomp(emb_columns, scale. = TRUE)
      fviz_pca_ind(res.pca,
                   label = 'none',
                   col.ind = "cos2", # Color by the quality of representation
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   #repel = TRUE     # Avoid text overlapping
      )
    }
    else if (input$Plot_type_varAnalysis == "Variance of PCs"){
      df = reactives$mydata
      df = df[, -c(1:2)]
      # Select columns starting with 'emb' using grep
      emb_columns <- df[, !grepl("^emb", names(df))]
      res.pca <- prcomp(emb_columns, scale. = TRUE)
      fviz_eig(res.pca)
    }
  })
  
  output$plot5 <- renderPlot({
    df = reactives$mydata
    df = df[, -c(1:2)]
    
    # Define features and labels
    features <- df[, -20]  # Exclude the species column (labels)
    labels <- df$Male
    # Apply t-SNE for dimensionality reduction
    set.seed(123)
    tsne_result <- Rtsne(features, perplexity = input$slider, dims = 2)  # Adjust perplexity as needed
    
    # Combine the t-SNE reduced features with labels
    tsne_df <- data.frame(tsne_result$Y, Species = labels)
    
    # Plot the t-SNE reduced features
    p <- ggplot(tsne_df, aes(x = X1, y = X2, color = Species)) +
      geom_point() +
      labs(title = "t-SNE Dimensionality Reduction", x = "Component 1", y = "Component 2")
    return(p)
  })
  
  output$contents <- renderDT({
    df <- reactives$mydata 
    
    return(datatable(df, options = list(pageLength = 10)))
  })

  output$summary <- renderPrint({
    req(input$file1)
    X <- read.csv(input$file1$datapath)
    get_variable_info <- function(data) {
      # Get the types of variables: binary, character, numeric
      variable_types <- sapply(data, function(column) {
        if (all(column %in% c(-1, 1))) {
          return("Binary")
        } else if (is.character(column)) {
          return("Character")
        } else if (is.numeric(column)) {
          return("Numeric")
        } else {
          return("Other")
        }
      })
      
      # Count the number of missing values
      missing_values <- sapply(data, function(column) sum(is.na(column)))
      
      # Calculate the number of rows
      num_rows <- nrow(data)
      
      # Create the table
      info_table <- data.frame(
        Variable_Type = variable_types,
        Missing_Values = missing_values
      )
      
      # Count the occurrences of each variable type
      count_table <- table(info_table$Variable_Type)
      
      # Add the number of rows to the table
      count_table <- rbind(count_table, c("Num_Rows" = num_rows))
      
      return(count_table)
    }
    
    # Replace 'X' with the name of your dataframe
    result_table <- get_variable_info(X)
    rownames(result_table) <- c("Number of variable", "length") 
    print(result_table)
  })

}

# Create Shiny app ----
shinyApp(ui, server)





















