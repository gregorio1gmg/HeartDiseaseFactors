# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

# Read original dataset
df <- read.csv("C:\\Users\\goyo_\\OneDrive\\MS Applied Statistics (Official)\\DATA 824 Data Visualization\\Final Project\\framingham.csv")
View(df)
df$TenYearCHD = as.factor(df$TenYearCHD)

# Select variables for density histogram
hist_df <- df %>% select(age, glucose, cigsPerDay, totChol,
                         sysBP, diaBP, BMI, heartRate, TenYearCHD)

# Select variables for box plot
box_df <- df %>% select(age, glucose, cigsPerDay, totChol,
                         sysBP, diaBP, BMI, heartRate, TenYearCHD)

# Select variables for scatter plot
scatter_df <- df %>% select(age, glucose, cigsPerDay, totChol,
                        sysBP, diaBP, BMI, heartRate)

# Select variables for correlation matrix
corr_df <- df %>% select(age, glucose, heartRate, cigsPerDay, totChol,
                         sysBP, diaBP, BMI)
corr_df <- drop_na(corr_df)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Heart Disease Factors"),
  
  # Create a tabset panel with three tabs
  tabsetPanel(
    
    # First tab - Table View
    tabPanel("Table View",
             tableOutput("view_table")
             
             
    ),
    
  # Second tab - Summary
    tabPanel("Summary Stats",
           tableOutput("summary_table")
           
           
  ),
  
    # Third tab - Histogram
    tabPanel("Density Histogram",
             sidebarLayout(
               sidebarPanel(
                 # Select variable to plot
                 selectInput("hist_var", "Select Histogram Variable:", choices = names(hist_df), selected = "age")
               ),
               mainPanel(plotOutput("hist_plot"))
             )
    ),
    
    # Fourth tab - Scatter Plot
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
                 # Select X-axis variable for scatter plot
                 selectInput("scatter_x_var", "Select X-axis:", choices = names(scatter_df), selected = "age"),
                 # Select Y-axis variable for scatter plot
                 selectInput("scatter_y_var", "Select Y-axis:", choices = names(scatter_df), selected = "sysBP")
               ),
               mainPanel(plotOutput("scatter_plot"))
             )
    ),
    
    # Fifth tab - Box Plot
    tabPanel("Box Plot",
             sidebarLayout(
               sidebarPanel(
                 # Select variable for box plot
                 selectInput("box_var", "Select Box Plot Variable:", choices = names(box_df), selected = "age")
               ),
               mainPanel(plotOutput("box_plot"))
             )
    ),
  
  # Sixth tab - Possible Covariates
  tabPanel("Correlation Plot",
             mainPanel(plotOutput("corr_plot"))
           )
  )
)




# Define server logic
server <- function(input, output) {
  
  # Full Data View
  output$view_table <- renderTable({
    view_df <- df
    view_df
  
  })
  
  # Summary Table
  output$summary_table <- renderTable({
  summary_df <- summary(df)
  summary_df
  
  })
  
  # Density Histogram
  output$hist_plot <- renderPlot({
 # Plot density histograms,categorized by TenYearCHD value
    ggplot(hist_df, aes_string(x = input$hist_var, color = "TenYearCHD", fill = "TenYearCHD")) +
      geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
      geom_density(alpha = 0.2) +
      labs(x = input$hist_var, y = "Density")
    
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    
    ggplot(scatter_df, aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
      geom_point(position = position_jitter(width = 0.5, height = 0.5), color = "#E7B800") +
      geom_smooth()
  
  })
  
  # Box plot
  output$box_plot <- renderPlot({

    ggplot(box_df, aes_string(x = "TenYearCHD", y = input$box_var, fill = "TenYearCHD")) +
      geom_boxplot()
  
  })
  
  # Correlation Plot
  output$corr_plot <- renderPlot({
    corr_matrix <- round(cor(corr_df), 2)
    ggcorrplot(corr_matrix, lab=TRUE)
    
  })
  

}

# Run the Shiny app
shinyApp(ui, server)
