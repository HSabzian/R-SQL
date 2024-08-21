
#### Iris Dataset explorer


#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinythemes")


library(shiny)
library(ggplot2)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Iris Dataset Explorer"),
  
  # Sidebar with controls to select variables and plot types
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis variable:", 
                  choices = colnames(iris)[1:4]),
      selectInput("yvar", "Y-axis variable:", 
                  choices = colnames(iris)[1:4],
                  selected = colnames(iris)[[2]]),
      selectInput("colorvar", "Color by:", 
                  choices = colnames(iris)[5]),
      selectInput("plottype", "Plot type:", 
                  choices = list("Scatter Plot" = "scatter",
                                 "Box Plot" = "box")),
      hr(),
      p("This is a simple Shiny application to explore the Iris dataset. You can select different variables for the X and Y axes, choose a grouping variable for coloring, and switch between scatter plot and box plot.")
    ),
    
    # Show the plot
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # Conditional plotting based on user input
    if(input$plottype == "scatter") {
      p <- ggplot(iris, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
        geom_point(size = 3) +
        labs(title = paste("Scatter plot of", input$xvar, "vs", input$yvar)) +
        theme_minimal()
    } else {
      p <- ggplot(iris, aes_string(x = input$colorvar, y = input$yvar, fill = input$colorvar)) +
        geom_boxplot() +
        labs(title = paste("Box plot of", input$yvar, "by", input$colorvar)) +
        theme_minimal()
    }
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
