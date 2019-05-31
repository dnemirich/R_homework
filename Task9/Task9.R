
library(shiny)
library(ggplot2)
library(dplyr)

data <- mtcars

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("My plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        
        textInput(inputId = 'title',
                  label = 'Plot title:',
                  placeholder = 'Enter the title'),
        
        selectInput(inputId = "x",
                     label = "X-axis",
                     choices = c(
                       "Miles per gallon" = "mpg",
                       "Displacement" = "disp",
                       "Gross horsepower" = "hp",
                       "Rear axle ratio" = "drat",
                       "Weight" = "wt",
                       "1/4 mile time" = "qsec"),
                     selected = "mpg"),
         
         selectInput(inputId = "y",
                     label = "Y-axis",
                     choices = c(
                       "Miles per gallon" = "mpg",
                       "Displacement" = "disp",
                       "Gross horsepower" = "hp",
                       "Rear axle ratio" = "drat",
                       "Weight" = "wt",
                       "1/4 mile time" = "qsec"),
                     selected = "disp"),
         
         selectInput(inputId = "color",
                     label = "Color:",
                     choices = c(
                       "Number of cylynders" = "cyl",
                       "Engine type" = "vs",
                       "Transmission" = "am",
                       "Number of forward gears" = "gear",
                       "Number of carburetors" = "carb"),
                     selected = "cyl"),
         
         sliderInput(inputId = 'alpha',
                     label = 'Alpha:',
                     min = 0, max = 1,
                     value = 0.5),
         
         numericInput(inputId = 'size',
                      label = 'Dot size:',
                      value = 3,
                      min = 1, max = 9),
        
        checkboxInput(inputId = 'show_d',
                      label = 'Show data?',
                      value = FALSE),
        
        checkboxInput(inputId = 'show_s',
                      label = 'Show summary?',
                      value = FALSE),
        
        actionButton('appl', 'Apply'),
        actionButton('save', 'Save plot')

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "scatter"),
         
         fluidRow(
           column(width = 6,
                  tableOutput(outputId = 'data_selected')),
           column(width = 6,
                  tableOutput(outputId = 'summary'))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatter <- renderPlot({
    ggplot(data, aes_string(x = input$x, y = input$y, col=input$color)) +
      geom_point(alpha = input$alpha, size = req(input$size)) +
      ggtitle(tools::toTitleCase(isolate(input$title)))
    })
    
  output$data_selected <- renderTable({
    if (input$show_d) {
      data %>% select_(input$x, input$y, input$color)
    }
  })
  
  output$summary <- renderTable({
    if (input$show_s) {
      data %>%
        select_(input$x, input$y, input$color) %>%
        group_by_(input$color) %>% 
        summarise_all(funs(mean, sd))
    }
  })
  
  new_data <- reactive({
    data %>% select_(input$x, input$y, input$color)
  })
  
  output$data_selected <- renderTable({
    if (input$show_d) {
      new_data()
    }
  })
  
  output$summary <- renderTable({
    if (input$show_s) {
      new_data() %>%
        group_by_(input$color) %>% 
        summarise_all(funs(mean, sd))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

