library(shiny)
library(DT)

options(shiny.maxRequestSize=30*1024^2)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Visualization of Lorica output"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Type of plot", 
                  choices = c("depth plot", "profile development")),
      # fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var_selector"),
      uiOutput("row_slider"),
      # uiOutput("col_slider")
    ),
    mainPanel(plotOutput = "plot")
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    req(test_soils)  # Use the object from the environment
    test_soils
  })
  
  
  # Dynamically generate variable selector
  output$var_selector <- renderUI({
    req(data())
    selectInput("var", "Select Variable to display", choices = names(data()))
  })
  
  # Dynamically generate slider based on selected variable's range
  output$row_slider <- renderUI({
    req(data())
    var_values <- data()[["row"]]
    
    # Ensure the selected variable is numeric
    if (!is.numeric(var_values)) {
      return("Please select a numeric variable.")
    }
    sliderInput("row", "Select row", 
                min = min(var_values, na.rm = TRUE), 
                max = max(var_values, na.rm = TRUE),
                value = 0,
                step = 1)
  })
  
  
  # Render the plot based on the selected data
  output$plot <- renderPlot({
    req(data(), input$var, input$row)
    
    ggplot(data() %>% filter(row == input$row, col == input$row, t == 1200), aes_string(x = input$var, y = "midthick_m")) +
      geom_path() + 
      scale_y_reverse() + 
      theme_bw()
    
  })
}



#   
#   # Sidebar with a slider to control the value of t
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("t_value", 
#                   "Select t Value:", 
#                   min = 100, 
#                   max = 1200, 
#                   value = 100,  # Initial value set to 100
#                   step = 100,
#                   animate = TRUE)
#     ),
#     
#     # Main panel to display the plot
#     mainPanel(
#       plotOutput("plot")
#     )
#   )
# )

# Define server logic
# server <- function(input, output) {
#   
#   # Reactive expression to subset the data based on the selected t
#   selected_data <- reactive({
#     tab_out_15[tab_out_15$t == input$t_value, c("col", "z", "nlayer")]
#   })
#   
#   # Render the plot based on the selected data
#   output$plot <- renderPlot({
#     data <- selected_data()
#     
#     # Plot col vs z for the selected t, differentiating nlayer 0 and 14
#     ggplot(data, aes(x = col, y = z, color = factor(nlayer))) +
#       geom_line() +  # Creates a continuous line plot
#       labs(title = paste("Col vs Z for t =", input$t_value),
#            x = "Column",
#            y = "Elevation",
#            color = "Nlayer") +  # Add legend for nlayer
#       scale_color_manual(values = c("0" = "tan4", "14" = "slategrey")) +  # Different colors for nlayer 0 and 14
#       theme_minimal()
#   })
# }


#should fix the y axes
# Run the application
shinyApp(ui = ui, server = server)
