library(shiny)
library(shinythemes)
library(keras) #ensure prior installation of virtual environment
library(imager)

loaded_model<-load_model_hdf5('butterflies_mobilenet_v2.h5')
classes<-read.csv('classes.txt') # relates model output index to context
source('functions.R')

ui <- fluidPage(
  includeCSS("app_styles.css"),
  tags$script(HTML("
    function toggleTheme() {
      const body = document.body;
      body.dataset.theme = body.dataset.theme === 'dark' ? 'light' : 'dark';
    }
  ")),
  
  titlePanel(
    div("Title", 
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$button(
          id = "themeToggle",
          onclick = "toggleTheme()",
          "🌓",
          class = "theme-toggle-btn"
        )
    ), 
    windowTitle = "window title"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Navigation"),
      fileInput('image', 'Upload Image',
                accept = c('image/png', 'image/jpeg', 'image/jpg')),
      hr(),
      p("")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Overview",
                 fluidRow(
                   column(12, div(class = "custom-panel",
                                 h3("panel content"),
                                 textOutput('prediction'),
                                 plotOutput('image')
                                 )),
                 ),
                 )
        ),
      )
    )
)


server <- function(input, output) {
  
   output$prediction <- renderText({
      req(input$image)
      unlist(predict_image(loaded_model,classes,input$image$datapath))[1]
   })
   
   output$image <- renderPlot({
     req(input$image)
     img <- load.image(input$image$datapath)
     class(img)
     plot(as.raster(img))
   })
}

shinyApp(ui = ui, server = server)