packages <- c('shiny','shinythemes' ,'shinycssloaders','keras','imager')

for(pkg in packages) {
  library(pkg, character.only = TRUE)
}

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
    div("Butterfly classification model", 
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$button(
          id = "themeToggle",
          onclick = "toggleTheme()",
          "🌓",
          class = "theme-toggle-btn"
        )
    ), 
    windowTitle = "Butterfly classification"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Navigation"),
      fileInput('image', 'Upload butterfly image',
                accept = c('image/png', 'image/jpeg', 'image/jpg')),
      hr(),
      tags$div(style = "text-align: center",h3('Uploaded image')),
      withSpinner(imageOutput('image')),
      hr(),
      tags$div(style = "text-align: center",h5('Libraries')),
      hr(),
      tags$ul(style = "list-style-type: none; padding-left: 0;", 
              lapply(c(packages,'tensorflow','stringr','dplyr','reticulate'), function(lib) {
                tags$li(style = "display: inline-block; margin: 5px; text-align: center;",
                        tags$span(
                          style = "border-radius: 20%; background-color: var(--background-color); padding: 5px;",
                          tags$b(lib)))})),
      tags$ul(style = "list-style-type: none; padding-left: 0;", 
              lapply(c('numpy','Pillow','tensorflow-directml','rembg','scipy','pandas','h5py'), function(lib) {
                tags$li(style = "display: inline-block; margin: 5px; text-align: center;",
                        tags$span(
                          style = "border-radius: 20%; background-color: var(--background-color2); padding: 5px;",
                          tags$b(lib)))})),
      hr(),
      tags$div(style = "text-align: center",h5('About the application')),
      hr(),
      p("")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Model inference",
                 fluidRow(
                   column(12, div(class = "custom-panel", height = '224px',
                                 h3('Prediction'),
                                 hr(),
                                 withSpinner(uiOutput("prediction")),
                   )),
                   ),
                 )
        ),
      )
    )
)


server <- function(input, output) {
  
  output$prediction <- renderUI({
    req(input$image)
    
    # Predict the image and retrieve details
    prediction <- predict_image(loaded_model, classes, input$image$datapath)
    
    output_text <- paste(
      "<b>Probability:</b> ", sprintf("%.2f%%", prediction$probability * 100), "<br>",
      "<b>Common name:</b> ", tolower(prediction$class), "<br>",
      "<b>Scientific Name:</b> <i>", prediction$scientific_name, "</i><br>",
      "<b>Family:</b> ", prediction$family, "<br>",
      "<b>Location:</b> ", prediction$location, "<br>",
      "<b>Size (mm):</b> ", prediction$size_in_mm
    )
    HTML(output_text)  # Render the output as HTML
  })
   
   output$image <- renderPlot({
     req(input$image)
     img <- load.image(input$image$datapath)
     resized_img <- resize(img, 224, 224)
     par(mar = c(0, 0, 0, 0))
     plot(as.raster(resized_img))
   },bg='transparent')
}

shinyApp(ui = ui, server = server)