packages <- c('shiny','shinythemes' ,'shinycssloaders','keras','imager','htmltools','bslib')

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
      tags$div(style = "text-align: center",h5('Butterfly data information & source')),
      hr(),
      HTML(paste('<strong>File type: </strong>','.jpeg','<br>')),
      HTML(paste('<strong>Image size: </strong>','224x224','<br>')),
      HTML(paste('<strong>Sample size: </strong>','6499','<br>')),
      HTML(paste('<strong>Number of species: </strong>','75','<br>')),
      HTML(paste('<strong>Source: </strong> <a href="https://www.kaggle.com/datasets/phucthaiv02/butterfly-image-classification" target="_blank">Kaggle</a>','<br>')),
      HTML(paste("<strong>Downloaded on:</strong>",'April 21st, 2025','<br>')),
      hr(),
      tags$div(style = "text-align: center",h5('About the application')),
      hr(),
      p("This application features a deep-learning exercise on the classification of 75 butterfly species. Inference can be performed
        in the main panel, provided it's an image of a species the model has been trained on. The convolutional neural network (CNN) model is constructed and trained
        using the Keras API that leverages TensorFlow in a virtual python environment. It's been established through transfer training, on top of the mobilenet_v2 CNN.
        Model training can be assessed in its respective tab in the main panel.")
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
                 ),
        tabPanel('Model Training',
                 tabsetPanel(
                 tabPanel('html',tags$iframe(src = "Butterflies_model_development.html")),
                 tabPanel('markdown',includeMarkdown("www/Butterflies_model_development.qmd")),
        ))
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
     resized_img <- imager::resize(img, 224, 224)
     par(mar = c(0, 0, 0, 0))
     plot(as.raster(resized_img))
   },bg='transparent')
   
}

shinyApp(ui = ui, server = server)