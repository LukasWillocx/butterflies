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
                   column(12, div(class = "custom-panel",
                                 h3('Prediction'),
                                 hr(),
                                 withSpinner(uiOutput("prediction")),
                   )),
                   ),
                 ),
        tabPanel('Model training',
                 tabsetPanel(
                 tabPanel('html',tags$iframe(src = "Butterflies_model_development.html")),
                 tabPanel('markdown',includeMarkdown("www/Butterflies_model_development.qmd")),
        )),
        tabPanel('Training data distribution',
                 fluidRow(
                   column(12, div(class = "custom-panel",
                                  withSpinner(plotlyOutput("distribution",height='1200px')),
                   )),
                 ),
                 )
        ),
      )
    )
)


server <- function(input, output) {
  
  
  labels<-read.csv('Training_set.csv')$label
  label_freq<-data.frame(table(labels))

  
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
   
   output$distribution<- renderPlotly({
     p<-ggplot(label_freq,aes(x=labels,y=Freq))+
       geom_bar(stat='identity',aes(fill=Freq),color = "transparent",width=0.5)+
       coord_flip()+  # Flip coordinates if you prefer horizontal bars
       theme(
         plot.background = element_rect(fill = "transparent", colour = NA),
         panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.major.x = element_line(color='#CEDECE',linetype=4),
         panel.grid.minor.x = element_line(color='#CEDECE',linetype=4),
         legend.position='none',
         axis.title = element_blank(),
         axis.text = element_text(color = "#7aa6a1",hjust = -1),
         axis.ticks = element_blank(),
       )+
       labs(title=NULL)
     ggplotly(p,tooltip = c('labels','y'))
   })
   
}

shinyApp(ui = ui, server = server)