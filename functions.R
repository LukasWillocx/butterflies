
# function to provide single image inference 
predict_image <- function(model,classes,image_path) {
  img <- image_load(image_path, target_size = c(224, 224))
  x <- image_to_array(img)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x / 255  # Match training preprocessing
  pred <- predict(model, x)
  class_id <- which.max(pred)
  return(list(class = classes$Name[class_id],
              scientific_name = classes$Scientific.Name[class_id],
              family = classes$Family[class_id],
              location = classes$Location[class_id],
              size_in_mm = classes$Size..mm.[class_id],
              probability = max(pred)))
}

# function to depict the distribution of the training labels
distribution_plotter<- function(labels){
  p<-ggplot(labels,aes(x=labels,y=Freq))+
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
      axis.title = element_text(color = "#7aa6a1"),
      axis.text = element_text(color = "#7aa6a1",hjust = -1),
      axis.ticks = element_blank(),
    )+
    labs(title=NULL,x='',y='Number of images')
  ggplotly(p,tooltip = c('labels','y'))
}