
# single image prediction
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

read_html_file <- function(file_path) {
  lines <- readLines(con = file_path, warn = FALSE)
  paste(lines, collapse = "\n")
}