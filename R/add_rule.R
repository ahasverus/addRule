#' Add a Rule to a JPG Picture
#'
#' This function adds a rule to a JPG picture and a marker of the feature height
#' 
#' @param image (string) Path to the picture
#' @param height (numeric) Real height of the feature
#' @param min (numeric) Minimum value of the rule
#' @param max (numeric) Maximum value of the rule 
#' @param path (string) Folder to store picture
#'
#' @export
#' @importFrom jpeg readJPEG
#' @importFrom graphics par plot rasterImage lines rect text polygon
#' @importFrom grDevices jpeg
#'
#' @examples
#' ##


add_rule <- function(image, height, min = 0, max = 140, path) {
  
  
  ## Arguments Checks ----
  
  if (missing(image)) {
    stop("Argument 'image' is required.")
  }
  
  if (length(image) != 1 || !is.character(image)) {
    stop("Argument 'image' must be a filename (string) of length 1.")
  }
  
  if (missing(height)) {
    stop("Argument 'height' is required.")
  }
  
  if (length(height) != 1 || !is.numeric(height)) {
    stop("Argument 'height' must be a numeric of length 1.")
  }
  
  image_path <- image
  
  
  ## Image Name ----
  
  image_name <- strsplit(image, .Platform$file.sep)[[1]]
  image_name <- image_name[length(image_name)]
  image_name <- gsub("\\.jpg$|\\.JPG$", "", image_name)
  
  
  ## Import JPG ----
  
  jpg <- jpeg::readJPEG(image)
  
  
  ## Init Graphical Device ----
  
  jpeg(
    filename  = file.path(path, paste0(image_name, "_scale.jpg")),
    width     = dim(jpg)[1], 
    height    = dim(jpg)[2], 
    units     = "px", 
    res       = 96, 
    pointsize = 24
  )
  
  ## Set Graphical Parameters ----
  
  par(mar = rep(0, 4), xaxs = "i", yaxs = "i", family = "serif")
  
  
  ## Empty Plot (for dimensions) ----
  
  plot(0, type = "n", bty = "n", ann = FALSE, axes = FALSE,
       xlim = c(1, dim(jpg)[1]), ylim = c(1, dim(jpg)[2]), asp = 1)
  
  
  ## Add Original JPG ----
  
  rasterImage(jpg, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4])
  
  
  ## Rule Coordinates ----
  
  margins <- c(x = 50, y = 100)
  width   <- 0.085 * (par()$usr[2] - par()$usr[1])
  
  xleft   <- par()$usr[1] + margins["x"]
  ybottom <- par()$usr[3] + margins["y"]
  xright  <- par()$usr[1] + margins["x"] + width
  ytop    <- par()$usr[4] - margins["y"]
  
  
  ## Rule Background ----
  
  rect(xleft, ybottom, xright, ytop, col = "#e7dc73", border  = "black")
  
  
  ## Rule Major Ticks ----
  
  major_ticks <- seq(min + 10, max - 10, by = 10)
  
  for (tick in major_ticks) {
    
    lines(
      x   = c(xleft, xleft + width / 2.8), 
      y   = rep(ybottom + (ytop - ybottom) * tick / max, 2), 
      lwd = 1.2
    )
    
    text(
      x      = xright, 
      y      = ybottom + (ytop - ybottom) * tick / max, 
      labels = tick, 
      pos    = 2, 
      cex    = 0.85
    )
  }
  
  
  ## Rule Minor Ticks ----
  
  minor_ticks <- seq(min + 1, max - 1, by = 1)
  minor_ticks <- minor_ticks[!(minor_ticks %in% major_ticks)]
  
  for (tick in minor_ticks) {
  
    lines(
      x = c(xleft, xleft + width / 5), 
      y = rep(ybottom + (ytop - ybottom) * tick / max, 2)
    )
  }
  
  
  ## Rule Title ----
  
  text(
    x      = xleft + (xright - xleft) / 2, 
    y      = ytop, 
    labels = "cm", 
    pos    = 1, 
    cex    = 0.95, 
    col    = "#ff0000dd", 
    font   = 2
  )
  
  
  ## Height Marker (triangle) ----
  
  height <- ybottom + (ytop - ybottom) * height / max
  
  polygon(
    x      = c(xright, xright + 30, xright + 30),
    y      = c(height, height + 20, height - 20),
    col    = "black",
    border = NA
  )
  
  dev.off()
}
