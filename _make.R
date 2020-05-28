#' Run Entire Project
#'
#' See `DESCRIPTION` file for further informations.
#'
#' @usage source("_make.R")
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @author Nicolas MOUQUET, \email{nicolas.mouquet@@fondationbiodiversite.fr}
#' 
#' @date 2020/05/28



## Install Missing Packages (listed in DESCRIPTION) ----

remotes::install_deps()


## Attach Required Packages (listed in `Depends` section in DESCRIPTION) ----

devtools::load_all()


## Parameters ----

path_out <- file.path("data", "images", "scaled")            ###

column_picture <- "picture"                                  ###
column_height  <- "height"                                   ###


## Import Heights ----

heights <- read.csv(
  file = here::here("data", "heights.csv"),                  ###
  stringsAsFactors = FALSE
)       


## Get Pictures Path ----

images_paths <- list.files(
  path       = here::here("data", "images", "originals"),    ###
  full.names = TRUE
)


## Get Pictures Names ----

images_names <- strsplit(images_paths, .Platform$file.sep)
images_names <- unlist(lapply(images_names, function(x) x[length(x)]))
images_names <- gsub("\\.jpg$|\\.JPG$", "", images_names)


for (i in 1:length(images_paths)) {
  
  ## Get Specific Height ----
  
  height <- heights[heights[ , column_picture] == images_names[i], column_height]
  
  ##  Add Rule ----
  
  add_rule(images_paths[i], height = height, path = path_out)  
}

