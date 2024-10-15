#' Run the famexploreR Shiny Application   
#' 
#' Launches the famexploreR app 
#' 
#' @importFrom shiny shinyApp
#' @export
launch_famexplorer <- function() {
  
  # Get app directory - don't include "inst" here as doesn't exist when you install the package from github
  appDir <- base::system.file("famexploreRapp", package = "famexploreR")
  
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `famexploreR`.", call. = FALSE)
  }
  
  if (!dir.exists(appDir)) {
    stop(paste("Error: The application directory does not exist:", appDir))
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Error: The 'shiny' package is not installed. Please install it before proceeding.")
  }
  
  # Run app
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
