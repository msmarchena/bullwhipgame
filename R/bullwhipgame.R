#' @export
bullwhipgame <- function() {
  appDir <- system.file("shiny", package = "bullwhipgame")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bullwhipgame`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}




