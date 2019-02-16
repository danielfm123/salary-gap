#' installs non existing libraries
#'
#' It loads libraries, if the library is not installed it would install it first.
#' @param x library to use
#' @param load load packages after checking if is installed
#' @usage safeLibrary(x)
#' @author Daniel Fischer
#' @example examples\safeLibraryex.R
#' @export
safeLibrary <- function(x,load = TRUE) {
  paquetes = as.character(substitute(x))
  if (!all(paquetes %in% .packages(all.available = TRUE))) {
    if (version$os == "mingw32") {
      install.packages(paquetes, dependencies = TRUE)
    } else{
      install.packages(paquetes, dependencies = TRUE)
      # install.packages(paquetes, dependencies = TRUE, lib = "/usr/local/lib/R/site-library")
    }
  }
  if(load){
    paquetes = paste(paquetes, collapse = ",")
    string = paste("suppressPackageStartupMessages(require(",paquetes,"))", sep = "")
    eval(parse(text = string))
  }
}
