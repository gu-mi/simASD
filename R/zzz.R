


.onAttach = function(libname, pkgname) {
  
  packageStartupMessage("\n Welcome to simASD, an R package for performing a simple two-stage adaptive signature design (ASD) based on simulated datasets.")

  # suppress loading package messages
  suppressMessages(library(survival))
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  #
  message("\n For updates of the simASD package (licensed under GPL-3), please visit https://github.com/gu-mi/simASD \n")

}
