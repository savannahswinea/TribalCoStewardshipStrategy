# The purpose of this script is to setup the necessary packages for the other scripts to function

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore(prompt = FALSE)