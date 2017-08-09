
.onLoad <- function(libname = find.package("Sentometrics"), pkgname = "Sentometrics"){

  # CRAN note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      c("value", "variable", "word_count", "w")
    )

  invisible()
}

# .onUnload <- function (libpath) {
#  library.dynam.unload("Sentometrics", libpath)
# }

