.onAttach <- function(libname, pkgname) {
  packageStartupMessage("seurathelpeR contains scripts for working with the Seurat package.")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Kin Lau",
    devtools.desc.author = "Kin Lau <kin.lau@vai.org> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
