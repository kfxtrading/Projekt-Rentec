if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

if (interactive() && identical(Sys.getenv("TERM_PROGRAM"), "vscode")) {
  if (requireNamespace("httpgd", quietly = TRUE)) {
    options(vsc.rstudioapi = TRUE)
    options(vsc.use_httpgd = TRUE)
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = FALSE)
    })
  }
}
