args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)

script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path("scripts", "setup_packages.R"), winslash = "/", mustWork = TRUE)
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

required_packages <- c(
  "quantmod", "TTR", "scales", "foreach", "doParallel", "dlm",
  "progress", "xgboost", "rugarch", "rmarkdown", "highcharter",
  "PerformanceAnalytics", "htmltools", "rstudioapi", "renv",
  "languageserver", "httpgd", "xts", "zoo"
)

if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing renv bootstrap package...")
  install.packages("renv")
}

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv is unavailable after installation.", call. = FALSE)
}

if (!file.exists(file.path(project_root, "renv", "activate.R"))) {
  message("Initializing renv project...")
  renv::init(bare = TRUE, restart = FALSE)
}

source(file.path(project_root, "renv", "activate.R"))

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  message("Installing missing project packages: ", paste(missing_packages, collapse = ", "))
  renv::install(missing_packages, prompt = FALSE)
} else {
  message("All required project packages are already installed.")
}

message("Writing renv.lock...")
renv::snapshot(prompt = FALSE)

status <- renv::status()
if (isTRUE(status$synchronized)) {
  message("renv is synchronized.")
} else {
  message("renv is not synchronized. Run renv::status() for details.")
}
