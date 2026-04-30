args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)

script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path("scripts", "run_rsi_only_strategy.R"), winslash = "/", mustWork = TRUE)
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

if (file.exists(file.path(project_root, "renv", "activate.R"))) {
  source(file.path(project_root, "renv", "activate.R"))
}

trailing_args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(trailing_args) >= 1L) trailing_args[[1L]] else file.path("config", "rsi_only_default.R")
config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "rsi_only_strategy.R"))

config_env <- new.env(parent = baseenv())
source(config_path, local = config_env)

if (!exists("rsi_only_config", envir = config_env, inherits = FALSE)) {
  stop("Config file must define a list named rsi_only_config.", call. = FALSE)
}

tryCatch(
  {
    run_rsi_only_strategy(get("rsi_only_config", envir = config_env, inherits = FALSE))
  },
  error = function(error) {
    message("\nRSI-only strategy run failed:")
    message(conditionMessage(error))
    quit(status = 1L, save = "no")
  }
)
