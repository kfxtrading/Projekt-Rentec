args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)

script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path("scripts", "run_strategy.R"), winslash = "/", mustWork = TRUE)
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

if (file.exists(file.path(project_root, "renv", "activate.R"))) {
  source(file.path(project_root, "renv", "activate.R"))
}

trailing_args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(trailing_args) >= 1L) trailing_args[[1L]] else file.path("config", "default.R")
config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "strategy.R"))

config_env <- new.env(parent = baseenv())
source(config_path, local = config_env)

if (!exists("strategy_config", envir = config_env, inherits = FALSE)) {
  stop("Config file must define a list named strategy_config.", call. = FALSE)
}

cfg <- get("strategy_config", envir = config_env, inherits = FALSE)

# Load the walk-forward extension only when the config requests it. This
# keeps R/strategy.R free of walk-forward logic.
use_wf <- isTRUE(cfg$walk_forward)
if (use_wf) {
  source(file.path(project_root, "R", "strategy_walk_forward.R"))
}

tryCatch(
  {
    if (use_wf) {
      run_strategy_walk_forward(cfg)
    } else {
      run_strategy(cfg)
    }
  },
  error = function(error) {
    message("\nStrategy run failed:")
    message(conditionMessage(error))
    quit(status = 1L, save = "no")
  }
)
