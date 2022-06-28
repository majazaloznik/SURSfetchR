#' Log script run
#'
#' Simple helper function that creates a log if it doesn't exist yet in the `logs`
#' folder and appends a row logging a script run. Logs in two logs, one for
#' the individual script file and one common log for all the scripts, which
#' also includes the script name. Logs are dataframes saved as .rds files.
#'
#' @param path path to logs folder, defaults to `here::here("logs")`
#' @param script name of script being run, defaults to actual script being run
#'
#' @return NA if there is an error, otherwise nothing, but has side effects
#' of writing to log files.
#' @export
#'
log_script_run <- function(path = here::here("logs"),
                           script = envDocument::getScriptPath() ){
  script <- sub("O:/","\\\\192.168.38.7\\public$/", script)
  last_run <- Sys.time()
  tryCatch(
    {fname <- paste0(path,"/log.rds")
      if(file.exists(fname)) {
        log <- readRDS(fname)
        log[nrow(log) + 1, "timestamp"] <- last_run
        log[nrow(log) , "script"] <- script
      } else {
        log <- data.frame(timestamp = last_run,
                          script = script)
      }
      saveRDS(log, file = paste0(path,"/log.rds"))
    },
    error = function(error_message) {
      return(NA)
    }
  )

  tryCatch(
    {fname <- paste0(path, "/log_", sub("(.*)\\..*$", "\\1", basename(script)),".rds")
      if(file.exists(fname)) {
        log <- readRDS(fname)
        log[nrow(log) + 1, "timestamp"] <- last_run
        } else {
        log <- data.frame(timestamp = last_run)
      }
      saveRDS(log, file = fname)
    },
    error = function(error_message) {
      return(NA)
    }
  )
}
