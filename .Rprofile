source("renv/activate.R")
Sys.setenv(http_proxy="http://proxy.gov.si:80")
Sys.setenv(http_proxy_user="http://proxy.gov.si:80")
Sys.setenv(https_proxy="http://proxy.gov.si:80")
Sys.setenv(https_proxy_user="http://proxy.gov.si:80")
cat("UMAR proxy is set!")
options(continue = " ")

# always run testthat in interactive session, cuz it comes in handy i guess
if (interactive()) {
  suppressMessages(require(testthat))
}
