# .onAttach <- function(libname, pkgname) {
#   check_update <- function() {
#     update_available <- "lighthouse" %in% old.packages(
#       repos = "file:////1b02file01/LI/All_Work/R/lighthouse"
#     )$Package
#     if (update_available) packageStartupMessage(paste0(
#       'An update to the lighthouse package is available. To update, run:\n\n',
#       'install.packages(\n',
#       '  "lighthouse",\n',
#       '  repos = "file:////1b02file01/LI/All_Work/R/lighthouse"\n',
#       ')\n'
#     ))
#   }
#   fail_msg <- function(...) packageStartupMessage(paste0(
#     "Did not check for lighthouse package updates because unable to connect ",
#     "to '//1b02file01/LI/All_Work/R/lighthouse/'."
#   ))
#   tryCatch(check_update(), warning = fail_msg, error = fail_msg)
# }
