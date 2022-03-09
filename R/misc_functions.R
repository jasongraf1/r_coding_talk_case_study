# -------------------------------------------------------------------------
# File: misc_functions.R
#
# Description:
# Custom defined functions for various applications
# -------------------------------------------------------------------------

# print out session info for project
PrintSessionInfo <- function(){
  top_msg <- paste0(
    "Session Info for project\n",
    "Author: Jason Grafmiller\n",
    "Last modified: ", Sys.time(), "\n\n")
  writeLines(
    utils::capture.output(cat(top_msg), utils::sessionInfo()),
    here::here("sessionInfo.txt")
  )
}