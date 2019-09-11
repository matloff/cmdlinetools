
# handy ops for files and directories

# for a specified set of files 'fls', report which ones contain the
# string 's'; if 'have' is FALSE, report which do NOT contain 's'

whichFilesHave <- function(fls,s,have=TRUE) {
   haveIt <- function(fl) {
      lns <- readLines(fl)
      length(grep(s,lns)) > 0
   }
   foundIdxs <- which(sapply(fls,haveIt))
   if (have) found <- fls[foundIdxs]
   else found <- fls[-foundIdxs]
   found
}
