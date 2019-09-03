
# routines to aid in the debugging process

library(gtools)

# globals:

#   srcname: name of the source file being debugger (assumes just 1)
#   applines: lines of that file

#####################  misc. debug ops  ############################

# for debugging runtime errors; after running this, whenever have a
# runtime error, run debugger()
odf <- function() options(error=dump.frames)

# typing saver
tb <- function() traceback()

##################### aids for using debug(), browser() ###############

# say you are debugging a file x.R; call srci("x.R") for init; then can
# do things like displaying the current line (R only displays it once,
# and it may be gone now), printing a range of lines in the file,
# conveniently setting a breakpoint, etc. 

# init; srci() must be called each time you modify 'src'!
srci <- function(src,debug=TRUE) {
   srcname <<- src
   source(src)
   if (debug) {
      applines <<- readLines(src)
      # to avoid cluttering current directory, this goes to the home
      # directory; TODO: make the location an option
      sink(file="~/debugrecord",split=T)
   }
}

# find line on which the debugger currently stands
cl <- function() {
   rec <- readLines("~/debugrecord")
   target <- "debug at"
   for (i in length(rec):1) {
      reci <- rec[i]
      ge <- gregexpr(target,reci)[[1]]
      if (ge == 1) {
         numbersign <- gregexpr("#",reci)[[1]][1]
         if (numbersign < 0) continue
         linenumstart <- numbersign + 1
         tmp <- substr(reci,linenumstart,nchar(reci))
         colon <- gregexpr(":",tmp)[[1]][1]
         return(as.integer(substr(tmp,1,colon-1)))
      }
   }
   print("line number not found")
}

# print the lines in app from m to n; if one of them is null, print all within
# 5 lines in that direction
l <- function(m=NULL,n=NULL) {
   cli <- cl()
   if (is.null(m)) {
      m <- max(1,cli-5)
   }
   if (is.null(n)) {
      n <- min(length(applines),cli+5)
   }
   for (i in m:n) {
      if (i == cli) cat("* ")
      cat(i,applines[i],"\n",sep=" ")
   }
}

# abbreviation, so can type ll instead of l()
make_ll <- function() {
   require(ksREPL)
   ll <<- ksrProto
   ll$f <<- l
}

# convenient setting of breakpoint; must call trace(function) to cancel
b <- defmacro(linenum, expr={
   setBreakpoint(srcname,linenum)
})


############################## other stuff #########################

# quick-and-dirty edit; invoke text editor on f, and write back to f; 
# f is a character string
fedit <- function(f) {
   if (!is.character(f)) stop("f must be a character string")
   cmd <- paste(f," <- edit(",f,")")
   evalrstring(cmd)
}

# load updated package
relib <- function(pkg) {
   evalrstring(paste("detach(package:",pkg,")",sep=""))
   evalrstring(paste("library(",pkg,")"))
}

# execute the given R expression
evalrstring <- function(toexec) {
   eval(parse(text=toexec))
}
