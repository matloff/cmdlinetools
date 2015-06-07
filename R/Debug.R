
# routines to aid in the debugging process

# for debugging exec errors
odf <- function() options(error=dump.frames)

# typing saver
tb <- function() traceback()

##################### aids for using debug(), browser() ###############

# say you are debugging a file x.R; call srci("x.R") for init; then can
# do things like displaying the current line (R only displays it once,
# and it may be gone now), printing a range of lines in the file,
# conveniently setting a breakpoint, etc. 

# init; srci() must be called each time you modify 'src'!
srci <- function(src) {
   srcname <<- src
   source(src)
   applines <<- readLines(src)
   sink(file="debugrecord",split=T)
}

# find line on which the debugger currently stands
cl <- function() {
   rec <- readLines("debugrecord")
   target <- paste(srcname,"#",sep="")
   for (i in length(rec):1) {
      reci <- rec[i]
      ge <- gregexpr(target,reci)[[1]]
      if (ge == 1) {
         numbersign <- gregexpr("#",r)[[1]][1]
         if (numbersign < 0) continue
         linenumstart <- numbersign + 1
         tmp <- substr(reci,linenumstart,nchar(reci))
         colon <- gregexpr(":",tmp)[[1]][1]
         return(as.integer(substr(tmp,1,colon-1)))
      }
   }
   print("line number not found; maybe you didn't call debug() or
   browser()?")
}

# print the lines in app from m to n; if one of them is null, print all within
# 5 lines in that direciton
l <- function(m=NULL,n=NULL) {
   cli <- cl()
   if (is.null(m)) {
      m <- max(1,cli-5)
   }
   if (is.null(n)) {
      n <- min(length(applines),cli+5)
   }
   for (i in m:n) {
      if (i == cli) 
         cat("CURRENT: ")
      cat(i,applines[i],"\n",sep=" ")
   }
}

# set breakpoint at line linenum; can be turned off only by
# untrace(functionname), which also cancels debug(functionname)
b <- function(linenum) {
   setBreakpoint(srcname,linenum)
}

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
   cat(toexec,file="tmpexec")
   source("tmpexec")
}
