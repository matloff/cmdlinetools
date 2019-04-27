
require(gtools)

# globals:

#    allcmds:  current command history

######################  cmnds()  ###################################

# lists all commands so far in the current session

# arguments:
#    wild: character string for indicating wild card 
#       to be matched at start of command
#    exc: exclude commands with containing this string
#    keep: return a vector of found commands
#    ask: prompt the user for the number of a command 
#       to execute (Enter means none); 'ask' and 
#    'keep' should not both be set to TRUE
# value: if 'keep', character vector of the commands, 
#    or 'ask', the selection number 

cmds <- function(wild=NULL,exc=NULL,keep=FALSE,ask=FALSE) {
   getallcmds()
   foundcmds <- NULL
   for (i in 1:length(allcmds)) {
      ci <- allcmds[i]
      if (!is.null(exc)) {
         gy <- grepyes(exc,ci)
         if (gy) next
      }
      if (!is.null(wild)) {
         gy <- grepyes(wild,ci)
         if (!gy) next
         if (wild != substr(ci,1,nchar(wild))) next
      }
      cat(i,ci,"\n")
      foundcmds <- c(foundcmds,ci)
   }
   if (ask) {
      resp <- readline("enter command number:  ")
      if (resp == "") return()
      return(resp)
   }
   if (keep) foundcmds
}

######################  cmdn()  ###################################

# executes the command of the given number, like Unix shell, e.g. '!12'

cmdn <- defmacro(cmdnum,expr={
      getallcmds()
      # allcmds <- invisible(cmds(keep=TRUE))
      wishcmd <- allcmds[cmdnum]
      cat(">>",wishcmd,"\n")
      print(docmdmac(wishcmd))
   }
)

######################  cmdw()  ###################################

# execute latest wild match, if any; like Unix shell, e.g. !ls

cmdw <- defmacro(wld,expr={
      tmp <- cmds(wild=wld,keep=TRUE,ask=FALSE)
      wishcmd <- tmp[length(tmp)]
      cat(">>",wishcmd,"\n")  # fake special prompt
      if (!grepyes("cmdw",wishcmd))
         print(docmdmac(wishcmd))
   }
)

######################  cmde()  ###################################

# execute command number optionally specified by user after she views
# commands list; user can preface with 'e', e.g. 'e 12' to edit cmd 12
# and then execute

cmde <- defmacro(dummy,expr={
      wishcmd <- cmds(ask=TRUE)
      if (!is.null(wishcmd)) {
         getallcmds()
         tmp <- strsplit(wishcmd,' ')[[1]]
         if (tmp[1] == 'e') {
             cmdnum <- as.integer(tmp[2])
             newcmd <- edit(allcmds[cmdnum])
             allcmds <<- c(allcmds,newcmd)
             cmdn(length(allcmds))
             cat(newcmd,'\n')
         } else  {
             cmdnum <- as.integer(tmp[1])
             cmdn(cmdnum)
             cat(allcmds[cmdnum],'\n')
         }
      } 
   }
)

######################  getallcmds()  ################################

# utility; updates the global, 'allcmds'

getallcmds <- function() 
{
   savehistory("cmdshistory")  # base R ftn
   allcmds <<- scan("cmdshistory",what="",sep="\n",quiet=TRUE)
}

######################  docmdmac()  #################################
 
# utility: executes specified command at the caller's level

docmdmac <- defmacro(strcmd,
   expr={eval(parse(text=strcmd))})

######################  docmdmac()  #################################

# utility: return TRUE if patt is found in s

grepyes <- function(patt,s) length(grep(patt,s)) > 0

######################  makeNoParen()  ###############################

# insidious!!!!

# for creating quick, short commands with no parentheses!
# typing cmdname at '>' will execute cmd

# could be done more elegantly than this, but just execute this and then
# copy and paste the output at the R prompt

makeNoParen <- function(cmdname,cmd) {
      cat(paste0(cmdname,'<- list(y=3)'),'\n');
      tmp <- sample(1:100,1);
      classname <- paste0('c',as.character(tmp));
      cat(paste0('class(',cmdname,') <- "',classname,'"'),'\n');
      printcmd <- paste0('print.',classname);
      obj <- paste0(classname,'Object')
      printcmd <- paste0(printcmd,' <- function(',obj,') ');
      printcmd <- paste0(printcmd,cmd);
      cat(printcmd,'\n')
}

# example:
# 
# x <- 8
# makeNoParen('w','x <<- x + 1')
# x  # prints 8
# w
# x  # prints 9


######################  obje()  ###################################

# lists objects with ls(); if user responds with object number, then
# apply str() to that object

obje <- defmacro(dummy,expr={
         lsout <- ls()
         print(lsout)
         num <- readline('apply str() to? ')
         if (num != '') {
            obj <- lsout[as.numeric(num)]
            docmdmac(paste0('str(',obj,')')) }
         }
)

