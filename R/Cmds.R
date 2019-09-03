
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
   unlink('cmdshistory')
}

######################  docmdmac()  #################################
 
# utility: executes specified command at the caller's level

docmdmac <- defmacro(strcmd,
   expr={eval(parse(text=strcmd))})

######################  grepyes()  #################################

# utility: return TRUE if patt is found in s

grepyes <- function(patt,s) length(grep(patt,s)) > 0

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

