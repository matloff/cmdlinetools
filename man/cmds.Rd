\name{cmds,cmdn,cmde}
\alias{cmds}
\alias{cmdn}
\alias{cmde}

\title{
Comand history routines.
}

\description{
Comand history routines.
}

\usage{
cmds(wild = NULL, exc = NULL, keep = FALSE, ask = FALSE) 
cmdn(cmdnum)
cmde()
}

\arguments{
  \item{wild}{Character string for indicating wild card 
     to be matched at start of command.}
  \item{exc}{Exclude any commands containing this string.}
  \item{keep}{Return a vector with the found commands.}
  \item{ask}{If true, prompt the user for the number of a command 
     to execute (Enter means none); \code{ask} and 
     \code{keep} should not both be set to TRUE.}
}

\author{
Norm Matloff
}

