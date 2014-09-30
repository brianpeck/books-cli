#!/usr/bin/Rscript

# Note: This doesn't work!  Can't use menu() in non-interactive mode. :(
# Will need to interface with command line arguments, which will be more work.

source('~/other/books-cli/csvinterface.R')
source('~/other/books-cli/books.R')
source('~/other/books-cli/uuid.R')
args <- commandArgs(TRUE)

options <- c("Add New Book","Add Reading Progress")

test <- function() {
  cat("This is a test!")
}

test2 <- function() {
  cat("This is another test!")
}

#cat(args,"\n",sep=",")

book.num <- menu(strtrim(list.book.titles(),32),title="Select Book:")
