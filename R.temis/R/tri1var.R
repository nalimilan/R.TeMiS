

#' Tri1var
#'
#' This function make an univariate analyse.
#'
#' @param corpus a document-term matrix.
#' @param var  variable.
#'
#' @return plot a barchart.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' tri1var(corpus,"reg")
#' @author Bouchet-Valat Milan
#' @import lattice
#' @export

tri1var <- function(corpus,var){
  absVarFreqs <- table(meta(corpus,var), dnn=var)
  varFreqs <- prop.table(absVarFreqs) * 100
  varFreqstot <- addmargins(varFreqs)
  print(varFreqstot)
  barchart(varFreqs, xlab="% des documents",
           main=paste("Document distribution",var,sep=" by "), auto.key=list(rev=TRUE))
}


#' Tri2var
#'
#' This function make an bivariate analyse.
#'
#' @param corpus a document-term matrix.
#' @param var.row variable.
#' @param var.col  variable.
#'
#' @return plot a barchart.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' tri2var(corpus,"reg","statut")
#' @author Bouchet-Valat Milan
#' @export


tri2var <- function(corpus,var.row, var.col){
absVarFreqs <- table(meta(corpus, c(var.row, var.col)))
varFreqs <- prop.table(absVarFreqs, 1) * 100

varFreqstot <- addmargins(varFreqs, 2)
print(varFreqstot)
barchart(varFreqs, stack=FALSE, xlab="% des documents",
         main=paste("Document distribution" ,paste(var.row,var.col,sep=" and "),sep = " by "),auto.key=list(rev=TRUE))
}
