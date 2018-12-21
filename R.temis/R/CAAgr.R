#' corpusCa
#'
#' This function make a correspondence analysis, you should observe it by explor.
#'
#' @param corpus  corpus
#' @param dtm Document-term matrix
#' @param variables variables.
#' @param sparsity numeric argument that indicate the sparsity of your dtm.
#' @param ... others arguments
#'
#' @return a correspondence analysis
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- corpusCa(corpus,dtm)
#' @author Bouchet-Valat Milan
#' @export


corpusCa <- function(corpus, dtm=NULL, variables=NULL, sparsity=1, ...) {
  oldMeta<-meta<-meta(corpus)[colnames(meta(corpus)) != "MetaID"]

  # removeSparseTerms() does not accept 1
  if(sparsity < 1)
    dtm<-removeSparseTerms(dtm, sparsity)

  invalid<-which(apply(dtm,1,sum)==0)
  if(length(invalid) > 0) {
    dtm<-dtm[-invalid, , drop=FALSE]
    meta<-oldMeta[-invalid, , drop=FALSE]
    corpus<-corpus[-invalid]
  }

  ndocs<-nrow(dtm)
  nterms<-ncol(dtm)

  if(ndocs <= 1 || nterms <= 1) {
    warning("Please increase the value of the 'sparsity' parameter so that at least two documents and two terms are retained.")
    return()
  }

  if(length(invalid) > 0) {
    print("Documents skipped from correspondence analysis:\n")

  }

  skippedVars1<-character()
  skippedVars2<-character()
  skippedLevs<-character()
  origVars<-character()

  dupLevels<-any(duplicated(unlist(lapply(meta, function(x) substr(unique(as.character(x[!is.na(x)])), 0, 30)))))


  varDtm <- NULL

  # Create mean dummy variables as rows
  # Keep in sync with showCorpusClustering()

  # Just in case variables have common levels, and are truncated to the same string
  vars <- colnames(meta)
  vars10<-make.unique(substr(vars, 0, 10))
  vars20<-make.unique(substr(vars, 0, 20))

  if(ncol(meta) > 0) {
    for(i in 1:ncol(meta)) {
      var<-vars[i]
      levs<-levels(factor(meta[,i]))
      totNLevels<-nlevels(factor(oldMeta[,i]))

      if(length(levs) == 0) {
        skippedVars1<-c(skippedVars1, var)
        next
      }
      else if(length(levs) > 100) {
        skippedVars2<-c(skippedVars2, var)
        next
      }
      else if(length(levs) < totNLevels) {
        skippedLevs<-c(skippedLevs, var)
      }

      # suppressWarnings() is used because rollup() warns when NAs are present
      suppressWarnings(mat<-rollup(dtm[1:ndocs, , drop=FALSE], 1, meta[i]))

      # If only one level is present, don't add the level name (e.g. YES),
      # except if all values are the same (in which case variable is useless but is more obvious that way)
      if(totNLevels == 1 && any(is.na(meta[,i])))
        rownames(mat)<-vars20[i]
      # In case of ambiguous levels of only numbers in levels, add variable names everywhere
      else if(dupLevels || !any(is.na(suppressWarnings(as.numeric(levs)))))
        rownames(mat)<-make.unique(paste(vars10[i], substr(levs, 0, 30), sep ="_" ))
      else # Most general case: no need to waste space with variable names
        rownames(mat)<-substr(levs, 0, 30)

      varDtm<-rbind(varDtm, mat)
      origVars<-c(origVars, rep(var, nrow(mat)))
    }
  }

  if(!is.null(variables) && sum(origVars %in% variables) < 2) {
    browser()
    print("Please select active variables so that at least two levels are present in the retained documents.")
    return()
    }

print("Running correspondence analysis")
msg <- ""

if(length(skippedVars1) > 0)
  msg <- print("Variable(s) have been skipped since they contain only missing values for retained documents.")

if(length(skippedVars2) > 0) {
  msg2 <- print("Variable(s) have been skipped since they contain more than 100 levels.")
  if(msg != "")
    msg <- paste0(msg, "\n\n", msg2)
  else
    msg <- msg2
 }

skippedVars <- unique(c(skippedVars1, skippedVars2))

if(length(skippedLevs) > 0) {
  msg2 <- print("Some levels of variable(s) have been skipped since they contain only missing values for retained documents.")
  if(msg != "")
    msg <- paste0(msg, "\n\n", msg2)
  else
    msg <- msg2
}

if(msg != "")
  print("info")

rownames(dtm) <- paste0("DOC", rownames(dtm))
newDtm <- as.matrix(rbind(dtm, varDtm))
rownames(newDtm) <- make.names(rownames(newDtm)) #BUG EXPLOR caractere speciaux

if(!is.null(variables))
  obj <- FactoMineR::CA(newDtm, row.sup =c(1:nrow(dtm), nrow(dtm) + which(!origVars %in% variables)), ...)
else if(nrow(newDtm) - ndocs > 0)
  obj <- FactoMineR::CA(newDtm, row.sup = (ndocs+1):nrow(newDtm), ...)
else
  obj <- FactoMineR::CA(newDtm, ...)

if(nrow(newDtm) - ndocs > 0) {
  obj$rowvars <- seq.int(ndocs + 1, nrow(newDtm))
  names(obj$rowvars) <- origVars
  }

obj
}


#' viewCa
#'
#' This function print information about your ca
#'
#' @param ca  result of corpusCa
#' @param axe number of your axe
#' @param by sort by words or docs.
#' @param nbmax number of docs or words
#' @param type print coordinate or contribution
#'
#' @return an array
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- corpusCa(corpus,dtm)
#' tab <- ViewCa(res,1,by="words",20,type="contribution")
#' @author Bouchet-Valat Milan
#' @export




ViewCa <- function(ca,axe,by="words",nbmax=10,type="contribution"){
  if(type=="contribution" && by=="words"){
    return(sort(ca$col$contrib[,axe],T)[1:nbmax])}
  if(type=="coordinate" && by=="words"){
    return(sort(ca$col$coord[,axe],T)[1:nbmax])}
  if(type=="coordinate" && by=="docs"){
    return(sort(ca$row$coord[,axe],T)[1:nbmax])}
  if(type=="contribution" && by=="docs"){
    return(sort(ca$row$contrib[,axe],T)[1:nbmax])}
}

