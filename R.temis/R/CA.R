#' StartCA
#'
#' This function make a correspondence analysis, you should observe it by explor.
#'
#' @param dtm document term matrix
#' @param ncp nomber of axes you want to keep
#'
#' @return a correspondence analysis
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- StartCA(dtm,ncp=5)
#' @author Bouchet-Valat Milan
#' @export
StartCA <- function(dtm,ncp=5){
  res <- FactoMineR::CA(ncp=as.numeric(ncp),as.matrix(dtm[rowSums(as.matrix(dtm))>0,]))
  return(res)
}

#' StartClus
#'
#' This function make a clustering.
#'
#' @param res result of your correspondence analysis.
#' @param clus nomber of group you want to keep. Keep 0 in order to manualy cut the tree.
#'
#' @return a clustering.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- StartCA(dtm,ncp=5)
#' resHCPC <- StartClus(res,clus=3)
#' @author Bouchet-Valat Milan
#' @export
StartClus <- function(res,clus=0){
  resHCPC<- FactoMineR::HCPC(res,as.numeric(clus))
  plot(resHCPC,choice="map",label="none",draw.tree = F)
  return(resHCPC)
}

#' ClusterVariable
#'
#' This function import cluster data into your corpus.
#'
#' @param corpus Corpus.
#' @param resHCPC a clustering from StartClus.
#'
#' @return a corpus.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- StartCA(dtm,ncp=5)
#' resHCPC <- StartClus(res,clus=3)
#' newcorpus <- ClusterVariable(corpus, resHCPC)
#' @author Bouchet-Valat Milan
#' @export

ClusterVariable <- function(corpus,resHCPC){
  meta(corpus,"Cluster") <- resHCPC$call$X$clust
  return(corpus)
}


