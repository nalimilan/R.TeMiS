#' corpus_clustering
#'
#' Run a hierarchical clustering on documents of a corpus based on
#' a correspondence analysis. The number of axes from `ca` which are
#' used depends on the value of the `n` argument passed to [`corpus_ca`].
#'
#' @param ca A [`CA`][FactoMineR::CA] object resulting from a call to [`corpus_ca`].
#' @param n Number of clusters to create. If 0 (the default), it is determined by clicking
#'   on the plot to choose the cut height.
#'
#' @return A [`HCPC`][FactoMineR::HCPC] object.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' res <- corpus_ca(corpus, dtm, ncp=2, sparsity=0.98)
#' corpus_clustering(res, 3)
#'
#' @export
corpus_clustering <- function(ca, n=0){
  res <- FactoMineR::HCPC(ca, n)
  plot(res, choice="map", label="none", draw.tree=FALSE)
  res
}

#' add_clusters
#'
#' Add a meta-data variable to a corpus indicating the cluster
#' to which each document belongs.
#'
#' @param corpus A `Corpus` object.
#' @param clust A [`HCPC`][FactoMineR::HCPC] object resulting from
#'   a call to [`corpus_clustering`].
#'
#' @return A `Corpus` object with `meta(corpus, "cluster")` indicating the cluster
#'   of each document.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' res <- corpus_ca(corpus, dtm, ncp=2, sparsity=0.98)
#' clust <- corpus_clustering(res, 3)
#' corpus <- add_clusters(corpus, clust)
#' meta(corpus)
#'
#' @export
add_clusters <- function(corpus, clust) {
  meta(corpus, .gettext("cluster")) <- clust$call$X[names(corpus), "clust"]
  return(corpus)
}