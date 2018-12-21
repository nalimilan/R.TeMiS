#' terms_graph
#'
#' Plot a graph of terms.
#'
#' @param dtm A `DocumentTermMatrix` object.
#' @param n The maximum number of terms to represent.
#' @param min_occ The minimum number of occurrences for a term to be retained.
#' @param interactive If `TRUE`, show an interactive plot using [`tkplot`]. This
#'   is the case by default for interactive sessions.
#' @param vertex.label.cex The font size for vertex labels.
#'   It is interpreted as a multiplication factor of some device-dependent base font size.
#' @param ... Optional arguments passed to [`plot.igraph`] or [`tkplot`].
#'
#' @return The ID of the plot returned by [`tkplot`] if `interactive=TRUE`,
#'   or `NULL` invisibly otherwise.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' terms_graph(dtm, 100, 3)
#'
#' @export
#'
terms_graph <- function(dtm, n=100, min_occ=0, interactive=base::interactive(),
                        vertex.label.cex=1, ...) {
  if(min_occ > 0)
    dtm <- dtm[, col_sums(dtm) > min_occ]

  if(ncol(dtm) > n)
    dtm <- dtm[, head(order(col_sums(dtm), decreasing=TRUE), n)]

  m <- crossprod_simple_triplet_matrix(dtm > 0)
  g1 <- igraph::graph.adjacency(m,mode="lower", weighted=TRUE, diag = FALSE)
  eff <- col_sums(dtm)
  igraph::E(g1)$weight <- 1/igraph::edge_attr(g1, 'weight')
  g3 <- igraph::minimum.spanning.tree(g1)
  igraph::E(g3)$weight <- 1/igraph::E(g3)$weight
  do_plot <- if(interactive) igraph::tkplot else plot
  do_plot(g3, ..., vertex.label=colnames(m),
          edge.width=(igraph::E(g3)$weight/max(igraph::E(g3)$weight))*10,
          vertex.size=0,
          vertex.label.cex=sqrt(10*eff/max(eff))*vertex.label.cex,
          edge.curved=0.6)
}
