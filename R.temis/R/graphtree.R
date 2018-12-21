#' CreateTree
#'
#' This function create a word graph
#'
#' @param dtm document term matrix.
#' @param freq term minimum frequency.
#' @param n nomber of term.
#'
#' @return a word graph
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' tree <- CreateTree(dtm,freq=3,n=100)
#' @author Bouchet-Valat Milan
#' @export
#'
CreateTree <- function(dtm,freq=0,n=100){
  if(freq>0){
    dtm <-dtm[,col_sums(dtm)>freq]
  }
  if(ncol(dtm)>n){
    dtm <- dtm[,head(order(col_sums(dtm),decreasing = T),n)]
  }
  m <-crossprod_simple_triplet_matrix(dtm>0)
  g1<-igraph::graph.adjacency(m,mode="lower",weighted=T,diag = F)
  eff<-col_sums(dtm)
  igraph::E(g1)$weight<-1/igraph::edge_attr(g1,'weight')
  g3<-igraph::minimum.spanning.tree(g1)
  igraph::E(g3)$weight<-1/igraph::E(g3)$weight
  igraph::tkplot(g3,vertex.label=colnames(m),edge.width=(igraph::E(g3)$weight/max(igraph::E(g3)$weight))*10,vertex.size=0,vertex.label.cex=sqrt(10*eff/max(eff)), edge.curved=0.6)
}
