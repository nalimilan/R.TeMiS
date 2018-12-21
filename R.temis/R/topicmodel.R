
#' Topicmodel
#'
#' This function create a topic model.
#'
#' @param dtm  document term matrix.
#' @param nb number of topic.
#' @import slam
#' @import topicmodels
#' @import tidytext
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- Topicmodel(dtm)
#' @author Bouchet-Valat Milan
#' @export

Topicmodel<-function(dtm,nb){
ap_lda <- LDA(dtm, k = nb, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
  restopic <- ""
  attr(restopic,"ap_lda") <- ap_lda
  attr(restopic,"ap_topics") <- ap_topics
  attr(restopic,"ap_top_terms") <- ap_top_terms
return(restopic)
  }

#' Viewtopic
#'
#' This function create resume of your topic model.
#'
#' @param restopic a topic model.
#'
#' @return an array.
#'
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- Topicmodel(dtm)
#' tab <- Viewtopic(res)
#' @author Bouchet-Valat Milan
#' @export
Viewtopic <- function(restopic){
  beta_spread <- attr(restopic,"ap_topics") %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001)
  return(beta_spread)
  }



#' PLottopic
#'
#' This function plot data from your topic model.
#'
#' @param restopic a topic model.
#'
#' @return a graphic.
#'
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' res <- Topicmodel(dtm)
#' Plottopic(res)
#' @author Bouchet-Valat Milan
#' @export
Plottopic <- function(restopic){
  attr(restopic,"ap_top_terms") %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
}
