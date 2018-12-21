
library(RcmdrPlugin.temis)
library(RcmdrPlugin.temis)
library(ggplot2)
library(FactoMineR)
library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)


ex <- "C:/Users/chollet_ant/Corpus/Corpus_final.txt"
corpus <- ImportCorpus(ex,format = "alceste")


df <- read.csv2("C:/Users/chollet_ant/var.csv", header = TRUE)

#corpus <- ImportCorpus(testtxt,"txt")
#corpusvar <- setCorpusVariables(corpus,df)
d <- Createdtm(corp)
dtm <- Createdtm(corpus)
dic <- CreateDictionary(corpus,dtm)
dic <- SortDictionary(dic,"Occurrences")
print("ze")
dic <- dic[,"Mot.vide"==""]
View(dic)
dic[c("nr","depart"),]

dtm <- StemDtm(dtm,dic)


dtm <- dtm[,dic["Mot.vide"]== ""]
cooccurrentTerms("bien",dtm)
print("aa")
corpus <- SplitCorpus(corpus,3)

keep <- row_sums(dtm[, c("recherche","sociologie")]) >= 1
corpusrechsocio<-corpus[keep]
dtmrechsocio<- dtm[keep,]

corpusidf <- corpus[meta(corpus,"reg")=="idf"]
View(corpus)
print("yo")


print("cc")
- dtm[,dic["Mot.vide"]== ""]
nuagemot <- CreateCloud(corpus, "fr", max.words = 50)
col_sums(dtm)[c("recherche","politique","anthropologie")]
tree <- CreateTree(dtm)
dtm[0,"recherche"] <- "yo"



re <- Freqterm(dtm)
re <- specificTerms(dtm,meta(corpus, "reg"))
cooccurrentTerms(term = "anthropologie",dtm)

re$idf
dtm <- dtm[,dic["Mot.vide"]== ""]


res <- corpusCa(corpus,dtm,sparsity = 0.9)
explor::explor(res)
res$col$contrib
sort(res$col$contrib[,1],decreasing = T)

clus <- StartClus(res)
clus$desc.var
clus$desc.ind$para
clus$desc.ind$dist
plot(clus$call$t$tree)
View(ClusterVariable(corpus,clus))


res <- corpusCa(corpus,dtm,variables = c("reg","them"),sparsity = 0.9)
ConcordenceTerm(corpus,dtm,"anthropologie")


for(i in(corpus)){
  print(corpus[[as.String(i)]][["content"]])
}
corpus[["content"]]

View(corpus)
for(i in corpus$content){
  print(i$content)
}

meta(corpus,"docs")

tri1var(corpus,c("reg"))#mettre number doc
tri1var(corpus,"statut")
tri1var(corpus,"them")
tri1var(corpus,"form")


restopic <- Topicmodel(dtm,3)
Viewtopic(restopic)
Plottopic(restopic)
re <-LexicalSummary(corpus,dtm,"reg")

