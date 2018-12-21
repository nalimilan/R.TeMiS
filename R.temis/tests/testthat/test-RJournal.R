# Bouchet-Valat & Bastin, RcmdrPlugin.temis, a Graphical Integrated Text Mining Solution in R,
# R Journal, 5(1), 2013.

context("RJournal")

# Stopwords change accross tm versions
stopwords.en <- c("a", "about", "above", "across", "after", "again", 
                  "against", "all", "almost", "alone", "along", "already", "also", 
                  "although", "always", "am", "among", "an", "and", "another", 
                  "any", "anybody", "anyone", "anything", "anywhere", "are", "area", 
                  "areas", "aren't", "around", "as", "ask", "asked", "asking", 
                  "asks", "at", "away", "b", "back", "backed", "backing", "backs", 
                  "be", "became", "because", "become", "becomes", "been", "before", 
                  "began", "behind", "being", "beings", "below", "best", "better", 
                  "between", "big", "both", "but", "by", "c", "came", "can", "can't", 
                  "cannot", "case", "cases", "certain", "certainly", "clear", "clearly", 
                  "come", "could", "couldn't", "d", "did", "didn't", "differ", 
                  "different", "differently", "do", "does", "doesn't", "doing", 
                  "don't", "done", "down", "downed", "downing", "downs", "during", 
                  "e", "each", "early", "either", "end", "ended", "ending", "ends", 
                  "enough", "even", "evenly", "ever", "every", "everybody", "everyone", 
                  "everything", "everywhere", "f", "face", "faces", "fact", "facts", 
                  "far", "felt", "few", "find", "finds", "first", "for", "four", 
                  "from", "full", "fully", "further", "furthered", "furthering", 
                  "furthers", "g", "gave", "general", "generally", "get", "gets", 
                  "give", "given", "gives", "go", "going", "good", "goods", "got", 
                  "great", "greater", "greatest", "group", "grouped", "grouping", 
                  "groups", "h", "had", "hadn't", "has", "hasn't", "have", "haven't", 
                  "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", 
                  "hers", "herself", "high", "higher", "highest", "him", "himself", 
                  "his", "how", "how's", "however", "i", "i'd", "i'll", "i'm", 
                  "i've", "if", "important", "in", "interest", "interested", "interesting", 
                  "interests", "into", "is", "isn't", "it", "it's", "its", "itself", 
                  "j", "just", "k", "keep", "keeps", "kind", "knew", "know", "known", 
                  "knows", "l", "large", "largely", "last", "later", "latest", 
                  "least", "less", "let", "let's", "lets", "like", "likely", "long", 
                  "longer", "longest", "m", "made", "make", "making", "man", "many", 
                  "may", "me", "member", "members", "men", "might", "more", "most", 
                  "mostly", "mr", "mrs", "much", "must", "mustn't", "my", "myself", 
                  "n", "necessary", "need", "needed", "needing", "needs", "never", 
                  "new", "newer", "newest", "next", "no", "nobody", "non", "noone", 
                  "nor", "not", "nothing", "now", "nowhere", "number", "numbers", 
                  "o", "of", "off", "often", "old", "older", "oldest", "on", "once", 
                  "one", "only", "open", "opened", "opening", "opens", "or", "order", 
                  "ordered", "ordering", "orders", "other", "others", "ought", 
                  "our", "ours", "ourselves", "out", "over", "own", "p", "part", 
                  "parted", "parting", "parts", "per", "perhaps", "place", "places", 
                  "point", "pointed", "pointing", "points", "possible", "present", 
                  "presented", "presenting", "presents", "problem", "problems", 
                  "put", "puts", "q", "quite", "r", "rather", "really", "right", 
                  "room", "rooms", "s", "said", "same", "saw", "say", "says", "second", 
                  "seconds", "see", "seem", "seemed", "seeming", "seems", "sees", 
                  "several", "shall", "shan't", "she", "she'd", "she'll", "she's", 
                  "should", "shouldn't", "show", "showed", "showing", "shows", 
                  "side", "sides", "since", "small", "smaller", "smallest", "so", 
                  "some", "somebody", "someone", "something", "somewhere", "state", 
                  "states", "still", "such", "sure", "t", "take", "taken", "than", 
                  "that", "that's", "the", "their", "theirs", "them", "themselves", 
                  "then", "there", "there's", "therefore", "these", "they", "they'd", 
                  "they'll", "they're", "they've", "thing", "things", "think", 
                  "thinks", "this", "those", "though", "thought", "thoughts", "three", 
                  "through", "thus", "to", "today", "together", "too", "took", 
                  "toward", "turn", "turned", "turning", "turns", "two", "u", "under", 
                  "until", "up", "upon", "us", "use", "used", "uses", "v", "very", 
                  "w", "want", "wanted", "wanting", "wants", "was", "wasn't", "way", 
                  "ways", "we", "we'd", "we'll", "we're", "we've", "well", "wells", 
                  "went", "were", "weren't", "what", "what's", "when", "when's", 
                  "where", "where's", "whether", "which", "while", "who", "who's", 
                  "whole", "whom", "whose", "why", "why's", "will", "with", "within", 
                  "without", "won't", "work", "worked", "working", "works", "would", 
                  "wouldn't", "x", "y", "year", "years", "yes", "yet", "you", "you'd", 
                  "you'll", "you're", "you've", "young", "younger", "youngest", 
                  "your", "yours", "yourself", "yourselves", "z")

library(R.temis)

corpus <- import_corpus(system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva"),
                        "factiva", language="en")
test_that("importation works", {
  expect_equal(length(corpus), 19)
})

dtm <- build_dtm(corpus)
dict <- dictionary(dtm)
dict <- dict[!rownames(dict) %in% stopwords.en,]
dtm2 <- combine_terms(dtm, dict)
test_that("dtm works", {
  expect_equal(nrow(dtm), 19)
  expect_equal(nrow(dtm2), 19)
  expect_equal(ncol(dtm), 1032)
  expect_equal(ncol(dtm2), 705)
})

specTerms <- specific_terms(dtm2, meta(corpus)[["Date"]], p=0.1, min_occ=5, n=10)
test_that("specific terms match Table 1", {
  expect_equal(round(c(specTerms[[1]]), 4),
               c(2.0101, 1.2563, 1.5075, 1.7588, 1.005, 1.2563, 2.5126, 1.7588,
                 NA, 0, 0, 66.6667, 71.4286, 60, 50, 66.6667, 55.5556, 38.4615,
                 43.75, NA, 0, 0, 0.5794, 0.338, 0.4829, 0.676, 0.2897, 0.4346,
                 1.2554, 0.7726, NA, 0.9174, 1.0623, 8, 5, 6, 7, 4, 5, 10, 7,
                 NA, 0, 0, 12, 7, 10, 14, 6, 9, 26, 16, NA, 19, 22, 3.3408,
                 2.6673, 2.5714, 2.3631, 2.1825, 2.1365, 2.1162, 2.0293, NA,
                 -2.1201, -2.3698, 0.0004, 0.0038, 0.0051, 0.0091, 0.0145, 0.0163,
                 0.0172, 0.0212, NA, 0.017, 0.0089),
               check.attributes=FALSE)
})

ca <- corpus_ca(corpus, dtm2, sparsity=0.94)
test_that("CA matches text and Figure 4", {
  expect_equal(nrow(ca$row$coord), length(corpus))
  expect_equal(nrow(ca$col$coord), 239)
  expect_equal(head(rownames(ca$col$coord)[order(ca$col$contrib[,1], decreasing=TRUE)], 12),
               c("post", "grade", "crude", "west", "bbl", "texa", "bring", "sweet", 
                 "dlrs", "texaco", "compani", "effect"))
})