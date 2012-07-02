FactivaSource <- function(x, encoding = "UTF-8") {
    tm::XMLSource(x, function(tree) {
        XML::xmlChildren(XML::xmlRoot(tree)$children$ppsArticleResponse[[1]])
    },
                  readFactiva, encoding)
}

