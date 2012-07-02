FactivaSource <- function(x, encoding = "UTF-8") {
    tm::XMLSource(x, function(tree) {
        sapply(XML::xmlChildren(XML::xmlChildren(XML::xmlChildren(XML::xmlRoot(tree))
            $ppsArticleResponse)$ppsarticleResultSet), XML::xmlChildren)
    },
                  readFactiva, encoding)
}

