FactivaSource <- function(x, encoding = "UTF-8", format = c("auto", "XML", "HTML")) {
    format <- match.arg(format)

    # XML format
    if(format == "XML" ||
       (format == "auto" && grepl(".(xml|XML)$", x))) {
        tm::XMLSource(x, function(tree) {
            sapply(XML::xmlChildren(XML::xmlChildren(XML::xmlChildren(XML::xmlRoot(tree))
                $ppsArticleResponse)$ppsarticleResultSet), XML::xmlChildren)
        },
                      readFactivaXML, encoding)
    }
    # HTML format
    else {
        tree <- XML::htmlParse(x, encoding=encoding)

        # The full class is "article XXArticle", with XX the language code
        content <- XML::getNodeSet(tree, "//div[starts-with(@class, 'article')]")
        XML::free(tree)

        s <- tm:::.Source(readFactivaHTML, encoding, length(content), FALSE, seq(1, length(content)), 0, FALSE)
        s$Content <- content
        s$URI <- match.call()$x
        class(s) = c("FactivaSource", "Source")
        s
    }
}

# These functions need to be exactly the same as those for XMLSource
# since they can be used with the Factiva XML source
getElem.FactivaSource <- function(x) tm:::getElem.XMLSource(x)
eoi.FactivaSource <- function(x) tm:::eoi.XMLSource(x)
