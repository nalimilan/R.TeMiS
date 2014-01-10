FactivaSource <- function(x, encoding = "UTF-8", format = c("auto", "XML", "HTML")) {
    format <- match.arg(format)

    # XML format
    if(format == "XML" ||
       (format == "auto" && grepl(".(xml|XML)$", x))) {
        XMLSource(x, function(tree) {
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

        s <- Source(readFactivaHTML, encoding, length(content), as.character(seq(1, length(content))), 0, FALSE, "FactivaSource")
        s$Content <- content
        s$URI <- x
        s
    }
}

# This functions need to be exactly the same as those for XMLSource
# since they can be used with the Factiva XML source
# These functions are the same as those for XMLSource
getElem.FactivaSource <- function(x) list(content = XML::saveXML(x$Content[[x$Position]]), uri = x$URI)
