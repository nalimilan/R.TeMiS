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

        s <- .Source(readFactivaHTML, encoding, length(content), FALSE, seq(1, length(content)), 0, FALSE)
        s$Content <- content
        s$URI <- match.call()$x
        class(s) = c("FactivaSource", "Source")
        s
    }
}

# Taken from tm
.Source <- function(defaultreader, encoding, length, lodsupport, names, position, vectorized, class = NULL) {
    if (vectorized && (length <= 0))
        stop("vectorized sources must have positive length")

    if (!is.null(names) && (length != length(names)))
        stop("incorrect number of element names")

    structure(list(DefaultReader = defaultreader, Encoding = encoding, Length = length,
                   LoDSupport = lodsupport, Names = names, Position = position, Vectorized = vectorized),
              class = unique(c(class, "Source")))
}

# These functions need to be exactly the same as those for XMLSource
# since they can be used with the Factiva XML source
# These functions are the same as those for XMLSource
getElem.FactivaSource <- function(x) list(content = XML::saveXML(x$Content[[x$Position]]), uri = x$URI)
eoi.FactivaSource <- function(x) length(x$Content) <= x$Position
