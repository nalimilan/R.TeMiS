EuropresseSource <- function(x, encoding = "UTF-8") {
    tree <- htmlParse(x, encoding=encoding)

    content <- getNodeSet(tree, "/html/body/table/tbody/tr/td")

    # Some HTML files do not have <tbody> (depending on the browser?)
    if(length(content) == 0)
        content <- getNodeSet(tree, "/html/body/table/tr/td")

    free(tree)

    s <- .Source(readEuropresseHTML, encoding, length(content), FALSE, NULL, 0, FALSE, class="EuropresseSource")
    s$Content <- content
    s$URI <- x
    s
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

# These functions are the same as those for XMLSource
getElem.EuropresseSource <- function(x) list(content = saveXML(x$Content[[x$Position]]), uri = x$URI)
eoi.EuropresseSource <- function(x) length(x$Content) <= x$Position
