LexisNexisSource <- function(x, encoding = "UTF-8") {
    # This is a fragile method, but much simpler than actually parsing HTML
    # since documents are not a node but a sequence of unrelated nodes.
    # Parsing HTML before writing it in text again is inefficient but
    # is better than custom hacks to find out the correct encoding.
    tree <- htmlParse(x, encoding=encoding)
    con <- textConnection(saveXML(tree), encoding="UTF-8")
    free(tree)
    lines <- readLines(con, encoding="UTF-8")
    close(con)

    # Note that "<a" does not always appears at the beginning of a line
    # in the HTML produced by saveXML()
    newdocs <- grepl("<a name=", lines, fixed=TRUE)

    # Call as.character() to remove useless names and get a vector instead of a 1d array
    content <- as.character(tapply(lines, cumsum(newdocs), paste, collapse="\n"))[-1]

    s <- .Source(readLexisNexisHTML, encoding, length(content), FALSE, NULL, 0, FALSE, class="LexisNexisSource")
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
getElem.LexisNexisSource <- function(x) list(content = x$Content[[x$Position]], uri = x$URI)
eoi.LexisNexisSource <- function(x) length(x$Content) <= x$Position
