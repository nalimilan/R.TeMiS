AlcesteSource <- function(x, encoding = "UTF-8") {
    file <- file(x, "r", encoding=encoding)
    on.exit(close(file))

    lines <- readLines(file)

    newdocs <- grepl("^(\\*\\*\\*\\*|[[:digit:]]+ \\*)", lines)
    content <- split(lines, cumsum(newdocs))

    s <- .Source(readAlceste, encoding, length(content), FALSE, NULL, 0, FALSE, class="AlcesteSource")
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
getElem.AlcesteSource <- function(x) list(content = x$Content[[x$Position]], uri = x$URI)
eoi.AlcesteSource <- function(x) length(x$Content) <= x$Position
