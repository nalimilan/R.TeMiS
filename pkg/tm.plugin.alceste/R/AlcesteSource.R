AlcesteSource <- function(x, fileEncoding = "UTF-8", encoding = "unknown") {
    file <- file(x, "r", encoding=fileEncoding)
    on.exit(close(file))

    lines <- readLines(file)

    newdocs <- grepl("^(\\*\\*\\*\\*|[[:digit:]]+ \\*)", lines)
    content <- split(lines, cumsum(newdocs))

    s <- Source(readAlceste, encoding, length(content), NULL, 0, FALSE, "AlcesteSource")
    s$Content <- content
    s$URI <- x
    s
}

# This function is the same as that for XMLSource
getElem.AlcesteSource <- function(x) list(content = x$Content[[x$Position]], uri = x$URI)
