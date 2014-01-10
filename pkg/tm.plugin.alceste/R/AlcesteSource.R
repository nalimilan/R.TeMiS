AlcesteSource <- function(x, encoding = "UTF-8") {
    file <- file(x, "r", encoding=encoding)
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
