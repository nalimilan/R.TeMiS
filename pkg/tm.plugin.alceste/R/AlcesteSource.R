AlcesteSource <- function(x, encoding = "auto") {
    if(encoding == "auto")
        encoding <- stringi::stri_enc_detect(readBin(x, "raw", 1024))[[1]]$Encoding[1]

    if(is.null(encoding))
        encoding <- ""

    lines <- iconv(readLines(x, warn=FALSE),
                   from=encoding, to="UTF-8", sub="byte")

    newdocs <- grepl("^(\\*\\*\\*\\*|[[:digit:]]+ \\*)", lines)
    content <- split(lines, cumsum(newdocs))

    s <- Source(readAlceste, encoding, length(content), NULL, 0, FALSE, "AlcesteSource")
    s$Content <- content
    s$URI <- x
    s
}

# This function is the same as that for XMLSource
getElem.AlcesteSource <- function(x) list(content = x$Content[[x$Position]], uri = x$URI)
