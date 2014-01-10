EuropresseSource <- function(x, encoding = "UTF-8") {
    tree <- htmlParse(x, encoding=encoding)

    content <- getNodeSet(tree, "/html/body/table/tbody/tr/td")

    # Some HTML files do not have <tbody> (depending on the browser?)
    if(length(content) == 0)
        content <- getNodeSet(tree, "/html/body/table/tr/td")

    free(tree)

    s <- Source(readEuropresseHTML, encoding, length(content), NULL, 0, FALSE, "EuropresseSource")
    s$Content <- content
    s$URI <- x
    s
}

# This functions is the same as that for XMLSource
getElem.EuropresseSource <- function(x) list(content = saveXML(x$Content[[x$Position]]), uri = x$URI)
