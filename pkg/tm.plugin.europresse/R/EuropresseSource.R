EuropresseSource <- function(x, encoding = "UTF-8") {
    tree <- htmlParse(x, encoding=encoding)

    content <- getNodeSet(tree, "/html/body/table/tbody/tr/td")

    # Some HTML files do not have <tbody> (depending on the browser?)
    if(length(content) == 0)
        content <- getNodeSet(tree, "/html/body/table/tr/td")

    free(tree)

    SimpleSource(encoding, length(content),
                 content=content, uri=x,
                 reader=readEuropresseHTML, class="EuropresseSource")
}

# This functions is the same as that for XMLSource
getElem.EuropresseSource <- function(x) list(content = saveXML(x$content[[x$position]]), uri = x$URI)
