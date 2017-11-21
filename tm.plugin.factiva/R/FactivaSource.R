FactivaSource <- function(x, encoding = "UTF-8", format = c("auto", "XML", "HTML")) {
    format <- match.arg(format)

    # XML format
    if(format == "XML" ||
       (format == "auto" && grepl(".(xml|XML)$", x))) {
        XMLSource(x,
                  function(tree) xml_children(xml_children(xml_children(xml_children(xml_ns_strip(tree))))),
                  readFactivaXML)
    }
    # HTML format
    else {
        tree <- read_html(x, encoding=encoding)

        # The full class is "article XXArticle", with XX the language code
        content <- xml_find_all(tree, "//div[starts-with(@class, 'article ')]")

        SimpleSource(encoding, length=length(content),
                     content=content, uri=x,
                     reader=readFactivaHTML, class="FactivaSource")
    }
}

# This function need to be exactly the same as that for XMLSource
# since it can be used with the Factiva XML source
getElem.FactivaSource <- function(x) list(content = x$content[[x$position]], uri = x$uri)
