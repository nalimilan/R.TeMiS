LexisNexisSource <- function(x, encoding = "UTF-8") {
    # This is a fragile method, but much simpler than actually parsing HTML
    # since documents are not a node but a sequence of unrelated nodes.
    # Parsing HTML before writing it in text again is inefficient but
    # is better than custom hacks to find out the correct encoding.
    tree <- read_html(x, encoding=encoding)
    lines <- readLines(textConnection(as.character(tree), encoding="UTF-8"), encoding="UTF-8")

    # Skip tables at the top of the file, if any
    tables <- grep('<table class="c1"', lines, fixed=TRUE, value=FALSE)
    if(length(tables) > 0)
        lines <- lines[-seq(max(tables))]

    # Note that "<a" does not always appear at the beginning of a line
    # in the HTML produced by saveXML()
    newdocs <- grepl('<a name="doc', lines, ignore.case=TRUE)

    # Call as.character() to remove useless names and get a vector instead of a 1d array
    content <- as.character(tapply(lines, cumsum(newdocs), paste, collapse="\n"))[-1]

    # Get rid of short empty sections
    content <- content[nchar(content) > 200]

    # If LexisNexis has generated an error 'document' then we won't be able to handle it; warn and drop
    errtexts <- grepl("We are sorry but there is an error in this document and it is not possible to display it.",
                      content,
                      fixed=TRUE)
    if(any(errtexts)) {
        warning(paste0(x, ": LexisNexis failed to provide some documents; skipping number(s)", as.character(which(errtexts)), "\n", collapse=""))
        content <- content[!errtexts]
    }

    SimpleSource(encoding, length(content),
                 content=content, uri=x,
                 reader=readLexisNexisHTML, class="LexisNexisSource")
}

getElem.LexisNexisSource <- function(x) list(content = x$content[[x$position]], uri = x$URI)
