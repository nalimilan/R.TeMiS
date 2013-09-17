readEuropresseHTML <- tm::FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        # Remove annoying line breaks, as <p> is already here to separate paragraphs
        elem$content <- gsub("\r|&#13;|\n", "", elem$content)
        # These markups are used by some sources and create line breaks in the middle
        # of sentences, e.g. with quotations in italics
        elem$content <- gsub("</?(font|b|i).*?>", "", elem$content)
        # This markup is used to mark searches in red and creates the same result
        # Warning: </span> can also close a <span class="DocPublicationName"> tag,
        # within which a <span style=...> tag is sometimes nested:
        # this is why we need such a convoluted regexp
        elem$content <- gsub("<span style=[^>]*>(.*?)</span>", "\\1", elem$content)

        encoding <- if(Encoding(elem$content) == "unknown") character(0)
                    else Encoding(elem$content)
        tree <- htmlParse(elem$content, asText=TRUE, encoding=encoding)

        origin <- xmlValue(getNodeSet(tree, "//span[@class = 'DocPublicationName']")[[1]])
        # Issue n° is sometimes included in the publication name
        origin <- gsub(", no\\..*$", "", origin)

        dat <- sapply(getNodeSet(tree, "//span[@class = 'DocHeader']"), xmlValue)
        dat <- gsub("^, p. |, $|", "", dat)

        # We do not know what is the order of the fields, so try to find the date first
        # Fall back to the third field in case of failure
        date <- strptime(dat, "%d %B %Y")
        datepos <- which(!is.na(date))[1]

        if(is.na(datepos)) {
            # Try C locale: the English case requires a special date format
            old.locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "C")
            date <- strptime(dat, "%B %d, %Y")
            datepos <- which(!is.na(date))[1]
            Sys.setlocale("LC_TIME", old.locale)

            # A bug in Mac OS gives NA when start of month name matches an abbreviated name:
            # http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
            # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
            # Add a workaround for French
            if(is.na(datepos) && Sys.info()["sysname"] == "Darwin") {
                date <- strptime(sub("[jJ]uillet", "07", dat, "%d %m %Y"))
                datepos <- which(!is.na(date))[1]
            }

            if(is.na(datepos))
                warning(sprintf("Could not parse document date from \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", paste(dat, collapse='", "')))
        }

        if(is.na(datepos)) datepos <- 3
        date <- date[datepos]

        if(datepos > 2)
            section <- dat[1]
        else
            section <- character(0)

        # Not needed
        # weekday <- dat[datepos - 1]
        # Pages are not always present, and sometimes time replaces this field
        pages <- if(is.na(dat[datepos + 1]) || grepl("UTC", dat[datepos + 1])) character(0) else dat[datepos + 1]

        heading <- xmlValue(getNodeSet(tree, "//span[@class = 'TitreArticleVisu']")[[1]])

        author <- xmlValue(getNodeSet(tree, "//span[@class = 'TitreArticleVisu']/following::text()[1]")[[1]])
        # Sometimes author is missing or moved to the end of the article:
        # consider that above ten words, we are not dealing with the author name
        if(length(gregexpr(" ", author)[[1]]) > 10)
            author <- character(0)

        # If author is present, skip the corresponding line
        if(length(author) > 0)
            content <- sapply(getNodeSet(tree, "//span[@class = 'TitreArticleVisu']/following::text()[position() > 1]"),
                              xmlValue)
        else
            content <- sapply(getNodeSet(tree, "//span[@class = 'TitreArticleVisu']/following::text()"),
                              xmlValue)

        # Take last matching line using max() in case the content also matches
        copyright <- which(grepl("^\uA9 [[:digit:]]{4}", content))
        if(length(copyright) > 0)
            content <- content[seq(max(copyright) - 1)]

        content <- content[nchar(content) > 0]

        id <- gsub("[^[:alnum:]]|news", "",
                   xmlValue(getNodeSet(tree, "//tr/td[@align = 'center']/text()")[[1]]))
        id <- toupper(substr(id, 1, 20))
        # If extraction failed for some reason, make sure we return a unique identifier
        if(is.na(id))
            id <- paste(if(!is.na(date)) strftime(date, format="%Y%m%d") else "",
                        paste(sample(LETTERS, 10), collapse=""), sep="")

        free(tree)

        # XMLSource uses character(0) rather than NA, do the same
        doc <- tm::PlainTextDocument(x = content,
                                     author = author,
                                     datetimestamp = date,
                                     heading = heading,
                                     id = id,
                                     origin = origin,
                                     language = language)
        tm::meta(doc, "Section") <- section
        tm::meta(doc, "Pages") <- pages
        doc
    }
})

