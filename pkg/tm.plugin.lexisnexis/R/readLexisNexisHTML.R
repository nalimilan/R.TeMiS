readLexisNexisHTML <- tm::FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        # textConnection() in LexisNexisSource() converts strings to UTF-8
        tree <- htmlParse(elem$content, asText=TRUE, encoding="UTF-8")

        dat <- sapply(getNodeSet(tree, "//div[@class = 'c0']/p[@class = 'c1']/span[@class = 'c2']"),
                      xmlValue)

        # First item of dat is the document number
        origin <- dat[2]
        copyright <- dat[3]

        heading <- xmlValue(getNodeSet(tree, "//div[@class = 'c5']/p[@class = 'c6']/span[@class = 'c7']")[[1]])

        nodes <- getNodeSet(tree, "//div[@class = 'c5']/p[@class = 'c6']")
        names(nodes) <- sapply(nodes, function(x) xmlValue(x[[1]]))

        section.page <- xmlValue(nodes[["RUBRIQUE: "]][[2]])
        section.page <- strsplit("; Pg.", section.page)[[1]]
        section <- section.page[2]
        page <- section.page[3]

        wc <- xmlValue(nodes[["LONGUEUR: "]][[2]])
        author <- xmlValue(nodes[["AUTEUR: "]][[2]])
        type <- xmlValue(nodes[["TYPE-PUBLICATION: "]][[2]])
        intro <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["ENCART: "]][[2]]), "; ")[[1]])
        subject <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["SUJET: "]][[2]]), "; ")[[1]])
        coverage <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["LOCALISATION-GEO: "]][[2]]), "; ")[[1]])
        company <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["SOCIETE: "]][[2]]), "; ")[[1]])
        stocksymbol <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["SYMBOLE-BOURSIER: "]][[2]]), "; ")[[1]])
        industry <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(nodes[["SECTEUR-ACTIVITE: "]][[2]]), "; ")[[1]])

        language <- strsplit(xmlValue(nodes[["LANGUE: "]][[2]]), "; ")[[1]][1]
        lang <- ISO_639_2[match(tolower(language), tolower(ISO_639_2[["Name"]])), "Alpha_2"]
        if(is.na(lang))
            lang <- tolower(language)

        day <- xmlValue(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c4']")[[1]])
        monthyear <- xmlValue(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c2']")[[1]])
        monthyear.split <- strsplit(monthyear, " ")[[1]][-1]
        month <- monthyear.split[1]
        year <- monthyear.split[2]
        # Thid part of monthyear.split is the weekday

        strdate <- paste(day, month, year)
        date <- strptime(strdate, "%d %B %Y")

        if(is.na(date) && strdate != "") {
            # Try C locale, just in case
            old.locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "C")
            date <- strptime(paste(day, month, year), "%d %B %Y")
            Sys.setlocale("LC_TIME", old.locale)

            # A bug in Mac OS gives NA when start of month name matches an abbreviated name:
            # http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
            # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
            # Add a workaround for French
            if (Sys.info()["sysname"] == "Darwin")
                date <- strptime(sub("[jJ]uillet", "07", strdate, "%d %m %Y"))

            if(is.na(date))
                warning(sprintf("Could not parse document date \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", strdate))
        }

        content <- gsub("\n", "", sapply(getNodeSet(tree, "//div[@class = 'c5']/p[@class = 'c9']/span[@class = 'c2']"),
                                         xmlValue),
                        fixed=TRUE)

        id <- paste(gsub("[^[:alnum:]]", "", substr(origin, 1, 10)),
                    if(!is.na(date)) strftime(date, format="%Y%m%d") else "",
                    id, sep="")

        free(tree)

        # XMLSource uses character(0) rather than NA, do the same
        doc <- tm::PlainTextDocument(x = content,
                                     author = if(!is.na(author)) author else character(0),
                                     datetimestamp = date,
                                     heading = if(!is.na(heading)) heading else character(0),,
                                     id = id,
                                     origin = if(!is.na(origin)) origin else character(0),,
                                     language = lang)
        tm::meta(doc, "Introduction") <- if(!is.na(intro)) intro else character(0)
        tm::meta(doc, "Section") <- if(!is.na(section)) section else character(0)
        tm::meta(doc, "Subject") <- if(!all(is.na(subject))) subject else character(0)
        tm::meta(doc, "Coverage") <- if(!all(is.na(coverage))) coverage else character(0)
        tm::meta(doc, "Company") <- if(!all(is.na(company))) company else character(0)
        tm::meta(doc, "StockSymbol") <- if(!all(is.na(stocksymbol))) stocksymbol else character(0)
        tm::meta(doc, "Industry") <- if(!all(is.na(industry))) industry else character(0)
        tm::meta(doc, "Type") <- if(!is.na(type)) type else character(0)
        tm::meta(doc, "WordCount") <- if(!is.na(wc)) wc else character(0)
        tm::meta(doc, "Pages") <- if(!is.na(page)) page else character(0)
        tm::meta(doc, "Rights") <- if(!is.na(copyright)) copyright else character(0)
        doc
    }
})

