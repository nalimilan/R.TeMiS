# Known translation of field names
fields <- list(section=c("section", "rubrique"),
               length=c("length", "longueur"),
               author=c("byline", "auteur"),
               typepub=c("type", "publication-type", "type-publication"),
               subject=c("subject", "sujet"),
               language=c("language", "langue"),
               # The English translation is uncertain for these
               insert=c("insert", "encart"),
               geo=c("geo-localization", "localisation-geo"),
               company=c("company", "societe"),
               stocksymbol=c("stock-symbol", "symbole-boursier"),
               sector=c("activity-sector", "secteur-activite"))

getfield <- function(nodes, field) {
    ind <- which(names(nodes) %in% paste0(toupper(fields[[field]]), ": "))
    if(length(ind) > 0)
        nodes[[ind]]
    else
        NULL
}

readLexisNexisHTML <- FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        # textConnection() in LexisNexisSource() converts strings to UTF-8
        tree <- htmlParse(elem$content, asText=TRUE, encoding="UTF-8")

        dat <- sapply(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c2']|//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c2']|//div[@class = 'c0']/p[@class = 'c1']/span[@class = 'c2']|//div[@class = 'c10']/p[@class = 'c11']/span[@class = 'c12']|//div[@class = 'c13']/p[@class = 'c11']/span[@class = 'c12']|//div[@class = 'c15']/p[@class = 'c1']/span[@class = 'c14']|//div[@class = 'c0']/p[@class = 'c1']/span[@class = 'c14']|//div[@class = 'c14']/p[@class = 'c15']/span[@class = 'c11']|//div[@class = 'c16']/p[@class = 'c15']/span[@class = 'c11']"),
                      xmlValue)

        # Several types of document
        if(length(dat) == 3) {
            # First item of dat is the document number
            origin <- dat[2]
            copyright <- tail(dat, 1)

            date1 <- sapply(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c4']|//div[@class = 'c13']/p[@class = 'c11']/span[@class = 'c14']"),
                            xmlValue)
            date2 <- sapply(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c2']|//div[@class = 'c13']/p[@class = 'c11']/span[@class = 'c12']"),
                            xmlValue)
        }
        else {
            # First item of dat is the document number
            origin <- dat[2]

            # Needed for some formats
            date1 <- sapply(getNodeSet(tree, "//div[@class = 'c3']/p[@class = 'c1']/span[@class = 'c4']|//div[@class = 'c13']/p[@class = 'c11']/span[@class = 'c14']|//div[@class = 'c15']/p[@class = 'c1']/span[@class = 'c16']"),
                            xmlValue)

            date2 <- dat[3]
            copyright <- tail(dat, 1)
        }

        headingnodes <- getNodeSet(tree, "//div[@class = 'c4']/p[@class = 'c5']/span[@class = 'c6']|//div[@class = 'c5']/p[@class = 'c6']/span[@class = 'c7']|//div[@class = 'c5']/p[@class = 'c6']/span[@class = 'c10']|//div[@class = 'c0']/p[@class = 'c5']/span[@class = 'c14']|//div[@class = 'c0']/p[@class = 'c5']/span[@class = 'c18']")
        heading <- ifelse(length(headingnodes) > 0, xmlValue(headingnodes[[1]]), "")

        nodes <- getNodeSet(tree, "//div[@class = 'c5']/p[@class = 'c6']|//div[@class = 'c5']/p[@class = 'c7']|//div[@class = 'c0']/p[@class = 'c5']")
        names(nodes) <- sapply(nodes, function(x) xmlValue(x[[1]]))


        wc <- xmlValue(getfield(nodes, "length")[[2]])
        author <- xmlValue(getfield(nodes, "author")[[2]])
        type <- xmlValue(getfield(nodes, "typepub")[[2]])
        section <- xmlValue(getfield(nodes, "section")[[2]])
        intro <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "insert")[[2]]), "; ")[[1]])
        subject <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "subject")[[2]]), "; ")[[1]])
        coverage <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "geo")), "; ")[[1]])
        company <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "company")[[2]]), "; ")[[1]])
        stocksymbol <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "stocksymbol")[[2]]), "; ")[[1]])
        industry <- gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(xmlValue(getfield(nodes, "sector")[[2]]), "; ")[[1]])

        language <- strsplit(xmlValue(getfield(nodes, "language")[[2]]), "; ")[[1]][1]
        lang <- ISO_639_2[match(tolower(language), tolower(ISO_639_2[["Name"]])), "Alpha_2"]
        if(is.na(lang))
            lang <- tolower(language)

        if(length(date1) == 0 || is.na(date1)) date1 <- ""
        if(length(date2) == 0 || is.na(date2)) date1 <- ""
        date.split <- strsplit(paste(date1[1], date2[1]), " ")[[1]]
        date.split <- date.split[date.split != ""]
        strdate <- paste(gsub(",| ", "", date.split[1]), gsub(",| ", "", date.split[2]), gsub(",| ", "", date.split[3]))
        # English uses the first format, French the second one
        date <- strptime(strdate, "%B %d %Y")
        if(is.na(date)) date <- strptime(strdate, "%d %B %Y")
        if(is.na(date) && strdate != "") {
            # Try C locale, just in case
            old.locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "C")
            date <- strptime(strdate, "%B %d %Y")
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

        content.nodes <- getNodeSet(tree, "//div[@class = 'c5']/p[@class = 'c9']/span[@class = 'c2']|//div[@class = 'c4']/p[@class = 'c8']/span[@class = 'c2']|//div[@class = 'c5']/p[@class = 'c8']/span[@class = 'c2']|//div[@class = 'c0']/p[@class = 'c16']/span[@class = 'c12']|//div[@class = 'c5']/p[@class = 'c10']/span[@class = 'c2']|//div[@class = 'c0']/p[@class = 'c15']/span[@class = 'c12']|//div[@class = 'c0']/p[@class = 'c20']/span[@class = 'c11']|//div[@class = 'c0']/p[@class = 'c12']/span[@class = 'c11']")
        content <- gsub("\n", "", sapply(content.nodes, xmlValue), fixed=TRUE)

        id <- paste(gsub("[^[:alnum:]]", "", substr(origin, 1, 10)),
                    if(!is.na(date)) strftime(date, format="%Y%m%d") else "",
                    id, sep="")

        free(tree)

        # XMLSource uses character(0) rather than NA, do the same
        doc <- PlainTextDocument(x = content,
                                 author = if(length(author) > 0 && !is.na(author)) author else character(0),
                                 datetimestamp = date,
                                 heading = if(length(heading) > 0 && !is.na(heading)) heading else character(0),
                                 id = id,
                                 origin = if(length(origin) > 0 && !is.na(origin)) origin else character(0),
                                 language = lang)
        meta(doc, "intro") <- if(length(intro) > 0 && !is.na(intro)) intro else character(0)
        meta(doc, "section") <- if(length(section) > 0 && !is.na(section)) section else character(0)
        meta(doc, "subject") <- if(length(subject) > 0 && !all(is.na(subject))) subject else character(0)
        meta(doc, "coverage") <- if(length(coverage) > 0 && !all(is.na(coverage))) coverage else character(0)
        meta(doc, "company") <- if(length(company) > 0 && !all(is.na(company))) company else character(0)
        meta(doc, "stocksymbol") <- if(length(stocksymbol) > 0 && !all(is.na(stocksymbol))) stocksymbol else character(0)
        meta(doc, "industry") <- if(length(industry) > 0 && !all(is.na(industry))) industry else character(0)
        meta(doc, "type") <- if(!length(type) > 0 && is.na(type)) type else character(0)
        meta(doc, "wordcount") <- if(length(wc) > 0 && !is.na(wc)) wc else character(0)
        meta(doc, "rights") <- if(length(copyright) > 0 && !is.na(copyright)) copyright else character(0)
        doc
    }
})

