readFactivaHTML <- tm::FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        tree <- XML::xmlParse(elem$content, asText=TRUE)

        if(is.na(language)) {
            cl <- XML::xmlAttrs(XML::xmlChildren(tree)[[1]])["class"]
            language <- regmatches(cl, regexec("^article ([[:alpha:]]{2})Article$", cl))[[1]][2]
        }

        table <- XML::readHTMLTable(XML::xmlChildren(tree)[[1]])
        XML::free(tree)

        # Without this, sometimes table ends up being a mere list
        table <- as.data.frame(table)

        vars <- c("AN", "BY", "CY", "ED", "HD", "LA", "LP", "NS",
                  "PD", "PUB", "RE", "SE", "SN", "TD", "WC")

        # Only compare first two chars because of trailing spaces
        data <- as.character(table[match(vars, substr(table[,1], 0, 2)), 2])
        names(data) <- vars

        # Encoding is passed explicitly to work around a bug in XML: htmlParse() and xmlParse() do not set it
        # as they should when asText=TRUE for now
        if(any(Encoding(data) == "unknown"))
            Encoding(data) <- Encoding(elem$content)

        date <- strptime(data[["PD"]], "%d %B %Y")
        if(any(is.na(date) && !data[["PD"]] == "")) {
            # Try C locale, just in case
            old.locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "C")
            date[is.na(date)] <- strptime(data[["PD"]][is.na(date)], "%d %B %Y")
            Sys.setlocale("LC_TIME", old.locale)

            # A bug in Mac OS gives NA when start of month name matches an abbreviated name:
            # http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
            # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
            # Add a workaround for French
            if (Sys.info()["sysname"] == "Darwin")
                date[is.na(date)] <- strptime(sub("[jJ]uillet", "07", data[["PD"]][is.na(date)]), "%d %m %Y")

            if(any(is.na(date) && !data[["PD"]] == ""))
                warning(sprintf("Could not parse document date \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", data[["PD"]]))
        }

        data[["AN"]] <- gsub("Document ", "", data[["AN"]])

        wc <- as.integer(regmatches(data[["WC"]], regexpr("^[[:digit:]]+", data[["WC"]])))[[1]]

        # Merge article header and body, splitting vector into paragraphs
        content <- c(data[["LP"]], strsplit(data[["TD"]], "\\n")[[1]])

        # Remove useless escape sequences
        content <- gsub("\\r", "", content)

        # Extract useful information: origin, date, and three last characters to avoid collisions
        m <- regmatches(data[["AN"]], regexec("^([A-Za-z]+)0*[1-9][0-9]([0-9][0-9][0-3][0-9][0-3][0-9]).*([A-Za-z0-9]{3})$",
                                              data[["AN"]]))[[1]]
        # If extraction failed for some reason, make sure we return a unique identifier
        if(length(m) == 4)
            id <- paste(toupper(m[2]), "-", m[3], "-", m[4], sep="")
        else
            id <- paste(sample(LETTERS, 10), collapse="")

        # XMLSource uses character(0) rather than NA, do the same
        doc <- tm::PlainTextDocument(x = content,
                                     author = if(!is.na(data[["BY"]])) data[["BY"]] else character(0),
                                     datetimestamp = date,
                                     heading = if(!is.na(data[["HD"]])) data[["HD"]] else character(0),
                                     id = id,
                                     origin = if(!is.na(data[["SN"]])) data[["SN"]] else character(0),
                                     language = language)
        tm::meta(doc, "Edition") <- if(!is.na(data[["ED"]])) data[["ED"]] else character(0)
        tm::meta(doc, "Section") <- if(!is.na(data[["SE"]])) data[["SE"]] else character(0)
        tm::meta(doc, "Subject") <- if(!is.na(data[["NS"]])) strsplit(data[["NS"]], "( \\| )")[[1]] else character(0)
        tm::meta(doc, "Coverage") <- if(!is.na(data[["RE"]])) strsplit(data[["RE"]], "( \\| )")[[1]] else character(0)
        tm::meta(doc, "WordCount") <- wc
        tm::meta(doc, "Publisher") <- if(!is.na(data[["PUB"]])) data[["PUB"]] else character(0)
        tm::meta(doc, "Rights") <- if(!is.na(data[["CY"]])) data[["CY"]] else character(0)
        doc
    }
})

