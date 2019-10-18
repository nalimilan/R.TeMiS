# Known translation of field names
fields <- list(section=c("section", "rubrique", "rubrik"),
               length=c("length", "longueur", "l\ue4nge"),
               author=c("byline", "auteur", "autor"),
               typepub=c("type", "publication-type", "type-publication"),
               subject=c("subject", "sujet"),
               language=c("language", "langue", "sprache"),
               # The English translation is uncertain for these
               insert=c("insert", "encart"),
               geo=c("geo-localization", "localisation-geo"),
               company=c("company", "societe"),
               stocksymbol=c("stock-symbol", "symbole-boursier"),
               sector=c("activity-sector", "secteur-activite"))

getfield <- function(nodes, field) {
    ind <- which(names(nodes) %in% paste0(toupper(fields[[field]]), ":"))
    if(length(ind) > 0) {
        x <- xml_children(xml_child(nodes[[ind]]))
        if(length(x) > 1)
            xml_text(x[[2]])
        else
            character(0)
    }
    else {
        character(0)
    }
}

splitfield <- function(nodes, field) {
    str <- getfield(nodes, field)
    if(length(str) > 0)
        gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(str, "; ")[[1]])
    else
        character(0)
}

weekdays <- paste(c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
                    "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
                    weekdays(seq(as.Date("2018-01-01"), as.Date("2018-01-07"), by=1))),
                  collapse="|")
months <- paste(c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december",
                  "janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre",
                  months(seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by=31))),
                collapse="|")

readLexisNexisHTML <- FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        # textConnection() in LexisNexisSource() converts strings to UTF-8
        tree <- read_html(elem$content, asText=TRUE, encoding="UTF-8")
        nodes <- xml_find_all(tree, "//br/following-sibling::div[1]")
        nodes <- nodes[sapply(nodes, function(x) length(xml_children(x))) > 0]
        if(xml_text(nodes[[1]], trim=TRUE) == "Return to List")
            nodes <- nodes[-1]

        # Extract first field, which contains meta-data entry type
        names(nodes) <- sapply(nodes, function(x) xml_text(xml_children(xml_children(x)[[1]])[1], trim=TRUE))
        vals <- sapply(nodes, xml_text, trim=TRUE)

        cr <- which(grepl("^Copyright", vals))
        if (any(cr)) {
            copyright <- vals[[max(cr)]]
        } else {
            warning(sprintf("Could not parse copyright notice for article %s. This may indicate a problem with the source data, as LexisNexis copyright notices are nearly universal.\n", id))
            copyright <- NULL
        }

        # First item is the document number
        publication <- vals[2]

        # Date can be before or after heading: try to detect which is which
        datepos <- which(grepl(sprintf("(%s).*[0-9]{4}.*(%s)|(%s) [0-9]{2}, [0-9]{4}", months, weekdays, months),
                               vals[1:5], ignore.case=TRUE))
        if(length(datepos) > 0) {
            date <- vals[datepos[1]]
            heading <- vals[setdiff(1:4, datepos[1])[3]]
        }
        else {
            date <- vals[3]
            heading <- vals[4]
        }

        # Position of main text can vary, so choose the longest part after the heading
        contentpos <- which.max(sapply(tail(vals, -5), nchar)) + 5
        content <- sapply(xml_children(nodes[[contentpos]]), function(x)
                          paste(sapply(xml_children(x), function(y)
                                       paste(trimws(xml_find_all(y, ".//text()")), collapse="\n")),
                                collapse=" "))

        wcstr <- getfield(nodes, "length")
        if(length(wcstr) > 0)
            wc <- as.integer(regmatches(wcstr, regexec("[0-9]+", wcstr))[[1]])
        else
            wc <- NA

        author <- getfield(nodes, "author")
        type <- getfield(nodes, "typepub")
        section <- getfield(nodes, "section")
        intro <- splitfield(nodes, "insert")
        subject <- splitfield(nodes, "subject")
        coverage <- splitfield(nodes, "geo")
        company <- splitfield(nodes, "company")
        stocksymbol <- splitfield(nodes, "stocksymbol")
        industry <- splitfield(nodes, "sector")

        languagestr <- getfield(nodes, "language")
        if(length(languagestr) > 0) {
            language <- strsplit(languagestr, "; ")[[1]][1]
            lang <- ISO_639_2[match(tolower(language), tolower(ISO_639_2[["Name"]])), "Alpha_2"]
            if(is.na(lang))
                lang <- tolower(language)
        }
        else {
            lang <- character(0)
        }

        date.split <- strsplit(date, " ")[[1]]
        date.split <- date.split[date.split != ""]
        strdate <- paste(gsub(",| |\\.", "", date.split[1]),
                         gsub(",| |\\.", "", date.split[2]),
                         gsub(",| |\\.", "", date.split[3]))
        # English uses the first format, French the second one
        date <- strptime(strdate, "%B %d %Y")
        if(is.na(date)) date <- strptime(strdate, "%d %B %Y")
        if(is.na(date) && strdate != "") {
            # Try C locale, just in case
            old.locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "C")
            date <- strptime(strdate, "%B %d %Y")
            if(is.na(date)) date <- strptime(strdate, "%d %B %Y")
            Sys.setlocale("LC_TIME", old.locale)

            # A bug in Mac OS gives NA when start of month name matches an abbreviated name:
            # http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
            # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
            # Add a workaround for French
            if (Sys.info()["sysname"] == "Darwin")
                date <- strptime(sub("[jJ]uillet", "07", strdate), "%d %m %Y")

            if(is.na(date))
                warning(sprintf("Could not parse document date \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", strdate))
        }

        id <- paste(gsub("[^[:alnum:]]", "", substr(publication, 1, 10)),
                    if(!is.na(date)) strftime(date, format="%Y%m%d") else "",
                    id, sep="")

        # XMLSource uses character(0) rather than NA, do the same
        doc <- PlainTextDocument(x = content,
                                 author = if(length(author) > 0 && !is.na(author)) author else character(0),
                                 datetimestamp = date,
                                 heading = if(length(heading) > 0 && !is.na(heading)) heading else character(0),
                                 id = id,
                                 origin = if(length(publication) > 0 && !is.na(publication)) publication else character(0),
                                 language = lang)
        meta(doc, "intro") <- intro
        meta(doc, "section") <- section
        meta(doc, "subject") <- subject
        meta(doc, "coverage") <- coverage
        meta(doc, "company") <- company
        meta(doc, "stocksymbol") <- stocksymbol
        meta(doc, "industry") <- industry
        meta(doc, "type") <- type
        meta(doc, "wordcount") <- wc
        meta(doc, "rights") <- copyright
        doc
    }
})

