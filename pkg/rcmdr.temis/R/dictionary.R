termsDictionaryAlpha <- function() {
    doItAndPrint('dict <- termsDictionary(dtm)')

    setLastTable("dict", .gettext("Terms dictionary sorted alphabetically"))

    doItAndPrint("dict")

    activateMenus()
}

termsDictionaryOcc <- function() {
    doItAndPrint('dict <- termsDictionary(dtm, "occurrences")')

    setLastTable("dict", .gettext("Terms dictionary sorted by number of occurrences"))

    doItAndPrint("dict")

    activateMenus()
}

termsDictionary <- function(dtm, order=c("alphabetic", "occurrences")) {
    order <- match.arg(order)

    setBusyCursor()
    on.exit(setIdleCursor())

    lang <- attr(dtm, "language")
    words <- attr(dtm, "words")

    if(is.null(words))
        words <- col_sums(dtm)

    processing <- attr(dtm, "processing")
    if(is.null(processing))
        processing <- c(stemming=TRUE, custom.stemming=FALSE, stopwords=FALSE)

    stopword <- names(words) %in% stopwords(lang)

    if(!processing["stemming"] && !processing["custom.stemming"]) {
        dictionary <- data.frame(row.names=names(words), words,
                                 ifelse(stopword, .gettext("Stopword"), ""),
                                 ifelse(!names(words) %in% Terms(dtm), .gettext("Removed"), ""))

        names(dictionary) <- c(.gettext("Occurrences"), .gettext("Stopword?"), .gettext("Removed?"))


        if(order == "occurrences")
            dictionary[order(dictionary[, 1], decreasing=TRUE),]
        else
            dictionary
    }
    else {
        if(processing["custom.stemming"]) {
            stemming.dictionary <- attr(dtm, "stemming.dictionary")
            terms <- stemming.dictionary[[2]]
        }
        else {
            terms <- SnowballC::wordStem(names(words), language=lang)
        }

        dictionary <- data.frame(row.names=names(words), words,
                                 terms, col_sums(dtm)[ifelse(terms %in% Terms(dtm), terms, NA)],
                                 ifelse(stopword, .gettext("Stopword"), ""),
                                 # Some words can be removed as stopwords, but be present because another
                                 # word that has been kept is identical in its stemmed from
                                 ifelse(!terms %in% Terms(dtm) | (stopword & processing["stopwords"]),
                                        .gettext("Removed"), ""))

        names(dictionary) <- c(.gettext("Occurrences"), .gettext("Stemmed term"), .gettext("Stemmed occ."),
                               .gettext("Stopword?"), .gettext("Removed?"))

        if(order == "occurrences")
            dictionary[order(dictionary[, 3], decreasing=TRUE),]
        else
            dictionary
    }
}
