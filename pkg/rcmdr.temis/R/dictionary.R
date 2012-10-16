doTermsDictionary <- function() {
    if(is.null(attr(dtm, "words")))
        doItAndPrint("Terms(dtm)")
    else
        doItAndPrint("termsDictionary(dtm)")
}

termsDictionary <- function(dtm) {
    .setBusyCursor()
    on.exit(.setIdleCursor())

    words <- attr(dtm, "words")
    lang <- tm:::map_IETF_Snowball(meta(corpus, type="corpus", tag="language"))
    terms <- SnowballStemmer(words, control=RWeka::Weka_control(S=lang))
    dictionary <- data.frame(row.names=words, terms,
                             ifelse(terms %in% Terms(dtm), " ", .gettext("Removed")))

    names(dictionary) <- c(.gettext("Stemmed term"), .gettext("In matrix"))

    dictionary
}
