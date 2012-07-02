setCorpusVariables <- function() {
    if(!exists("corpus") || !("Corpus" %in% class(corpus))) {
        Message(message=.gettext("Please import a corpus first."),
                type="error")
        return()
    }

    if(!activeDataSetP()) {
        Message(message=.gettext("Please create or import a data set first."),
                type="error")
        return()
    }

    if(!checkVariables()) {
        Message(message=.gettext("Please create at least one variable (column)."),
                type="error")
        return()
    }

    # If corpus was split, we need to replicate variables
    split <- isTRUE(meta(corpus, type="corpus", tag="split"))

    dset <- get(.activeDataSet)
    len <- if (split) length(unique(meta(corpus, .gettext("Doc N"))[[1]])) else length(corpus)
    if(nrow(dset) != len) {
        Message(message=sprintf(.gettext("Active data set must contain exactly %d rows."), len),
                type="error")
        return()
    }

    # Remove dropped and empty variables
    for(var in colnames(meta(corpus))[!colnames(meta(corpus)) %in%
            c(colnames(dset), .gettext("Doc N"), .gettext("Doc ID"), .gettext("Cluster"),
              sapply(corpusVars, function(x) all(is.na(x) | x == "")))])
        doItAndPrint(sprintf('meta(corpus, "%s") <- NULL', var))

    # Add new variables
    indices <- which(sapply(dset, function(x) !all(is.na(x) | x == "", na.rm=TRUE)))

    # We need to call factor(as.character()) so that empty levels are dropped, and new levels are
    # in alphabetical order. This has the drawback that manual reordering cannot be done,
    # but that's the only solution to avoid a total mess for now.
    if(length(indices) > 0) {
        if(split) {
            for(i in indices)
               doItAndPrint(sprintf('meta(corpus, "%s") <- factor(as.character(%s[meta(corpus, "%s")[[1]], %i]))',
                                     colnames(dset)[i], ActiveDataSet(), .gettext("Doc N"), i))
        }
        else {
            for(i in indices)
                doItAndPrint(sprintf('meta(corpus, "%s") <- factor(as.character(%s[[%i]]))',
                                     colnames(dset)[i], ActiveDataSet(), i))
        }
    }

    # Update names only if they changed
    oldDocNames <- if(split) unique(meta(corpus, .gettext("Doc ID"))[[1]]) else names(corpus)
    corpusNames <- names(corpus)
    if(!identical(oldDocNames, row.names(dset))) {
        if(split) {
            doItAndPrint(sprintf('names(corpus) <- make.unique(row.names(%s)[meta(corpus, "%s")[[1]]])',
                                 ActiveDataSet(), .gettext("Doc N")))
            doItAndPrint(sprintf('meta(corpus, "%s") <- row.names(%s)[meta(corpus, "%s")[[1]]]',
                                 .gettext("Doc ID"), ActiveDataSet(), .gettext("Doc N")))
        }
        else {
            doItAndPrint(sprintf('names(corpus) <- row.names(%s)',
                                 ActiveDataSet()))
        }

        # Update the names of the dtm since it affects all operations and cannot be done manually
        # We assume the dtm corresponds to the current corpus if names were identical
        if(identical(corpusNames, rownames(dtm)))
            doItAndPrint("rownames(dtm) <- names(corpus)")

        if(exists("wordsDtm") && identical(corpusNames, rownames(wordsDtm)))
            doItAndPrint("rownames(wordsDtm) <- names(corpus)")
    }
}
