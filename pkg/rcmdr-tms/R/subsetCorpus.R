subsetCorpusByVarDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Subset Corpus by Variable"))

    vars <- colnames(meta(corpus))

    # We cannot use variableListBox as it is not meant for changing levels
    varsFrame <- tkframe(top)
    varsBox <- tklistbox(varsFrame, height=getRcmdr("variable.list.height"),
                         selectmode="single", export=FALSE)
    varsScrollbar <- ttkscrollbar(varsFrame, command=function(...) tkyview(varsBox, ...))
    tkconfigure(varsBox, yscrollcommand=function(...) tkset(varsScrollbar, ...))
    for(var in vars) tkinsert(varsBox, "end", var)

    levelsFrame <- tkframe(top)
    levelsBox <- tklistbox(levelsFrame, height=getRcmdr("variable.list.height"),
                           selectmode=getRcmdr("multiple.select.mode"), export=FALSE)
    levelsScrollbar <- ttkscrollbar(levelsFrame, command=function(...) tkyview(levelsBox, ...))
    tkconfigure(levelsBox, yscrollcommand=function(...) tkset(levelsScrollbar, ...))
    for(level in unique(meta(corpus, vars[1])[[1]])) tkinsert(levelsBox, "end", level)

    onSelect <- function() {
        var <- vars[as.numeric(tkcurselection(varsBox))+1]
        tkdelete(levelsBox, "0", "end")

        levs <- unique(meta(corpus, var)[[1]])
        for(level in levs) tkinsert(levelsBox, "end", level)
    }

    tkbind(varsBox, "<<ListboxSelect>>", onSelect)

    tclSave <- tclVar("1")
    checkSave <- tkcheckbutton(top, text=gettext_("Save original corpus to restore it later"),
                               variable=tclSave)

    onOK <- function() {
        var <- vars[as.numeric(tkcurselection(varsBox))+1]
        levs <- unique(meta(corpus, var)[[1]])[as.numeric(tkcurselection(levelsBox))+1]
        save <- tclvalue(tclSave) == "1"

        closeDialog()

        doItAndPrint(sprintf('keep <- meta(corpus, "%s")[[1]] %%in%% c("%s")',
                             var, paste(levs, collapse='", "')))

        if(save)
            doItAndPrint("origCorpus <- corpus")

        doItAndPrint("corpus <- corpus[keep]")

        if(exists("dtm")) {
            if(save)
                doItAndPrint("origDtm <- dtm")

            doItAndPrint("dtm <- dtm[keep,]")
            doItAndPrint("dtm <- dtm[,col_sums(dtm) > 0]")
        }

        if(exists("wordsDtm")) {
            if(save)
                doItAndPrint("origWordsDtm <- wordsDtm")

            doItAndPrint("wordsDtm <- wordsDtm[keep,]")
            doItAndPrint("wordsDtm <- wordsDtm[,col_sums(wordsDtm) > 0]")
        }

        doItAndPrint("corpusVars <- corpusVars[keep,]")

        # Remove objects left from a previous analysis on the old corpus to avoid confusion
        # (we assume later existing objects match the current corpus)
        objects <- c("keep", "voc", "lengths", "termFreqs", "absTermFreqs", "varTermFreqs",
                     "corpusClust", "corpusSubClust", "corpusCa", "plottingCa")
        doItAndPrint(paste('rm(list=c("', paste(objects[sapply(objects, exists)], collapse='", "'), '"))', sep=""))
        gc()

        doItAndPrint("corpus")
        doItAndPrint("dtm")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="subsetCorpusByVarDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Select a variable and one or more levels to retain:")),
           columnspan=2, sticky="w", pady=6)
    tkgrid(labelRcmdr(varsFrame, text=gettext_("Variable:"), foreground="blue"), sticky="w")
    tkgrid(varsBox, varsScrollbar, sticky="ewns", pady=6)
    tkgrid(labelRcmdr(levelsFrame, text=gettext_("Levels:"), foreground="blue"), sticky="w")
    tkgrid(levelsBox, levelsScrollbar, sticky="ewns", pady=6)
    tkgrid(varsFrame, levelsFrame, sticky="wns", pady=6)
    tkgrid(checkSave, sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=4, columns=2, focus=varsBox)
}


subsetCorpusByTermsDlg <- function() {
    initializeDialog(title=gettext_("Subset Corpus by Terms"))

    tclKeep <- tclVar("")
    entryKeep <- ttkentry(top, width="40", textvariable=tclKeep)

    tclExclude <- tclVar("")
    entryExclude <- ttkentry(top, width="40", textvariable=tclExclude)

    tclSave <- tclVar("1")
    checkSave <- tkcheckbutton(top, text=gettext_("Save original corpus to restore it later"),
                               variable=tclSave)

    onOK <- function() {
        keepList <- strsplit(tclvalue(tclKeep), " ")[[1]]
        excludeList <- strsplit(tclvalue(tclExclude), " ")[[1]]
        save <- tclvalue(tclSave) == "1"

        if(length(keepList) == 0 && length(excludeList) == 0) {
            errorCondition(recall=subsetCorpusByTermsDlg,
                           message=gettext_("Please enter at least one term."))
            return()
        }
        else if(!all(c(keepList, excludeList) %in% colnames(dtm))) {
            wrongTerms <- c(keepList, excludeList)[!c(keepList, excludeList) %in% colnames(dtm)]
            errorCondition(recall=subsetCorpusByTermsDlg,
                           message=sprintf(ngettext_(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=gettext_("\', \'"))))
            return()
        }
        else if(!any(row_sums(dtm[,keepList]) > 0 && row_sums(dtm[,excludeList]) == 0)) {
            errorCondition(recall=subsetCorpusByTermsDlg,
                           message=gettext("Specified conditions would exclude all documents from the corpus."))
            return()
        }

        closeDialog()

        if(length(keepList) > 0 && length(excludeList) > 0)
            doItAndPrint(sprintf('keep <- row_sums(dtm[,c("%s")]) > 0 & row_sums(dtm[,c("%s")]) == 0',
                                 paste(keepList, collapse='", "'), paste(excludeList, collapse='", "')))
        else if(length(keepList) > 0)
            doItAndPrint(sprintf('keep <- row_sums(dtm[,c("%s")]) > 0', paste(keepList, collapse='", "')))
        else
            doItAndPrint(sprintf('keep <- row_sums(dtm[,c("%s")]) == 0', paste(excludeList, collapse='", "')))

        if(save)
            doItAndPrint("origCorpus <- corpus")

        doItAndPrint("corpus <- corpus[keep]")

        if(exists("dtm")) {
            if(save)
                doItAndPrint("origDtm <- dtm")

            doItAndPrint("dtm <- dtm[keep,]")
            doItAndPrint("dtm <- dtm[,col_sums(dtm) > 0]")
        }

        if(exists("wordsDtm")) {
            if(save)
                doItAndPrint("origWordsDtm <- wordsDtm")

            doItAndPrint("wordsDtm <- wordsDtm[keep,]")
            doItAndPrint("wordsDtm <- wordsDtm[,col_sums(wordsDtm) > 0]")
        }

        doItAndPrint("corpusVars <- corpusVars[keep,]")

        # Remove objects left from a previous analysis on the old corpus to avoid confusion
        # (we assume later existing objects match the current corpus)
        objects <- c("keep", "voc", "lengths", "termFreqs", "absTermFreqs", "varTermFreqs",
                     "corpusClust", "corpusSubClust", "corpusCa", "plottingCa")
        doItAndPrint(paste('rm(list=c("', paste(objects[sapply(objects, exists)], collapse='", "'), '"))', sep=""))
        gc()

        doItAndPrint("corpus")
        doItAndPrint("dtm")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="subsetCorpusByTermsDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Keep documents with terms (space-separated):")),
           sticky="w")
    tkgrid(entryKeep, sticky="w")
    tkgrid(labelRcmdr(top, text=gettext_("Exclude documents with terms (space-separated):")),
           sticky="w")
    tkgrid(entryExclude, sticky="w")
    tkgrid(checkSave, sticky="w", pady=6)
    tkgrid(buttonsFrame, sticky="w", pady=6)
    dialogSuffix(rows=6, focus=entryKeep)
}

restoreCorpus <- function() {
    if(!exists("origCorpus"))
        Message(message=gettext_("No original corpus to restore was found."), type="error")

    doItAndPrint("corpus <- origCorpus")

    if(exists("origDtm"))
        doItAndPrint("dtm <- origDtm")

    if(exists("origWordsDtm"))
        doItAndPrint("dtm <- origWordsDtm")


    # Remove objects left from a previous analysis on the subset corpus to avoid confusion
    # (we assume later existing objects match the current corpus)
    objects <- c("keep", "voc", "lengths", "termFreqs", "absTermFreqs", "varTermFreqs",
                 "corpusClust", "corpusSubClust", "corpusCa", "plottingCa",
                 # Also remove backup objects
                 "origCorpus", "origDtm", "origWordsDtm")
    doItAndPrint(paste("rm(", paste(objects[sapply(objects, exists)], collapse=", "), ")", sep=""))

    doItAndPrint("corpus")
    doItAndPrint("dtm")
    gc()
}

