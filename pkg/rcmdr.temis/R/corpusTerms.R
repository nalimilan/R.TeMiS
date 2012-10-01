listTerms <- function() {
    doItAndPrint("colnames(dtm)")
}

freqTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Most Frequent Terms"))
    tclN <- tclVar(10)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    vars <- c(.gettext("None (whole corpus)"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Report results by variable:"),
                              initialSelection=0)

    onOK <- function() {
        var <- getSelection(varBox)
        n <- as.numeric(tclvalue(tclN))

        closeDialog()

        if(var == .gettext("None (whole corpus)"))
            doItAndPrint(paste("freqTerms <- sort(col_sums(dtm), decreasing=TRUE)[1:", n, "]", sep=""))
        else
            doItAndPrint(sprintf('freqTerms <- tapply(1:nrow(dtm), meta(corpus, "%s"), function(x) sort(col_sums(dtm[x,]), decreasing=TRUE)[1:%i])',
                                 var, n))

        doItAndPrint("freqTerms")

        # Used by saveTableToOutput()
        last.table <<- "freqTerms"
        if(var == .gettext("None (whole corpus)"))
            attr(freqTerms, "title") <<- .gettext("Most frequent terms in the corpus")
        else
            attr(freqTerms, "title") <<- sprintf(.gettext("Most frequent terms by %s"), var)

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="freqTermsDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Number of terms to show:")), sliderN,
           sticky="sw", pady=6)
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

termsAssocDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Associated Terms"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top,  width="35", textvariable=tclTerms)

    tclN <- tclVar(30)
    sliderN <- tkscale(top, from=0, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    vars <- c(.gettext("None (whole corpus)"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Report results by variable:"),
                              initialSelection=0)

    onOK <- function() {
        n <- as.numeric(tclvalue(tclN))
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)

        if(length(termsList) == 0) {
            Message(gettext("Please enter at least one term."), "error")

            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            Message(sprintf(.ngettext(length(wrongTerms),
                                      "Term \'%s\' does not exist in the corpus.",
                                      "Terms \'%s\' do not exist in the corpus."),
                                       # TRANSLATORS: this should be opening quote, comma, closing quote
                                       paste(wrongTerms, collapse=.gettext("\', \'"))),
                    "error")

            return()
        }

        closeDialog()

        .setBusyCursor()

        if(var == .gettext("None (whole corpus)")) {
            for(term in termsList) {
                doItAndPrint(sprintf('termsAssoc <- findAssocs(dtm, "%s", %s)', term, n/100))
                doItAndPrint("print(termsAssoc)")
            }
        }
        else {
            for(term in termsList) {
                doItAndPrint(sprintf('termsAssoc <- sapply(levels(factor(meta(corpus, "%s")[[1]])), function(l)\nfindAssocs(dtm[meta(corpus, "%s")[[1]] == l,], "%s", %s))',
                                     var, var, term, n/100))
                doItAndPrint("print(termsAssoc)")
            }
        }

        # Used by saveTableToOutput()
        last.table <<- "termsAssoc"
        title <- sprintf(.ngettext(length(termsList),
                                   "Terms associated with term \"%s\" at more than %s%%",
                                   "Terms associated with terms \"%s\" at more than %s%%"),
                         # TRANSLATORS: this should be opening quote, comma, closing quote
                         paste(termsList, collapse=.gettext("\", \"")), n)

       if(var != .gettext("None (whole corpus)"))
           attr(termsAssoc, "title") <<- paste(title, sprintf(.gettext("(for %s)"),
                                                              paste(levels(factor(meta(corpus, var)[[1]])),
                                                                    collapse=", ")))
       else
           attr(termsAssoc, "title") <<- title


        .setIdleCursor()
        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="termsAssocDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Reference terms (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(top, text=.gettext("Correlation coefficient (%):")), sliderN, sticky="sw", pady=6)
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

specificTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Specific Terms"))
    tclN <- tclVar(10)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    vars <- c(.gettext("Document"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Specific of levels of variable:"),
                              initialSelection=0)

    onOK <- function() {
        var <- getSelection(varBox)
        n <- as.numeric(tclvalue(tclN))
        closeDialog()

        if(var == .gettext("Document")) {
            doItAndPrint("expected <- row_sums(dtm) %o% col_sums(dtm)/sum(dtm)")
            doItAndPrint("chisq <- sign(as.matrix(dtm - expected)) *  as.matrix((dtm - expected)^2/expected)")
            doItAndPrint(sprintf("specificTerms <- sapply(rownames(dtm), simplify=FALSE, USE.NAMES=TRUE, function(x) round(chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[1:%i]]))", n))
            doItAndPrint("rm(expected, chisq)")
        }
        else {
            doItAndPrint(sprintf('specificDtm <- rollup(dtm, 1, meta(corpus, "%s"))', var))
            doItAndPrint("expected <- row_sums(specificDtm) %o% col_sums(specificDtm)/sum(specificDtm)")
            doItAndPrint("chisq <- sign(as.matrix(specificDtm - expected)) *  as.matrix((specificDtm - expected)^2/expected)")
            doItAndPrint(sprintf("specificTerms <- sapply(rownames(specificDtm), simplify=FALSE, USE.NAMES=TRUE, function(x) round(chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[1:%i]]))", n))
            doItAndPrint("rm(specificDtm, expected, chisq)")
        }

        doItAndPrint("specificTerms")

        # Used by saveTableToOutput()
        last.table <<- "specificTerms"
        if(var == .gettext("Document"))
            attr(specificTerms, "title") <<- .gettext("Most specific terms by document")
        else
            attr(specificTerms, "title") <<- sprintf(.gettext("Most specific terms by %s"), var)

        activateMenus()

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="specificTermsDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Number of terms to show:")), sliderN,
           sticky="sw", pady=6)
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

restrictTermsDlg <- function() {
    initializeDialog(title=.gettext("Select or Exclude Terms"))

    radioButtons(name="what",
                 buttons=c("retain", "exclude"),
                 labels=c(.gettext("Retain only these terms"),
                          .gettext("Exclude these terms")),
                 right.buttons=FALSE)

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]

        if(length(termsList) == 0) {
            Message(.gettext("Please enter at least one term."), "error")

            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!termsList %in% colnames(dtm)]
            Message(sprintf(.ngettext(length(wrongTerms),
                                      "Term \'%s\' does not exist in the corpus.",
                                      "Terms \'%s\' do not exist in the corpus."),
                                      # TRANSLATORS: this should be opening quote, comma, closing quote
                            paste(wrongTerms, collapse=.gettext("\', \'"))),
                    "error")

            return()
        }

        closeDialog()


        what <- tclvalue(whatVariable)
        if(what == "retain")
            doItAndPrint(paste("dtm <- dtm[, colnames(dtm) %in% c(\"",
                               paste(termsList, collapse="\", \""), "\")]", sep=""))
        else
            doItAndPrint(paste("dtm <- dtm[, !colnames(dtm) %in% c(\"",
                               paste(termsList, collapse="\", \""), "\")]", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="restrictTermsDlg")
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Terms (space-separated):")),
           columnspan=2, sticky="w")
    tkgrid(entryTerms, columnspan=2, sticky="w")
    tkgrid(buttonsFrame, sticky="w", pady=6)
    dialogSuffix(rows=3, columns=1, focus=entryTerms)
}

termFreqDlg <- function() {
    initializeDialog(title=.gettext("Frequency of Specific Terms"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    vars <- c(.gettext("Document"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Variable:"),
                              initialSelection=0)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=c(.gettext("Row % (term prevalence in category)"),
                          .gettext("Column % (distribution of occurrences)"),
                          .gettext("Absolute counts")),
                 title=.gettext("Measure:"),
                 right.buttons=FALSE)

    displayFrame <- tkframe(top)

    tclTitle <- tclVar(.gettext("Occurrences of term %T by %V"))
    titleEntry <- ttkentry(displayFrame, width="40", textvariable=tclTitle)

    tclPlotVar <- tclVar(1)
    plotButton <- tkcheckbutton(displayFrame, text=.gettext("Draw plot"), variable=tclPlotVar)

    tclTransVar <- tclVar(0)
    transButton <- tkcheckbutton(displayFrame, text=.gettext("Transpose table"), variable=tclTransVar)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)
        trans <- tclvalue(tclTransVar)

        if(length(termsList) == 0) {
            Message(.gettext("Please enter at least one term."), "error")

            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            Message(sprintf(.ngettext(length(wrongTerms),
                                      "Term \'%s\' does not exist in the corpus.",
                                      "Terms \'%s\' do not exist in the corpus."),
                                      # TRANSLATORS: this should be opening quote, comma, closing quote
                                      paste(wrongTerms, collapse=.gettext("\', \'"))),
                    "error")

            return()
        }

        closeDialog()

        what <- tclvalue(whatVariable)

        # Count occurrences
        if(var == .gettext("Document"))
            doItAndPrint(sprintf('absTermFreqs <- as.table(dtm[, c("%s")])',
                                 paste(termsList, collapse='", "')))
        else
            doItAndPrint(sprintf('absTermFreqs <- as.table(rollup(dtm[, c("%s")], 1, meta(corpus, "%s")))',
                                 paste(termsList, collapse='", "'), var))

        doItAndPrint("names(dimnames(absTermFreqs)) <- NULL")

        # Compute %
        if(what == "row") {
            if(var == .gettext("Document"))
                doItAndPrint("termFreqs <- absTermFreqs/row_sums(dtm) * 100")
            else
                doItAndPrint(sprintf('termFreqs <- absTermFreqs/c(tapply(row_sums(dtm), meta(corpus, "%s"), sum)) * 100',
                                     var))

            ylab <- .gettext("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreqs <- prop.table(absTermFreqs) * 100")
            else
                doItAndPrint("termFreqs <- prop.table(absTermFreqs, 2) * 100")

            ylab <- .gettext("% of occurrences")
        }
        else {
            doItAndPrint("termFreqs <- absTermFreqs")
            ylab <- .gettext("Number of occurrences")
        }

        # Plot
        if(plot == 1) {
           if(what == "col") {
                if(!is.matrix(termFreqs)) {
                    doItAndPrint(paste("pie(termFreqs)", sep=""))

                    title <- gsub("%V", tolower(var), gsub("%T", termsList[1], title))
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
                else {
                    doItAndPrint(paste("opar <- par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                    for(i in 1:ncol(termFreqs)) {
                        doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))

                        title <- gsub("%V", tolower(var), gsub("%T", tolower(colnames(termFreqs)[i]), title))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                    }
                    doItAndPrint("par(opar)")
                }
            }
            else {
                if(length(termsList) == 1)
                    title <- gsub("%T", termsList[1], title)
                else
                    title <- gsub(" %T ", " ", title)

                title <- gsub("%V", tolower(var), title)

                doItAndPrint(sprintf('barchart(termFreqs, stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", auto.key=list(space="bottom"))',
                                     ylab, title))
            }
        }

        if(trans == 1 && is.matrix(termFreqs))
            doItAndPrint("termFreqs <- t(termFreqs)")

        # We need more precision for row percents, which are usually small
        if(what == "row")
            doItAndPrint("print(termFreqs, digits=2)")
         else if(what == "col")
            doItAndPrint("print(termFreqs, digits=1)")
         else
            doItAndPrint("print(termFreqs)")

        # Used by saveTableToOutput()
        last.table <<- "termFreqs"
        if(what == "row")
            attr(termFreqs, "title") <<- paste(title, .gettext("(% of all terms)"))
        else if(what == "col")
            attr(termFreqs, "title") <<- paste(title, .gettext("(% of occurrences)"))
        else
            attr(termFreqs, "title") <<- title


        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="termFreqDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Terms to show (space-separated):")), sticky="w", columnspan=2)
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(displayFrame, text=.gettext("Display:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(labelRcmdr(displayFrame, text=.gettext("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(transButton, sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(displayFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

