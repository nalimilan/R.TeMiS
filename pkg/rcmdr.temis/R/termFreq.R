docTermFreqDlg <- function() {
    initializeDialog(title=.gettext("Terms Frequencies per Document"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=c(.gettext("Row % (term prevalence in category)"),
                          .gettext("Column % (distribution of occurrences)"),
                          .gettext("Absolute counts")),
                 title=.gettext("Measure:"),
                 right.buttons=FALSE)

    displayFrame <- tkframe(top)

    tclTitle <- tclVar(.gettext("Occurrences of term %T by document"))
    titleEntry <- ttkentry(displayFrame, width="40", textvariable=tclTitle)

    tclPlotVar <- tclVar(1)
    plotButton <- tkcheckbutton(displayFrame, text=.gettext("Draw plot"), variable=tclPlotVar)

    tclTransVar <- tclVar(0)
    transButton <- tkcheckbutton(displayFrame, text=.gettext("Transpose table"), variable=tclTransVar)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)
        trans <- tclvalue(tclTransVar)

        if(length(termsList) == 0) {
            errorCondition(recall=docTermFreqDlg,
                           message=.gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=docTermFreqDlg,
                           message=sprintf(.ngettext(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=.gettext("\', \'"))))
            return()
        }

        what <- tclvalue(whatVariable)

        closeDialog()

        doItAndPrint(paste("absTermFreqs <- as.table(dtm[, c(\"", paste(termsList, collapse="\", \""), "\")])", sep=""))

        doItAndPrint("names(dimnames(absTermFreqs)) <- NULL")

        if(what == "row") {
            doItAndPrint("termFreqs <- absTermFreqs/row_sums(dtm) * 100")
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

                    title <- gsub("%T", termsList[1], title)
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", title,
                                           "\")", sep=""))
                }
                else {
                    doItAndPrint(paste("opar <- par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                    for(i in 1:ncol(termFreqs)) {
                        doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))

                        title <- gsub("%T", colnames(termFreqs)[i], title)
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                    }
                    doItAndPrint("par(opar)")
                }
            }
            else {
                title <- gsub("%T", if(length(termsList) == 1) termsList[1] else "", title)
                doItAndPrint(sprintf('barchart(termFreqs, stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", auto.key=list(space="bottom"))',
                                     ylab, title))
            }
        }

        if(trans == 1 && is.matrix(termFreqs))
            doItAndPrint("termFreqs <- t(termFreqs)")

        # We need more precision for row percents, which are usually small
        if(what == "row")
            doItAndPrint("round(termFreqs, digits=2)")
         else if(what == "col")
            doItAndPrint("round(termFreqs, digits=1)")
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

    OKCancelHelp(helpSubject="docTermFreqDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Terms to show (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(displayFrame, text=.gettext("Display:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(labelRcmdr(displayFrame, text=.gettext("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(transButton, sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(displayFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

varTermFreqDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=.gettext("No corpus variables have been set. Use Text mining->Manage corpus->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Terms Frequencies per Variable"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    vars <- colnames(meta(corpus))
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
            errorCondition(recall=varTermFreqDlg,
                           message=.gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=varTermFreqDlg,
                           message=sprintf(.ngettext(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=.gettext("\', \'"))))
            return()
        }

        closeDialog()

        what <- tclvalue(whatVariable)

        # Count occurrences
        doItAndPrint(sprintf('absTermFreqs <- as.table(rollup(dtm[, c("%s")], 1, meta(corpus, "%s")))',
                             paste(termsList, collapse='", "'), var))

        doItAndPrint("names(dimnames(absTermFreqs)) <- NULL")

        # Compute %
        if(what == "row") {
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

                    title <- gsub("%V", var, gsub("%T", termsList[1], title))
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
                else {
                    doItAndPrint(paste("opar <- par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                    for(i in 1:ncol(termFreqs)) {
                        doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))

                        title <- gsub("%V", var, gsub("%T", colnames(termFreqs)[i], title))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                    }
                    doItAndPrint("par(opar)")
                }
            }
            else {
                title <- gsub("%V", var, gsub("%T", if(length(termsList) == 1) termsList[1] else "", title))
                doItAndPrint(sprintf('barchart(termFreqs, stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", auto.key=list(space="bottom"))',
                                     ylab, title))
            }
        }

        if(trans == 1 && is.matrix(termFreqs))
            doItAndPrint("termFreqs <- t(termFreqs)")

        # We need more precision for row percents, which are usually small
        if(what == "row")
            doItAndPrint("round(termFreqs, digits=2)")
         else if(what == "col")
            doItAndPrint("round(termFreqs, digits=1)")
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

    OKCancelHelp(helpSubject="varTermFreqDlg")
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

