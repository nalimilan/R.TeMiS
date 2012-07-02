docTermFreqDlg <- function() {
    initializeDialog(title=gettext_("Terms Frequencies per Document"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=c(gettext_("Row % (term prevalence in category)"),
                          gettext_("Column % (distribution of occurrences)"),
                          gettext_("Absolute counts")),
                 title=gettext_("Measure:"),
                 right.buttons=FALSE)

    displayFrame <- tkframe(top)

    tclPlotVar <- tclVar(1)
    plotButton <- tkcheckbutton(displayFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar(gettext_("Occurrences of term %T"))
    titleEntry <- ttkentry(displayFrame, width="20", textvariable=tclTitle)

    tclTransVar <- tclVar(0)
    transButton <- tkcheckbutton(displayFrame, text=gettext_("Transpose table"), variable=tclTransVar)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)
        trans <- tclvalue(tclTransVar)

        if(length(termsList) == 0) {
            errorCondition(recall=docTermFreqDlg,
                           message=gettext_("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=docTermFreqDlg,
                           message=sprintf(ngettext_(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=gettext_("\', \'"))))
            return()
        }

        what <- tclvalue(whatVariable)

        closeDialog()

        doItAndPrint(paste("absTermFreqs <- as.table(dtm[, c(\"", paste(termsList, collapse="\", \""), "\")])", sep=""))

        doItAndPrint("names(dimnames(absTermFreqs)) <- NULL")

        if(what == "row") {
            doItAndPrint("termFreqs <- absTermFreqs/row_sums(dtm) * 100")
            ylab <- gettext_("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreqs <- prop.table(absTermFreqs) * 100")
            else
                doItAndPrint("termFreqs <- prop.table(absTermFreqs, 2) * 100")

            ylab <- gettext_("% of occurrences")
        }
        else {
            doItAndPrint("termFreqs <- absTermFreqs")
            ylab <- gettext_("Number of occurrences")
        }

        # Plot
        if(plot == 1) {
           if(what == "col") {
                if(!is.matrix(termFreqs)) {
                    doItAndPrint(paste("pie(termFreqs)", sep=""))
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                           "\")", sep=""))
                }
                else {
                    doItAndPrint(paste("opar <- par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                    for(i in 1:ncol(termFreqs)) {
                        doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreqs)[i], title),
                                               "\")", sep=""))
                    }
                    doItAndPrint("par(opar)")
                }
            }
            else {
                doItAndPrint(sprintf('barchart(termFreqs, stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", auto.key=list(space="bottom"))',
                                     ylab, gsub("%T", if(length(termsList) == 1) termsList[1] else "", title)))
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

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="docTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Terms to show (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(displayFrame, text=gettext_("Display:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(transButton, sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(displayFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(displayFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

varTermFreqDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Manage corpus->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Terms Frequencies per Variable"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                              title=gettext_("Variable:"),
                              initialSelection=0)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=c(gettext_("Row % (term prevalence in category)"),
                          gettext_("Column % (distribution of occurrences)"),
                          gettext_("Absolute counts")),
                 title=gettext_("Measure:"),
                 right.buttons=FALSE)

    displayFrame <- tkframe(top)
    tclPlotVar <- tclVar(1)
    plotButton <- tkcheckbutton(displayFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar(gettext_("Occurrences of term %T"))
    titleEntry <- ttkentry(displayFrame, width="20", textvariable=tclTitle)

    tclTransVar <- tclVar(0)
    transButton <- tkcheckbutton(displayFrame, text=gettext_("Transpose table"), variable=tclTransVar)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)
        trans <- tclvalue(tclTransVar)

        if(length(termsList) == 0) {
            errorCondition(recall=varTermFreqDlg,
                           message=gettext_("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongVars <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=varTermFreqDlg,
                           message=sprintf(ngettext_(length(wrongVars),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=gettext_("\', \'"))))
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

            ylab <- gettext_("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreqs <- prop.table(absTermFreqs) * 100")
            else
                doItAndPrint("termFreqs <- prop.table(absTermFreqs, 2) * 100")

            ylab <- gettext_("% of occurrences")
        }
        else {
            doItAndPrint("termFreqs <- absTermFreqs")
            ylab <- gettext_("Number of occurrences")
        }

        # Plot
        if(plot == 1) {
           if(what == "col") {
                if(!is.matrix(termFreqs)) {
                    doItAndPrint(paste("pie(termFreqs)", sep=""))
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                           "\")", sep=""))
                }
                else {
                    doItAndPrint(paste("opar <- par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                    for(i in 1:ncol(termFreqs)) {
                        doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreqs)[i], title),
                                               "\")", sep=""))
                    }
                    doItAndPrint("par(opar)")
                }
            }
            else {
                doItAndPrint(sprintf('barchart(termFreqs, stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", auto.key=list(space="bottom"))',
                                     ylab, gsub("%T", if(length(termsList) == 1) termsList[1] else "", title)))
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

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Terms to show (space-separated):")), sticky="w", columnspan=2)
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(displayFrame, text=gettext_("Display:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(transButton, sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(displayFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(displayFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

copyTermFreq <- function() {
  R2HTML::HTML2clip(round(termFreqs, digits=2))
}

