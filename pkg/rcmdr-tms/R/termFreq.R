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
                 right=FALSE)

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar(gettext_("Occurrences of term %T"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)

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

        doItAndPrint(paste("absTermFreqs <- as.matrix(dtm[, c(\"", paste(termsList, collapse="\", \""), "\")])", sep=""))
        doItAndPrint(paste("absTermFreqs <- xtabs(cbind(", paste(colnames(absTermFreqs), collapse=", "),
                           ") ~ rownames(absTermFreqs), data=absTermFreqs)", sep=""))

        if(what == "row") {
            doItAndPrint("termFreqs <- absTermFreqs/row_sums(dtm)")
            doItAndPrint("termFreqs <- round(termFreqs*100, d=1)")
            ylab <- gettext_("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreqs <- prop.table(absTermFreqs)")
            else
                doItAndPrint("termFreqs <- prop.table(absTermFreqs, 2)")

            doItAndPrint("termFreqs <- round(termFreqs*100, d=1)")
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
                    if(what == "col") {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                        for(i in 1:ncol(termFreqs)) {
                            doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))
                            if(title != "")
                                doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreqs)[i], title),
                                                   "\")", sep=""))
                        }
                    }
                    else {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(nrow(termFreqs)/2), "))", sep=""))
                        for(i in 1:nrow(termFreqs)) {
                            doItAndPrint(paste("pie(termFreqs[", i, ",])", sep=""))
                            if(title != "")
                            doItAndPrint(paste("title(main=\"", names(dimnames(termFreqs))[1], " ",
                                               rownames(termFreqs)[i], "\")", sep=""))
                        }
                    }
                }
            }
            else {
                if(!is.matrix(termFreqs)) {
                        doItAndPrint(paste("barplot(termFreqs, ylab=\"",  ylab, "\")", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "row") {
                        doItAndPrint(paste("barplot(t(termFreqs), ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=colnames(termFreqs))", sep=""))
                    }
                    else {
                        doItAndPrint(paste("barplot(termFreqs, ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=rownames(termFreqs))", sep=""))
                    }

                    if(!is.matrix(termFreqs))
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
            }
        }

        doItAndPrint("print(termFreqs)")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="docTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Terms to show (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

varTermFreqDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
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
                 right=FALSE)

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar(gettext_("Occurrences of term %T"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)

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
        doItAndPrint(paste("absTermFreqs <- aggregate(as.matrix(dtm[, c(\"",
                           paste(termsList, collapse="\", \""), "\")]), ",
                           "meta(corpus, tag=\"", var, "\"), sum)", sep=""))

        doItAndPrint(paste("absTermFreqs <- xtabs(cbind(", paste(colnames(absTermFreqs)[-1], collapse=", "),
                           ") ~ ., data=absTermFreqs)", sep=""))

        # Compute %
        if(what == "row") {
            doItAndPrint(paste("termFreqs <- absTermFreqs/aggregate(row_sums(dtm), meta(corpus, tag=\"",
                               var, "\"), sum)[,-1]", sep=""))

            doItAndPrint("termFreqs <- round(termFreqs*100, d=1)")
            ylab <- gettext_("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreqs <- prop.table(absTermFreqs)")
            else
                doItAndPrint("termFreqs <- prop.table(absTermFreqs, 2)")

            doItAndPrint("termFreqs <- round(termFreqs*100, d=1)")
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
                    if(what == "col") {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(ncol(termFreqs)/2), "))", sep=""))
                        for(i in 1:ncol(termFreqs)) {
                            doItAndPrint(paste("pie(termFreqs[,", i, "])", sep=""))
                            if(title != "")
                                doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreqs)[i], title),
                                                   "\")", sep=""))
                        }
                    }
                    else {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(nrow(termFreqs)/2), "))", sep=""))
                        for(i in 1:nrow(termFreqs)) {
                            doItAndPrint(paste("pie(termFreqs[", i, ",])", sep=""))
                            if(title != "")
                            doItAndPrint(paste("title(main=\"", names(dimnames(termFreqs))[1], " ",
                                               rownames(termFreqs)[i], "\")", sep=""))
                        }
                    }
                }
            }
            else {
                if(!is.matrix(termFreqs)) {
                        doItAndPrint(paste("barplot(termFreqs, ylab=\"",  ylab, "\")", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "row") {
                        doItAndPrint(paste("barplot(t(termFreqs), ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=colnames(termFreqs))", sep=""))
                    }
                    else {
                        doItAndPrint(paste("barplot(termFreqs, ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=rownames(termFreqs))", sep=""))
                    }

                    if(!is.matrix(termFreqs))
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
            }
        }

        doItAndPrint("print(termFreqs)")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Terms to show (space-separated):")), sticky="w", columnspan=2)
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

copyTermFreq <- function() {
  R2HTML::HTML2clip(termFreqs)
}

