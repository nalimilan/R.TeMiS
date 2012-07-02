docTermFreqDlg <- function() {
    initializeDialog(title=gettext("Terms Frequencies per Document"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=gettext(c("Row % (term prevalence in category)",
                                  "Column % (distribution of occurrences)",
                                  "Absolute counts")),
                 title="Measure:",
                 right=FALSE)

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text="Draw plot", variable=tclPlotVar)

    tclTitle <- tclVar(gettext("Occurrences of term %T"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)

        if(length(termsList) == 0) {
            errorCondition(recall=varTermFreqDlg,
                           message=gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=varTermFreqDlg,
                           message=sprintf(ngettext(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     paste(wrongTerms, collapse='\', \'')))
            return()
        }

        what <- tclvalue(whatVariable)

        closeDialog()

        doItAndPrint(paste("absTermFreq <- as.matrix(dtm[, c(\"", paste(termsList, collapse="\", \""), "\")])", sep=""))
        doItAndPrint(paste("absTermFreq <- xtabs(cbind(", paste(colnames(absTermFreq), collapse=", "),
                           ") ~ rownames(absTermFreq), data=absTermFreq)", sep=""))

        if(what == "row") {
            doItAndPrint("termFreq <- absTermFreq/row_sums(dtm)")
            doItAndPrint("termFreq <- round(termFreq*100, d=1)")
            ylab <- gettext("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreq <- prop.table(absTermFreq)")
            else
                doItAndPrint("termFreq <- prop.table(absTermFreq, 2)")

            doItAndPrint("termFreq <- round(termFreq*100, d=1)")
            ylab <- gettext("% of occurrences")
        }
        else {
            doItAndPrint("termFreq <- absTermFreq")
            ylab <- gettext("Number of occurrences")
        }

        # Plot
        if(plot == 1) {
           if(what == "col") {
                if(is.na(ncol(termFreq))) {
                        doItAndPrint(paste("pie(termFreq)", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "col") {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(ncol(termFreq)/2), "))", sep=""))
                        for(i in 1:ncol(termFreq)) {
                            doItAndPrint(paste("pie(termFreq[,", i, "])", sep=""))
                            if(title != "")
                                doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreq)[i], title),
                                                   "\")", sep=""))
                        }
                    }
                    else {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(nrow(termFreq)/2), "))", sep=""))
                        for(i in 1:nrow(termFreq)) {
                            doItAndPrint(paste("pie(termFreq[", i, ",])", sep=""))
                            if(title != "")
                            doItAndPrint(paste("title(main=\"", names(dimnames(termFreq))[1], " ",
                                               rownames(termFreq)[i], "\")", sep=""))
                        }
                    }
                }
            }
            else {
                if(is.na(ncol(termFreq))) {
                        doItAndPrint(paste("barplot(termFreq, ylab=\"",  ylab, "\")", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "row") {
                        doItAndPrint(paste("barplot(t(termFreq), ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=colnames(termFreq))", sep=""))
                    }
                    else {
                        doItAndPrint(paste("barplot(termFreq, ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=rownames(termFreq))", sep=""))
                    }

                    if(is.na(ncol(termFreq)))
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
            }
        }

        doItAndPrint("print(termFreq)")

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="docTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext("Terms to show (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

varTermFreqDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext("Corpus has no meta-data. Use Text Mining->Set corpus meta-data to add variables."),
                type="error")
        return()
    }

    initializeDialog(title=gettext("Terms Frequencies per Variable"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                              title=gettext("Meta-data variable:"),
                              initialSelection=0)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=gettext(c("Row % (term prevalence in category)",
                                  "Column % (distribution of occurrences)",
                                  "Absolute counts")),
                 title="Measure:",
                 right=FALSE)

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text="Draw plot", variable=tclPlotVar)

    tclTitle <- tclVar(gettext("Occurrences of term %T"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)

        if(length(termsList) == 0) {
            errorCondition(recall=varTermFreqDlg,
                           message=gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongVars <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=varTermFreqDlg,
                           message=sprintf(ngettext(length(wrongVars),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     paste(wrongVars, collapse='\', \'')))
            return()
        }

        closeDialog()

        what <- tclvalue(whatVariable)

        # Count occurrences
        doItAndPrint(paste("absTermFreq <- aggregate(as.matrix(dtm[, c(\"",
                           paste(termsList, collapse="\", \""), "\")]), ",
                           "meta(corpus, tag=\"", var, "\"), sum)", sep=""))

        doItAndPrint(paste("absTermFreq <- xtabs(cbind(", paste(colnames(absTermFreq)[-1], collapse=", "),
                           ") ~ ., data=absTermFreq)", sep=""))

        # Compute %
        if(what == "row") {
            doItAndPrint(paste("termFreq <- absTermFreq/aggregate(row_sums(dtm), meta(corpus, tag=\"",
                               var, "\"), sum)[,-1]", sep=""))

            doItAndPrint("termFreq <- round(termFreq*100, d=1)")
            ylab <- gettext("% of all terms")
        }
        else if (what == "col") {
            if(length(termsList) == 1)
                doItAndPrint("termFreq <- prop.table(absTermFreq)")
            else
                doItAndPrint("termFreq <- prop.table(absTermFreq, 2)")

            doItAndPrint("termFreq <- round(termFreq*100, d=1)")
            ylab <- gettext("% of occurrences")
        }
        else {
            doItAndPrint("termFreq <- absTermFreq")
            ylab <- gettext("Number of occurrences")
        }

        # Plot
        if(plot == 1) {
           if(what == "col") {
                if(is.na(ncol(termFreq))) {
                        doItAndPrint(paste("pie(termFreq)", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "col") {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(ncol(termFreq)/2), "))", sep=""))
                        for(i in 1:ncol(termFreq)) {
                            doItAndPrint(paste("pie(termFreq[,", i, "])", sep=""))
                            if(title != "")
                                doItAndPrint(paste("title(main=\"", gsub("%T", colnames(termFreq)[i], title),
                                                   "\")", sep=""))
                        }
                    }
                    else {
                        doItAndPrint(paste("par(mfrow=c(2, ", ceiling(nrow(termFreq)/2), "))", sep=""))
                        for(i in 1:nrow(termFreq)) {
                            doItAndPrint(paste("pie(termFreq[", i, ",])", sep=""))
                            if(title != "")
                            doItAndPrint(paste("title(main=\"", names(dimnames(termFreq))[1], " ",
                                               rownames(termFreq)[i], "\")", sep=""))
                        }
                    }
                }
            }
            else {
                if(is.na(ncol(termFreq))) {
                        doItAndPrint(paste("barplot(termFreq, ylab=\"",  ylab, "\")", sep=""))
                        if(title != "")
                            doItAndPrint(paste("title(main=\"", gsub("%T", termsList[1], title),
                                               "\")", sep=""))
                }
                else {
                    if(what == "row") {
                        doItAndPrint(paste("barplot(t(termFreq), ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=colnames(termFreq))", sep=""))
                    }
                    else {
                        doItAndPrint(paste("barplot(termFreq, ylab=\"",  ylab,
                                           "\", beside=TRUE, legend.text=rownames(termFreq))", sep=""))
                    }

                    if(is.na(ncol(termFreq)))
                        doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
                }
            }
        }

        doItAndPrint("print(termFreq)")

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTermFreqDlg")
    tkgrid(labelRcmdr(top, text=gettext("Terms to show (space-separated):")), sticky="w", columnspan=2)
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(whatFrame, sticky="w", columnspan=2, pady=6)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

copyTermFreq <- function() {
  HTML2clip(termFreq)
}

