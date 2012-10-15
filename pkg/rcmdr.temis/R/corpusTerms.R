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

termChisqDist <- function(term, dtm, n=5, variable=NULL) {
    if(!term %in% colnames(dtm))
         stop("'term' is not present in 'dtm'")

    dev <- sweep(as.matrix(dtm)/col_sums(dtm), 1,
                 as.matrix(dtm[, term])/sum(dtm[, term]), "-")
    chisq <- sweep(dev^2, 1, row_sums(dtm)/sum(dtm), "/")

    # na.rm=TRUE is here because some empty documents might exist, even if it's useless
    if(is.null(variable)) {
        head(sort(colSums(chisq, na.rm=TRUE)), n)
    }
    else {
        sapply(levels(factor(variable)), function(l) {
            if(sum(dtm[variable == l, term]) == 0)
                NA
            else
                head(sort(colSums(chisq[variable == l, , drop=FALSE], na.rm=TRUE)), n)
        })
    }
}

termsCoocDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Terms Co-occurrences"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top,  width="35", textvariable=tclTerms)

    tclN <- tclVar(10)
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
        on.exit(.setIdleCursor())

        if(var == .gettext("None (whole corpus)")) {
            if(length(termsList) == 1)
                doItAndPrint(sprintf('coocs <- termChisqDist("%s", dtm, %i)', termsList, n))
            else
                doItAndPrint(sprintf('coocs <- sapply(c("%s"), termChisqDist, dtm, %i, simplify=FALSE)',
                                     paste(termsList, collapse='", "'), n))

            doItAndPrint("coocs")
        }
        else {
            if(length(termsList) == 1)
                doItAndPrint(sprintf('coocs <- termChisqDist("%s", dtm, %i, meta(corpus, "%s")[[1]])',
                                     termsList, n, var))
            else
                doItAndPrint(sprintf('coocs <- sapply(c("%s"), termChisqDist, dtm, %i, meta(corpus, "%s")[[1]], simplify=FALSE)',
                                     paste(termsList, collapse='", "'), n, var))

            doItAndPrint("coocs")
        }

        # Used by saveTableToOutput()
        last.table <<- "coocs"
        title <- sprintf(.ngettext(length(termsList),
                                   "Terms associated with term \"%s\" according to Chi-squared distance",
                                   "Terms associated with terms \"%s\" according to Chi-squared distance"),
                         # TRANSLATORS: this should be opening quote, comma, closing quote
                         paste(termsList, collapse=.gettext("\", \"")), n)

       if(var != .gettext("None (whole corpus)"))
           attr(coocs, "title") <<- paste(title, sprintf(.gettext("(for %s)"),
                                                              paste(levels(factor(meta(corpus, var)[[1]])),
                                                                    collapse=", ")))
       else
           attr(coocs, "title") <<- title


        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="termsCoocDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Reference terms (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(top, text=.gettext("Number of terms to show:")), sliderN, sticky="sw", pady=6)
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=5, columns=2, focus=entryTerms)
}

specificTerms <- function(variable, dtm, p=0.1, n.max=25, sparsity=0.95, min.occ=2) {
    if(!is.null(variable) && length(unique(variable)) < 2)
        stop("Please provide a variable with at least two levels.")

    if(sparsity < 1)
        dtm <- removeSparseTerms(dtm, sparsity)

    if(min.occ > 1)
        dtm <- dtm[, col_sums(dtm) >= min.occ]

    if(!is.null(variable))
        dtm <- rollup(dtm, 1, variable)

    rs <- row_sums(dtm)
    cs <- col_sums(dtm)
    tot <- sum(rs)
    cs.tot <- cs/tot

    sapply(rownames(dtm), simplify=FALSE, function(l) {
        # rownames(dtm) == l is used below because "" is a possible level
        i <- rownames(dtm) == l

        rp <- as.matrix(dtm[l,]/rs[l])
        cp <- as.matrix(dtm[l,])/cs
        sup <- rp > cs.tot

        counts <- as.matrix(dtm[i,])[1,]

        # As this is a discrete distribution, we need to subtract one
        # to include the value when switching sides
        counts <- ifelse(sup, counts - 1, counts)

        p.val <- phyper(counts, rs[l], tot - rs[l], cs)
        t.val <- qnorm(p.val)

        p.val[sup] <- 1 - p.val[sup]
        p.val <- 2 * p.val

        keep <- which(p.val <= p)

        if(length(keep) == 0) return(numeric(0))

        ord <- head(intersect(order(p.val), keep), n.max)
        ret <- cbind(term.clus=rp[ord] * 100, clus.term=cp[ord] * 100,
                     p.global=cs[ord]/tot * 100, n.global=cs[ord],
                     t.value=t.val[ord], p.value=round(p.val[ord], 4))
        colnames(ret) <- c(.gettext("% Term/Level"), .gettext("% Level/Term"), .gettext("Global %"), .gettext("Global freq."),
                           .gettext("t value"), .gettext("Prob."))
        ret
    })
}

specificTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Specific Terms"))

    vars <- c(.gettext("Document"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Show terms specific of levels of variable:"),
                              initialSelection=0)

    tclP <- tclVar(10)
    sliderP <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclP,
	               resolution=1, orient="horizontal")

    tclN <- tclVar(25)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    tclOcc <- tclVar(5)
    sliderOcc <- tkscale(top, from=1, to=50,
                         showvalue=TRUE, variable=tclOcc,
	                 resolution=1, orient="horizontal")

    onOK <- function() {
        var <- getSelection(varBox)
        n <- as.numeric(tclvalue(tclN))
        p <- as.numeric(tclvalue(tclP))
        occ <- as.numeric(tclvalue(tclOcc))
        closeDialog()

        if(var == .gettext("Document")) {
            doItAndPrint(sprintf('specTerms <- specificTerms(NULL, dtm, p=%s, min.occ=%s, n.max=%s)',
                                 p/100, occ, n))
        }
        else {
            doItAndPrint(sprintf('specTerms <- specificTerms(meta(corpus, "%s")[[1]], dtm, p=%s, min.occ=%s, n.max=%s)',
                                 var, p/100, occ, n))
        }

        doItAndPrint("specTerms")

        # Used by saveTableToOutput()
        last.table <<- "specTerms"
        if(var == .gettext("Document"))
            attr(specTerms, "title") <<- .gettext("Specific terms by document")
        else
            attr(specTerms, "title") <<- sprintf(.gettext("Specific terms by %s"), var)

        activateMenus()

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="specificTermsDlg")
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Show terms with a probability below (%):")), sliderN,
           sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Maximum number of terms to show per level:")), sliderP,
           sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Retain terms with a number of occurrences above:")), sliderOcc,
           sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=4, columns=2)
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
    initializeDialog(title=.gettext("Term Frequencies"))

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

