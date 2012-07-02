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
            doItAndPrint(paste("sort(col_sums(dtm), decreasing=TRUE)[1:", n, "]", sep=""))
        else
            doItAndPrint(sprintf('tapply(1:nrow(dtm), meta(corpus, "%s"), function(x) sort(col_sums(dtm[x,]), decreasing=TRUE)[1:%i])',
                                 var, n))

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
            errorCondition(recall=termsAssocDlg,
                           message=.gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            errorCondition(recall=termsAssocDlg,
                           message=sprintf(.ngettext(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=.gettext("\', \'"))))
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
                                   'Terms associated with term "%s" at more than %s%%',
                                   'Terms associated with terms "%s" at more than %s%%'),
                         # TRANSLATORS: this should be opening quote, comma, closing quote
                         paste(termsList, collapse=.gettext('", "')), n)

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

typicalTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Most Typical Terms"))
    tclN <- tclVar(10)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    vars <- c(.gettext("Per document"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Report results by variable:"),
                              initialSelection=0)

    onOK <- function() {
        var <- getSelection(varBox)
        n <- as.numeric(tclvalue(tclN))
        closeDialog()

        if(var == .gettext("Per document")) {
            doItAndPrint("expected <- row_sums(dtm) %o% col_sums(dtm)/sum(dtm)")
            doItAndPrint("chisq <- sign(as.matrix(dtm - expected)) *  as.matrix((dtm - expected)^2/expected)")
            doItAndPrint(sprintf("sapply(rownames(dtm), simplify=FALSE, USE.NAMES=TRUE, function(x) round(chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[1:%i]]))", n))
            doItAndPrint("rm(expected, chisq)")
        }
        else {
            doItAndPrint(sprintf('typicalDtm <- rollup(dtm, 1, meta(corpus, "%s"))', var))
            doItAndPrint("expected <- row_sums(typicalDtm) %o% col_sums(typicalDtm)/sum(typicalDtm)")
            doItAndPrint("chisq <- sign(as.matrix(typicalDtm - expected)) *  as.matrix((typicalDtm - expected)^2/expected)")
            doItAndPrint(sprintf("sapply(rownames(typicalDtm), simplify=FALSE, USE.NAMES=TRUE, function(x) round(chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[1:%i]]))", n))
            doItAndPrint("rm(typicalDtm, expected, chisq)")
        }

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="typicalTermsDlg")
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
            errorCondition(recall=restrictTermsDlg,
                           message=.gettext("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!termsList %in% colnames(dtm)]
            errorCondition(recall=restrictTermsDlg,
                           message=sprintf(.ngettext(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=.gettext("\', \'"))))
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

