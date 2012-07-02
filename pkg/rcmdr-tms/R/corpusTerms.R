listTerms <- function() {
    doItAndPrint("colnames(dtm)")
}

freqTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=gettext_("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Show Most Frequent Terms"))
    tclN <- tclVar(10)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    vars <- c(gettext_("None (whole corpus)"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=gettext_("Report results by variable:"),
                              initialSelection=0)

    onOK <- function() {
        var <- getSelection(varBox)
        n <- as.numeric(tclvalue(tclN))
        closeDialog()

        if(var == gettext_("None (whole corpus)"))
            doItAndPrint(paste("sort(col_sums(dtm), decreasing=TRUE)[1:", n, "]", sep=""))
        else
            doItAndPrint(sprintf('tapply(1:nrow(dtm), meta(corpus, "%s"), function(x) sort(col_sums(dtm[x,]), decreasing=TRUE)[1:%i])',
                                 var, n))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="freqTermsDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Number of terms to show:")), sliderN,
           sticky="sw", pady=6)
    tkgrid(getFrame(varBox), columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

termsAssocDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=gettext_("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Show Associated Terms"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top,  width="35", textvariable=tclTerms)

    tclN <- tclVar(80)
    sliderN <- tkscale(top, from=0, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        n <- as.numeric(tclvalue(tclN))
        termsList <- strsplit(tclvalue(tclTerms), " ")

        for(term in termsList[[1]])
            doItAndPrint(paste("findAssocs(dtm, \"", term, "\", ", n/100, ")", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="termsAssocDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Reference terms (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(top, text=gettext_("Correlation coefficient (%):")), sliderN, sticky="sw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

restrictTermsDlg <- function() {
    initializeDialog(title=gettext_("Select or Exclude Terms"))

    radioButtons(name="what",
                 buttons=c("retain", "exclude"),
                 labels=c(gettext_("Retain only these terms"),
                          gettext_("Exclude these terms")),
                 right=FALSE)

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]

        if(length(termsList) == 0) {
            errorCondition(recall=restrictTermsDlg,
                           message=gettext_("Please enter at least one term."))
            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!termsList %in% colnames(dtm)]
            errorCondition(recall=restrictTermsDlg,
                           message=sprintf(ngettext_(length(wrongTerms),
                                                    "Term \'%s\' does not exist in the corpus.",
                                                    "Terms \'%s\' do not exist in the corpus."),
                                                     # TRANSLATORS: this should be opening quote, comma, closing quote
                                                     paste(wrongTerms, collapse=gettext_("\', \'"))))
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
    tkgrid(labelRcmdr(top, text=gettext_("Terms (space-separated):")),
           columnspan=2, sticky="w")
    tkgrid(entryTerms, columnspan=2, sticky="w")
    tkgrid(buttonsFrame, sticky="w", pady=6)
    dialogSuffix(rows=3, columns=1, focus=entryTerms)
}

