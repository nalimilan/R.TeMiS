listTerms <- function() {
    doItAndPrint("colnames(dtm)")
}

freqTermsDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext("Show Most Frequent Terms"))
    tclN <- tclVar(10)
    sliderN <- tkscale(top, from=1, to=100,
                       showvalue=TRUE, variable=tclN,
	               resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        n <- as.numeric(tclvalue(tclN))

        doItAndPrint(paste("sort(col_sums(dtm), decreasing=TRUE)[1:", n, "]", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="freqTermsDlg")
    tkgrid(labelRcmdr(top, text=gettext("Number of terms to show:")), sliderN,
           sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2)
}

termsAssocDlg <- function() {
    if(!(exists("dtm") && class(dtm) == "DocumentTermMatrix")) {
        Message(message=gettext("Please import a corpus and create the document-term matrix first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext("Show Associated Terms"))

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
    tkgrid(labelRcmdr(top, text=gettext("Reference terms (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(top, text=gettext("Correlation coefficient (%):")), sliderN, sticky="sw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=4, columns=2, focus=entryTerms)
}

excludeTermsDlg <- function() {
    initializeDialog(title=gettext("Exclude Terms From Analysis"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top, width="30", textvariable=tclTerms)

    onOK <- function() {
        closeDialog()

        termsList <- strsplit(tclvalue(tclTerms), " ")
        doItAndPrint(paste("dtm <- dtm[, !colnames(dtm) %in% c(\"",
                           paste(termsList[[1]], collapse="\", \""), "\")]", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="excludeTermsDlg")
    tkgrid(labelRcmdr(top, text=gettext("Terms to exclude (space-separated):")),
           columnspan=2, sticky="w")
    tkgrid(entryTerms, columnspan=2, sticky="w")
    tkgrid(buttonsFrame, sticky="w", pady=6)
    dialogSuffix(rows=3, columns=1, focus=entryTerms)
}

