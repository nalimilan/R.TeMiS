cooccurrentTerms <- function(term, dtm, variable=NULL, p=0.1, n.max=25, sparsity=0.95, min.occ=2) {
    if(!term %in% colnames(dtm))
         stop(.gettext("'term' is not present in 'dtm'"))

    if(is.null(variable)) {
        specTerms <- specificTerms(dtm, as.vector(dtm[, term] > 0),
                                   p=p, n.max=n.max, sparsity=sparsity, min.occ=min.occ)[["TRUE"]]
        colnames(specTerms) <- c(.gettext("% Term/Cooc."), .gettext("% Cooc./Term"), .gettext("Global %"),
                                 .gettext("Cooc."), .gettext("Global"),
                                 .gettext("t value"), .gettext("Prob."))
        specTerms
    }
    else {
        sapply(levels(factor(variable)), simplify=FALSE, function(l) {
            subDtm <- dtm[variable == l,]
            if(sum(subDtm[, term]) == 0) {
                NA
            }
            else {
                specTerms <- specificTerms(subDtm, as.vector(subDtm[, term] > 0),
                                           p=p, n.max=n.max, sparsity=sparsity, min.occ=min.occ)[["TRUE"]]
                colnames(specTerms) <- c(.gettext("% Term/Cooc."), .gettext("% Cooc./Term"), .gettext("Level %"),
                                         .gettext("Cooc."), .gettext("Level"),
                                         .gettext("t value"), .gettext("Prob."))
                specTerms
            }
        })
    }
}

termCoocDlg <- function() {
    if(!(exists("dtm") && inherits(dtm, "DocumentTermMatrix"))) {
        .Message(message=.gettext("Please import a corpus and create the document-term matrix first."),
                 type="error")
        return()
    }

    initializeDialog(title=.gettext("Terms Co-occurring With Chosen Terms"))

    tclTerms <- tclVar("")
    entryTerms <- ttkentry(top,  width="35", textvariable=tclTerms)

    vars <- c(.gettext("None (whole corpus)"), colnames(meta(corpus)))
    varBox <- variableListBox(top, vars,
                              title=.gettext("Report results by variable:"),
                              initialSelection=0)

    tclP <- tclVar(10)
    spinP <- tkwidget(top, type="spinbox", from=0, to=100,
                      inc=1, textvariable=tclP,
                      validate="all", validatecommand=.validate.unum)

    tclOcc <- tclVar(2)
    spinOcc <- tkwidget(top, type="spinbox", from=1, to=.Machine$integer.max,
                        inc=1, textvariable=tclOcc,
                        validate="all", validatecommand=.validate.uint)

    tclN <- tclVar(25)
    spinN <- tkwidget(top, type="spinbox", from=1, to=.Machine$integer.max,
                      inc=1, textvariable=tclN,
                      validate="all", validatecommand=.validate.uint)

    onOK <- function() {
        termsList <- strsplit(tclvalue(tclTerms), " ")[[1]]
        var <- getSelection(varBox)
        p <- as.numeric(tclvalue(tclP))
        occ <- as.numeric(tclvalue(tclOcc))
        n <- as.numeric(tclvalue(tclN))

        if(length(termsList) == 0) {
            .Message(gettext("Please enter at least one term."), "error", parent=top)

            return()
        }
        else if(!all(termsList %in% colnames(dtm))) {
            wrongTerms <- termsList[!(termsList %in% colnames(dtm))]
            .Message(sprintf(.ngettext(length(wrongTerms),
                                      "Term \'%s\' does not exist in the corpus.",
                                      "Terms \'%s\' do not exist in the corpus."),
                                       # TRANSLATORS: this should be opening quote, comma, closing quote
                                       paste(wrongTerms, collapse=.gettext("\', \'"))),
                     "error", parent=top)

            return()
        }

        if(var != .gettext("None (whole corpus)") && length(unique(meta(corpus, var)[[1]])) < 2) {
            .Message(.gettext("Please choose a variable with at least two levels."), "error", parent=top)
            return()
        }

        closeDialog()

        setBusyCursor()
        on.exit(setIdleCursor())

        if(var == .gettext("None (whole corpus)")) {
            if(length(termsList) == 1)
                doItAndPrint(sprintf('coocs <- cooccurrentTerms("%s", dtm, p=%s, min.occ=%s, n.max=%s)',
                                     termsList, p/100, occ, n))
            else
                doItAndPrint(sprintf('coocs <- sapply(c("%s"), cooccurrentTerms, dtm, p=%s, min.occ=%s, n.max=%s, simplify=FALSE)',
                                     paste(termsList, collapse='", "'), p/100, occ, n))
        }
        else {
            if(length(termsList) == 1)
                doItAndPrint(sprintf('coocs <- cooccurrentTerms("%s", dtm, meta(corpus, "%s")[[1]], p=%s, min.occ=%s, n.max=%s)',
                                     termsList, var, p/100, occ, n))
            else
                doItAndPrint(sprintf('coocs <- sapply(c("%s"), cooccurrentTerms, dtm, meta(corpus, "%s")[[1]], p=%s, min.occ=%s, n.max=%s, simplify=FALSE)',
                                     paste(termsList, collapse='", "'), var, p/100, occ, n))
        }

        title <- sprintf(.ngettext(length(termsList),
                                   "Terms associated with term \"%s\"",
                                   "Terms associated with terms \"%s\""),
                         # TRANSLATORS: this should be opening quote, comma, closing quote
                         paste(termsList, collapse=.gettext("\", \"")), n)

       if(var != .gettext("None (whole corpus)"))
           setLastTable("coocs", paste(title, sprintf(.gettext("(for %s)"),
                                     paste(levels(factor(meta(corpus, var)[[1]])),
                                           collapse=", "))))
       else
           setLastTable("coocs", title)

        doItAndPrint("coocs")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="termsCoocDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Reference terms (space-separated):")), sticky="w")
    tkgrid(entryTerms, sticky="w", columnspan=2)
    tkgrid(getFrame(varBox), columnspan=2, sticky="w", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Show terms with a probability below (%):")), spinP,
           sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Only retain terms with a number of occurrences above:")), spinOcc,
           sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Maximum number of terms to show per level:")), spinN,
           sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan=2, sticky="ew", pady=6)
    dialogSuffix(focus=entryTerms)
}
