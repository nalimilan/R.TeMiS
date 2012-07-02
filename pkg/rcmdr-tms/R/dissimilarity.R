corpusDissimilarity <- function(x, y) {
    if(!(inherits(x, "simple_triplet_matrix") && inherits(y, "simple_triplet_matrix")))
        stop("x and y must be simple_triplet_matrix objects")

    if(!(length(dim(x)) == 2 && length(dim(y)) == 2))
        stop("x and y must have two dimensions")

    if(!isTRUE(all.equal(col_sums(x), col_sums(y))))
        stop("x and y must have the same column marginals")

    d <- matrix(NA, nrow(x), nrow(y), dimnames=list(rownames(x), rownames(y)))
    colProbs <- col_sums(x)/sum(x)
    xrSums <- row_sums(x)
    y <- as.matrix(y)/row_sums(y)

    for(i in 1:nrow(x)) {
        dev <- sweep(y, 2, as.matrix(x[i,])/xrSums[i], "-")
        d[i,] <- rowSums(sweep(dev^2, 2, colProbs, "/"))
    }

    d
}

dissimilarityTableDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Manage corpus->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Documents/Variables Dissimilarity"))

    vars <- c(gettext_("Document"), colnames(meta(corpus)))
    varBox1 <- variableListBox(top, vars,
                               title=gettext_("Row variable:"),
                               initialSelection=0)

    varBox2 <- variableListBox(top, vars,
                               title=gettext_("Column variable:"),
                               initialSelection=0)

    # Set the second variable identical to the first one by default,
    # as it's the most common operation
    onSelect <- function() {
        tkselection.clear(varBox2$listbox, tclvalue(tkcurselection(varBox2$listbox)))
        tkselection.set(varBox2$listbox, tclvalue(tkcurselection(varBox1$listbox)))
    }

    tkbind(varBox1$listbox, "<<ListboxSelect>>", onSelect)

    onOK <- function() {
        var1 <- getSelection(varBox1)
        var2 <- getSelection(varBox2)

        closeDialog()
        setBusyCursor()

        if(var1 == var2) {
            if(var1 == gettext_("Document")) {
                doItAndPrint('diss <- dist(sweep(dtm/row_sums(dtm), 2, sqrt(sum(dtm)/col_sums(dtm)), "*"))')
            }
            else {
                doItAndPrint(sprintf('dissDtm <- rollup(dtm, 1, meta(corpus, "%s"))', var1))
                doItAndPrint('diss <- dist(sweep(dissDtm/row_sums(dissDtm), 2, sqrt(sum(dissDtm)/col_sums(dissDtm)), "*"))')
                doItAndPrint('rm(dissDtm)')
            }
        }
        else {
            if(var1 == gettext_("Document")) {
                doItAndPrint(sprintf('dissDtm2 <- rollup(dtm, 1, meta(corpus, "%s"))', var2))
                doItAndPrint('diss <- corpusDissimilarity(dtm, dissDtm2)')
                doItAndPrint('rm(dissDtm2)')
            }
            else if(var2 == gettext_("Document")) {
                doItAndPrint(sprintf('dissDtm1 <- rollup(dtm, 1, meta(corpus, "%s"))', var1))
                doItAndPrint('diss <- corpusDissimilarity(dissDtm1, dtm)')
                doItAndPrint('rm(dissDtm1)')
            }
            else {
                doItAndPrint(sprintf('dissDtm1 <- rollup(dtm, 1, meta(corpus, "%s"))', var1))
                doItAndPrint(sprintf('dissDtm2 <- rollup(dtm, 1, meta(corpus, "%s"))', var2))
                doItAndPrint('diss <- corpusDissimilarity(dissDtm1, dissDtm2)')
                doItAndPrint('rm(dissDtm1, dissDtm2)')
            }
        }

        doItAndPrint("round(diss, digits=1)")

        activateMenus()

        setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="dissimilarityTableDlg")
    tkgrid(getFrame(varBox1), sticky="w", pady=6, columnspan=3)
    tkgrid(getFrame(varBox2), sticky="w", pady=6, columnspan=3)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=3)
    dialogSuffix(rows=3, columns=3, focus=varBox1$listbox)
}

