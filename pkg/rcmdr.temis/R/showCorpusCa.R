## Helper functions to plot a CA restricted to most contributive points

# Get rows contributions to the dimensions of a CA
rowCtr <- function(obj, dim) {
    I <- dim(obj$rowcoord)[1]
    # nd can be lower than actual number of dimensions if matrix does not require them
    K <- min(obj$nd, ncol(obj$rowcoord))
    svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow=TRUE)
    rpc <- obj$rowcoord[,1:K] * svF
    obj$rowmass * rowSums(rpc[,dim, drop=FALSE]^2) / sum(obj$sv[dim]^2)
}

# Get columns contributions to the dimensions of a CA
colCtr <- function(obj, dim) {
    J <- dim(obj$colcoord)[1]
    # nd can be lower than actual number of dimensions if matrix does not require them
    K <- min(obj$nd, ncol(obj$colcoord))
    svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow=TRUE)
    cpc <- obj$colcoord[,1:K] * svG
    obj$colmass * rowSums(cpc[,dim, drop=FALSE]^2) / sum(obj$sv[dim]^2)
}

# Restrain CA to a subset of rows (only for plotting!)
rowSubsetCa <- function(obj, indices) {
    ret <- obj
    for (i in 4:7) {
        ret[[i]] <- list()
        ret[[i]] <- obj[[i]][indices]
    }
    del <- which(!(1:nrow(obj$rowcoord) %in% indices))
    ret$rowsup <- as.numeric(sapply(obj$rowsup[obj$rowsup %in% indices],
                                    function(x) x - sum(del < x)))
    ret$rowcoord <- matrix()
    ret$rowcoord <- obj$rowcoord[indices,,drop=FALSE]
    ret$rownames <- obj$rownames[indices]
    ret
}

# Restrain CA to a subset of columns (only for plotting!)
colSubsetCa <- function(obj, indices) {
    ret <- obj
    for (i in 9:12) {
        ret[[i]] <- list()
        ret[[i]] <- obj[[i]][indices]
    }
    ret$colsup <- ret$colsup[ret$colsup %in% indices]
    ret$colsup <- as.numeric(lapply(obj$colsup, function(x) x - sum(indices < x)))
    ret$colcoord <- matrix()
    ret$colcoord <- obj$colcoord[indices,,drop=FALSE]
    ret$colnames <- obj$colnames[indices]
    ret
}

showCorpusCa <- function(corpusCa, dim=1, ndocs=10, nterms=10) {
    objects <- .getCorpusWindow()
    window <- objects$window
    txt <- objects$txt
    listbox <- objects$listbox

    tkwm.title(window, .gettext("Correspondence Analysis"))

    mark <- 0

    tktag.configure(txt, "heading", font="sans 13 bold")
    tktag.configure(txt, "articlehead", font="sans 12 bold")
    tktag.configure(txt, "details", font="sans 10 italic")
    tktag.configure(txt, "small", font="sans 5")
    tktag.configure(txt, "fixed", font="courier 11")

    cols <- c(.gettext("Position"), .gettext("Contribution (%)"), .gettext("Quality Repr. (%)"))

    tkinsert(txt, "end", paste(.gettext("Axes summary:"), "\n", sep=""), "heading")
    tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
    tkinsert(listbox, "end", .gettext("Axes summary"))
    mark <- mark + 1

    nd <- min(length(corpusCa$sv), corpusCa$nd)
    values <- 100 * (corpusCa$sv[1:nd]^2)/sum(corpusCa$sv^2)
    values2 <- cumsum(values)
    val <- round(rbind(values, values2), digits=1)
    rownames(val) <- c(.gettext("Inertia (%)"), .gettext("Cumulated inertia (%)"))
    colnames(val) <- seq.int(ncol(val))
    names(dimnames(val)) <- c("", "Axis")
    tkinsert(txt, "end", paste(capture.output(val), collapse="\n"), "fixed")

    # Contributions to both axes
    docs <- order(rowCtr(corpusCa, dim), decreasing=TRUE)[1:ndocs]
    docs <- docs[!docs %in% corpusCa$rowsup]

    terms <- order(colCtr(corpusCa, dim), decreasing=TRUE)[1:nterms]
    terms <- terms[!terms %in% corpusCa$colsup]


    for(j in 1:length(dim)) {
        # Per-axis contributions, decreasing order
        docsCtr <- rowCtr(corpusCa, dim[j])
        docs <- docs[order(docsCtr[docs], decreasing=TRUE)]
        docs <- docs[!docs %in% corpusCa$rowsup]

        termsCtr <- colCtr(corpusCa, dim[j])
        terms <- terms[order(termsCtr[terms], decreasing=TRUE)]
        terms <- terms[!terms %in% corpusCa$colsup]


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(.gettext("Most contributive terms on negative side of axis %i:"), dim[j]), "\n", sep=""),
                 "heading")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        tkinsert(listbox, "end", sprintf(.gettext("Axis %i - Negative Side:"), dim[j]))
        tkitemconfigure(listbox, mark, background="grey")
        mark <- mark + 1

        negterms <- terms[corpusCa$colcoord[terms, dim[j]] < 0]
        if(length(negterms) == 0) {
            tkinsert(txt, "end",
                     sprintf(.gettext("None among the %i most contributive terms."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$colnames[negterms],
                             round(corpusCa$colcoord[negterms, dim[j]] * corpusCa$sv[dim[j]], digits=2),
                             round(termsCtr[negterms] * 100, digits=1),
                             round((corpusCa$colcoord[negterms, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$coldist[negterms])^2 * 100, digits=1))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(.gettext("Most contributive documents on negative side of axis %i:"), dim[j]), "\n", sep=""),
                 "heading")
        posdocs <- docs[corpusCa$rowcoord[docs, dim[j]] < 0]
        if(length(posdocs) == 0) {
            tkinsert(txt, "end",
                     sprintf(.gettext("None among the %i most contributive documents."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$rownames[posdocs],
                             round(corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]], digits=2),
                             round(docsCtr[posdocs] * 100, digits=1),
                             round((corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[posdocs])^2 * 100, digits=1))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")

            for(i in posdocs) {
                # We need to use IDs rather than indexes to access documents in the corpus
                # since some documents may have been skipped in the CA
                id <- corpusCa$rownames[i]
                tkinsert(txt, "end", paste("\n\n", id, "\n", sep=""),
                         "articlehead")
                tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
                mark <- mark + 1
                tkinsert(listbox, "end", id)

                origin <- meta(corpus[[id]], "Origin")
                date <- meta(corpus[[id]], "DateTimeStamp")
                if(length(origin) > 0 && length(date) > 0)
                    tkinsert(txt, "end", paste(origin, " - ", date, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(origin, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(date, "\n", sep=""), "details")

                if(length(origin) > 0 || length(date) > 0)
                    tkinsert(txt, "end", "\n", "small")

                tkinsert(txt, "end", paste(paste(corpus[[id]], collapse="\n"), "\n\n"))
            }
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(.gettext("Most contributive terms on positive side of axis %i:"),
                                       dim[j]), "\n", sep=""),
                 "heading")
        tkinsert(listbox, "end", sprintf(.gettext("Axis %i - Positive Side:"), dim[j]))
        tkitemconfigure(listbox, mark, background="grey")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        mark <- mark + 1

        posterms <- terms[corpusCa$colcoord[terms, dim[j]] >= 0]
        if(length(posterms) == 0) {
            tkinsert(txt, "end",
                     sprintf(.gettext("None among the %i most contributive terms."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$colnames[posterms],
                             round(corpusCa$colcoord[posterms, dim[j]] * corpusCa$sv[dim[j]], digits=2),
                             round(termsCtr[posterms] * 100, digits=1),
                             round((corpusCa$colcoord[posterms, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$coldist[posterms])^2 * 100, digits=1))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(.gettext("Most contributive documents on positive side of axis %i:"),
                                       dim[j]), "\n", sep=""),
                 "heading")
        posdocs <- docs[corpusCa$rowcoord[docs, dim[j]] >= 0]
        if(length(posdocs) == 0) {
            tkinsert(txt, "end",
                     sprintf(.gettext("None among the %i most contributive documents."), ndocs))
        }
        else {
            df <- data.frame(row.names=corpusCa$rownames[posdocs],
                             round(corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]], digits=2),
                             round(docsCtr[posdocs] * 100, digits=1),
                             round((corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[posdocs])^2 * 100, digits=1))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")

            for(i in posdocs) {
                # We need to use IDs rather than indexes to access documents in the corpus
                # since some documents may have been skipped in the CA
                id <- corpusCa$rownames[i]
                tkinsert(txt, "end", paste("\n\n", id, "\n", sep=""),
                         "articlehead")
                tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
                mark <- mark + 1
                tkinsert(listbox, "end", id)

                origin <- meta(corpus[[id]], "Origin")
                date <- meta(corpus[[id]], "DateTimeStamp")
                if(length(origin) > 0 && length(date) > 0)
                    tkinsert(txt, "end", paste(origin, " - ", date, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(origin, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(date, "\n", sep=""), "details")

                if(length(origin) > 0 || length(date) > 0)
                    tkinsert(txt, "end", "\n", "small")

                tkinsert(txt, "end", paste(paste(corpus[[id]], collapse="\n"), "\n\n"))
            }
        }


        if(length(corpusCa$rowsup) > 0) {
            tkinsert(txt, "end",
                      paste("\n\n", sprintf(.gettext("Situation of variables on axis %i:"), dim[j]), "\n", sep=""),
                     "heading")
            tkinsert(listbox, "end", sprintf(.gettext("Axis %i - Variables"), dim[j]))
            tkitemconfigure(listbox, mark, background="grey")
            tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
            mark <- mark + 1

            supdocs <- corpusCa$rowsup
            df <- data.frame(row.names=corpusCa$rownames[supdocs],
                             round(corpusCa$rowcoord[supdocs, dim[j]] * corpusCa$sv[dim[j]], digits=2),
                             round((corpusCa$rowcoord[supdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[supdocs])^2 * 100, digits=1))
            colnames(df) <- cols[-2]

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }
    }

    tkraise(window)
}

showCorpusCaDlg <- function() {
    if(!exists("corpusCa") || !class(corpusCa) == "ca") {
        Message(message=.gettext("Please run a correspondence analysis on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Show Correspondence Analysis"))

    dimFrame <- tkframe(top)
    tkgrid(labelRcmdr(dimFrame, text=.gettext("Dimensions to plot:"), fg="blue"), sticky="s")
    tclXDim <- tclVar(1)
    tclYDim <- tclVar(2)
    nd <- min(length(corpusCa$sv), corpusCa$nd)
    xSlider <- tkscale(dimFrame, from=1, to=nd,
                      showvalue=TRUE, variable=tclXDim,
		      resolution=1, orient="horizontal")
    ySlider <- tkscale(dimFrame, from=1, to=nd,
                      showvalue=TRUE, variable=tclYDim,
		      resolution=1, orient="horizontal")

    if(length(corpusCa$rowsup) == 0) {
        checkBoxes(frame="labelsFrame",
                   boxes=c("docLabels", "termLabels"),
                   initialValues=c(0, 1),
                   labels=c(.gettext("Documents"), .gettext("Terms")),
                   title=.gettext("Draw labels for:"))

        checkBoxes(frame="pointsFrame",
                   boxes=c("docPoints", "termPoints"),
                   initialValues=c(0, 1),
                   labels=c(.gettext("Documents"), .gettext("Terms")),
                   title=.gettext("Draw point symbols for:"))
    }
    else {
        checkBoxes(frame="labelsFrame",
                   boxes=c("varLabels", "docLabels", "termLabels"),
                   initialValues=c(0, 0, 1),
                   labels=c(.gettext("Variables"), .gettext("Documents"), .gettext("Terms")),
                   title=.gettext("Draw labels for:"))

        checkBoxes(frame="pointsFrame",
                   boxes=c("varPoints", "docPoints", "termPoints"),
                   initialValues=c(0, 0, 1),
                   labels=c(.gettext("Variables"), .gettext("Documents"), .gettext("Terms")),
                   title=.gettext("Draw point symbols for:"))
    }

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                              selectmode="multiple",
                              title=.gettext("Variables to plot:"),
                              initialSelection=(1:length(vars))-1)

    nFrame <- tkframe(top)
    tkgrid(labelRcmdr(nFrame, text=.gettext("Number of items to plot:"), fg="blue"), sticky="s")
    tclNDocs <- tclVar(25)
    tclNTerms <- tclVar(25)
    docsSlider <- tkscale(nFrame, from=1, to=min(200, nrow(corpusCa$rowcoord)-length(corpusCa$rowsup)),
                          showvalue=TRUE, variable=tclNDocs,
		          resolution=1, orient="horizontal")
    termsSlider <- tkscale(nFrame, from=1, to=min(200, nrow(corpusCa$colcoord)-length(corpusCa$colsup)),
                           showvalue=TRUE, variable=tclNTerms,
		           resolution=1, orient="horizontal")

    ctrDimVariable <- tclVar("xyDim")
    ctrDimFrame <- tkframe(top)
    ctrDim1 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="xyDim", text=.gettext("Both axes"))
    ctrDim2 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="xDim", text=.gettext("Horizontal axis"))
    ctrDim3 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="yDim", text=.gettext("Vertical axis"))


    onShow <- function() {
        x <- tclvalue(tclXDim)
        y <- tclvalue(tclYDim)
        docLabels <- tclvalue(docLabelsVariable) == 1
        termLabels <- tclvalue(termLabelsVariable) == 1
        varLabels <- if(length(corpusCa$rowsup) == 0) FALSE else tclvalue(varLabelsVariable) == 1
        docPoints <- tclvalue(docPointsVariable) == 1
        termPoints <- tclvalue(termPointsVariable) == 1
        varPoints <- if(length(corpusCa$rowsup) == 0) FALSE else tclvalue(varPointsVariable) == 1
        vars <- getSelection(varBox)
        nDocs <- tclvalue(tclNDocs)
        nTerms <- tclvalue(tclNTerms)
        ctrDim <- switch(tclvalue(ctrDimVariable), xyDim=paste("c(", x, ", ", y, ")", sep=""), xDim=x, yDim=y)
        varPoints <- if(length(corpusCa$rowsup) == 0) FALSE else tclvalue(varPointsVariable) == 1
        docPoints <- tclvalue(docPointsVariable) == 1
        termPoints <- tclvalue(termPointsVariable) == 1

        if(!(docLabels || termLabels || varLabels || docPoints || termPoints || varPoints)) {
            Message(.gettext("Please select something to plot."), "error")

            return()
        }

        doItAndPrint(sprintf("showCorpusCa(corpusCa, %s, %s, %s)", ctrDim, nDocs, nTerms))

        if((docLabels || docPoints) && (varLabels || varPoints)) {
            rowWhat <- "all"
            varIndexes <- corpusCa$rowsup[corpusCa$rowsupvars[seq_along(corpusCa$rowsup)] %in% vars]
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, c(order(rowCtr(corpusCa, ", ctrDim,
                               "), decreasing=TRUE)[1:", nDocs, "], ", paste(varIndexes, collapse=", "), "))", sep=""))
        }
        else if(docLabels || docPoints) {
            rowWhat <- "active"
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, order(rowCtr(corpusCa, ", ctrDim,
                               "), decreasing=TRUE)[1:", nDocs, "])", sep=""))
        }
        else if(varLabels || varPoints) {
            rowWhat <- "passive"
            varIndexes <- corpusCa$rowsup[corpusCa$rowsupvars[seq_along(corpusCa$rowsup)] %in% vars]
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, c(",
                               paste(varIndexes, collapse=", "), "))", sep=""))
        }
        else {
            rowWhat <- "none"
        }

        if(((docPoints || docLabels) && (varPoints || varLabels)) && docLabels != varLabels)
            Message(.gettext("Plotting documents and variables at the same time currently forces labels to be drawn for both or none."),
                    "note")

        if(termLabels || termPoints) {
            colWhat <- "all"
            if(docLabels || docPoints || varLabels || varPoints)
                doItAndPrint(paste("plottingCa <- colSubsetCa(plottingCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
            else
                doItAndPrint(paste("plottingCa <- colSubsetCa(corpusCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
        }
        else {
            colWhat <- "none"
        }

        doItAndPrint(sprintf('plotCorpusCa(plottingCa, dim=c(%s, %s), what=c("%s", "%s"), labels=c(%i, %i), pch=c(%s, %s, %s, NA), mass=TRUE, xlab="%s", ylab="%s")',
                              x, y, rowWhat, colWhat,
                              if(docLabels  || varLabels) 2 else 0, if(termLabels) 2 else 0,
                              if(docPoints) 16 else NA, if(varPoints) 1 else NA, if(termPoints) 17 else NA,
                              sprintf(.gettext("Dimension %s (%.1f%%)"), x, 100 * corpusCa$sv[as.integer(x)]^2/sum(corpusCa$sv^2)),
                              sprintf(.gettext("Dimension %s (%.1f%%)"), y, 100 * corpusCa$sv[as.integer(y)]^2/sum(corpusCa$sv^2))))

        activateMenus()
    }

    # Custom buttons, adapted from OKCancelHelp()
    buttonsFrame <- tkframe(top, borderwidth=5)
    plotButton <- buttonRcmdr(buttonsFrame, text=.gettext("Show"), foreground="darkgreen",
                              command=onShow, default="active", borderwidth=3)
    onClose <- function() {
        closeDialog()
        tkfocus(CommanderWindow())
    }
    closeButton <- buttonRcmdr(buttonsFrame, text=.gettext("Close"), foreground="red",
                               command=onClose, borderwidth=3)
    onHelp <- function() {
        if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
        print(help("showCorpusCaDlg"))
    }
    helpButton <- buttonRcmdr(buttonsFrame, text=gettextRcmdr("Help"), width="12",
                              command=onHelp, borderwidth=3)
    tkgrid(plotButton, labelRcmdr(buttonsFrame, text="  "),
           closeButton, labelRcmdr(buttonsFrame, text="            "),
           helpButton, sticky="w")

    tkgrid(labelRcmdr(dimFrame, text=.gettext("Horizontal axis:")), xSlider, sticky="w")
    tkgrid(labelRcmdr(dimFrame, text=.gettext("Vertical axis:")), ySlider, sticky="w")
    tkgrid(dimFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(labelsFrame, pointsFrame, sticky="w", pady=6, padx=c(0, 6))
    if(length(corpusCa$rowsup) > 0)
        tkgrid(getFrame(varBox), columnspan=2, sticky="we", pady=6)
    tkgrid(labelRcmdr(nFrame, text=.gettext("Documents:")), docsSlider, sticky="w")
    tkgrid(labelRcmdr(nFrame, text=.gettext("Terms:")), termsSlider, sticky="w")
    tkgrid(nFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(labelRcmdr(ctrDimFrame, text=.gettext("Most contributive to:")), sticky="w", columnspan=2, pady=6)
    tkgrid(ctrDim1, ctrDim2, ctrDim3, sticky="w", pady=6)
    tkgrid(ctrDimFrame, sticky="w", pady=6, columnspan=2)
    tkgrid.columnconfigure(ctrDimFrame, "all", uniform="a")
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    nrows <- if(length(corpusCa$rowsup) == 0) 6 else 7

    # We don't use dialogSuffix() itself because the dialog should not close,
    # and yet not call tkwait.window(): the plot does not draw correctly on Mac OS if we do.
    # The grab has also been removed since it prevents the user from scrolling the CA text window.
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onShow)
    tkbind(top, "<Escape>", onClose)
    if (getRcmdr("double.click") && (!preventDoubleClick)) tkbind(window, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    tkfocus(top)
    if (getRcmdr("crisp.dialogs")) tclServiceMode(on=TRUE)
}

