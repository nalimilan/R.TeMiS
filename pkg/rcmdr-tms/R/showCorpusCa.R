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
rowSubsetCa <- function(ca, indices) {
    ret <- ca
    for (i in 4:7) {
        ret[[i]] <- list()
        ret[[i]] <- ca[[i]][indices]
    }
    del <- which(!(1:nrow(ca$rowcoord) %in% indices))
    ret$rowsup <- as.numeric(sapply(ca$rowsup[ca$rowsup %in% indices],
                                    function(x) x - sum(del < x)))
    ret$rowcoord <- matrix()
    ret$rowcoord <- ca$rowcoord[indices,,drop=FALSE]
    ret$rownames <- ca$rownames[indices]
    ret
}

# Restrain CA to a subset of columns (only for plotting!)
colSubsetCa <- function(ca, indices) {
    ret <- ca
    for (i in 9:12) {
        ret[[i]] <- list()
        ret[[i]] <- ca[[i]][indices]
    }
    ret$colsup <- ret$colsup[ret$colsup %in% indices]
    ret$colsup <- as.numeric(lapply(ca$colsup, function(x) x - sum(indices < x)))
    ret$colcoord <- matrix()
    ret$colcoord <- ca$colcoord[indices,,drop=FALSE]
    ret$colnames <- ca$colnames[indices]
    ret
}

showCorpusCa <- function(corpusCa, dim=1, ndocs=10, nterms=10) {
    if(exists("corpusCaTxt", "RcmdrEnv") &&
       !is.null(get("corpusCaTxt", "RcmdrEnv"))) {
        window <- getRcmdr("corpusCaWindow")
        txt <- getRcmdr("corpusCaTxt")
        listbox <- getRcmdr("corpusCaList")
        tkdelete(txt, "0.0", "end")
        tkdelete(listbox, 0, "end")
        tkraise(window)
    }
    else {
        window <- tktoplevel(class="Rcommander")
        tkwm.title(window, gettext_("Correspondence Analysis"))
        tkwm.geometry(window, "-0+20")
        scr1 <- tkscrollbar(window, repeatinterval=5,
                           command=function(...) tkyview(txt,...))
        txt <- tktext(window, bg="white", font="times", wrap="word",
                      yscrollcommand=function(...) tkset(scr1, ...))

        tkpack(txt, side="left", fill="both", expand=TRUE)
        tkpack(scr1, side="left", fill="y")

        scr2 <- tkscrollbar(window, repeatinterval=5,
                            command=function(...) tkyview(tl,...))
        listbox <- tklistbox(window, selectmode="single",
                             yscrollcommand=function(...) tkset(scr2,...))
        tkpack(listbox, side="left", fill="y")
        tkpack(scr2, side="left", fill="y")

        tkbind(listbox, "<<ListboxSelect>>", function() {
            tkyview(txt, paste("mark", tkcurselection(listbox), sep=""))
        })

        putRcmdr("corpusCaWindow", window)
        putRcmdr("corpusCaTxt", txt)
        putRcmdr("corpusCaList", listbox)

	tkwm.protocol(window, "WM_DELETE_WINDOW", function() {
            tkdestroy(getRcmdr("corpusCaWindow"))
            putRcmdr("corpusCaWindow", NULL)
            putRcmdr("corpusCaTxt", NULL)
            putRcmdr("corpusCaList", NULL)
        })
    }

    mark <- 0

    tktag.configure(txt, "heading", font="sans 13 bold")
    tktag.configure(txt, "articlehead", font="sans 12 bold")
    tktag.configure(txt, "fixed", font="courier 11")

    cols <- c(gettext_("Position"), gettext_("Abs. Contr."), gettext_("Rel. Contr."))

    tkinsert(txt, "end", paste(gettext_("Axes information:"), "\n", sep=""), "heading")
    tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
    tkinsert(listbox, "end", gettext_("Axes information"))
    mark <- mark + 1

    values <- 100 * (corpusCa$sv[1:corpusCa$nd]^2)/sum(corpusCa$sv^2)
    values2 <- cumsum(values)
    val <- round(rbind(values[1:corpusCa$nd], values2))
    rownames(val) <- c(gettext_("Inertia (%)"), gettext_("Cumulated inertia (%)"))
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
                 paste("\n\n", sprintf(gettext_("Most contributive terms on negative side of axis %i:"), dim[j]), "\n", sep=""),
                 "heading")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        tkinsert(listbox, "end", sprintf(gettext_("Axis %i - Negative Side:"), dim[j]))
        tkitemconfigure(listbox, mark, background="grey")
        mark <- mark + 1

        negterms <- terms[corpusCa$colcoord[terms, dim[j]] < 0]
        if(length(negterms) == 0) {
            tkinsert(txt, "end",
                     sprintf(gettext_("None among the %i most contributive terms."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$colnames[negterms],
                             round(corpusCa$colcoord[negterms, dim[j]] * corpusCa$sv[dim[j]], d=2),
                             round(termsCtr[negterms] * 1000),
                             round((corpusCa$colcoord[negterms, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$coldist[negterms])^2 * 1000))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(gettext_("Most contributive documents on negative side of axis %i:"), dim[j]), "\n", sep=""),
                 "heading")
        posdocs <- docs[corpusCa$rowcoord[docs, dim[j]] < 0]
        if(length(posdocs) == 0) {
            tkinsert(txt, "end",
                     sprintf(gettext_("None among the %i most contributive documents."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$rownames[posdocs],
                             round(corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]], d=2),
                             round(docsCtr[posdocs] * 1000),
                             round((corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[posdocs])^2 * 1000))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")

            for(i in posdocs) {
                id <- corpusCa$rownames[i]
                tkinsert(txt, "end", paste("\n\n", id, "\n", sep=""),
                         "articlehead")
                tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
                mark <- mark + 1
                tkinsert(listbox, "end", id)

                origin <- meta(corpus[[id]], "Origin")
                date <- meta(corpus[[id]], "DateTimeStamp")
                if(length(origin) > 0 && length(date) > 0)
                    tkinsert(txt, "end", paste(origin, " - ", date, "\n", sep=""))
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(origin, "\n", sep=""))
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(date, "\n", sep=""))

                tkinsert(txt, "end", paste(paste(corpus[[id]], collapse="\n"), "\n\n"))
            }
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(gettext_("Most contributive terms on positive side of axis %i:"),
                                       dim[j]), "\n", sep=""),
                 "heading")
        tkinsert(listbox, "end", sprintf(gettext_("Axis %i - Positive Side:"), dim[j]))
        tkitemconfigure(listbox, mark, background="grey")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        mark <- mark + 1

        posterms <- terms[corpusCa$colcoord[terms, dim[j]] >= 0]
        if(length(posterms) == 0) {
            tkinsert(txt, "end",
                     sprintf(gettext_("None among the %i most contributive terms."), nterms))
        }
        else {
            df <- data.frame(row.names=corpusCa$colnames[posterms],
                             round(corpusCa$colcoord[posterms, dim[j]] * corpusCa$sv[dim[j]], d=2),
                             round(termsCtr[posterms] * 1000),
                             round((corpusCa$colcoord[posterms, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$coldist[posterms])^2 * 1000))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }


        tkinsert(txt, "end",
                 paste("\n\n", sprintf(gettext_("Most contributive documents on positive side of axis %i:"),
                                       dim[j]), "\n", sep=""),
                 "heading")
        posdocs <- docs[corpusCa$rowcoord[docs, dim[j]] >= 0]
        if(length(posdocs) == 0) {
            tkinsert(txt, "end",
                     sprintf(gettext_("None among the %i most contributive documents."), ndocs))
        }
        else {
            df <- data.frame(row.names=corpusCa$rownames[posdocs],
                             round(corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]], d=2),
                             round(docsCtr[posdocs] * 1000),
                             round((corpusCa$rowcoord[posdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[posdocs])^2 * 1000))
            colnames(df) <- cols

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")

            for(i in posdocs) {
                id <- corpusCa$rownames[i]
                tkinsert(txt, "end", paste("\n\n", id, "\n", sep=""),
                         "articlehead")
                tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
                mark <- mark + 1
                tkinsert(listbox, "end", id)

                origin <- meta(corpus[[id]], "Origin")
                date <- meta(corpus[[id]], "DateTimeStamp")
                if(length(origin) > 0 && length(date) > 0)
                    tkinsert(txt, "end", paste(origin, " - ", date, "\n", sep=""))
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(origin, "\n", sep=""))
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(date, "\n", sep=""))

                tkinsert(txt, "end", paste(paste(corpus[[id]], collapse="\n"), "\n\n"))
            }
        }


        if(length(corpusCa$rowsup) > 0) {
            tkinsert(txt, "end",
                      paste("\n\n", sprintf(gettext_("Situation of variables on axis %i:"), dim[j]), "\n", sep=""),
                     "heading")
            tkinsert(listbox, "end", sprintf(gettext_("Axis %i - Variables"), dim[j]))
            tkitemconfigure(listbox, mark, background="grey")
            tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
            mark <- mark + 1

            supdocs <- corpusCa$rowsup
            df <- data.frame(row.names=corpusCa$rownames[supdocs],
                             round(corpusCa$rowcoord[supdocs, dim[j]] * corpusCa$sv[dim[j]], d=2),
                             round((corpusCa$rowcoord[supdocs, dim[j]] * corpusCa$sv[dim[j]] / corpusCa$rowdist[supdocs])^2 * 1000))
            colnames(df) <- cols[-2]

            tkinsert(txt, "end", paste(capture.output(format(df)), collapse="\n"), "fixed")
        }
    }
}

showCorpusCaDlg <- function() {
    if(!exists("corpusCa") || !class(corpusCa) == "ca") {
        Message(message=gettext_("Please run a correspondence analysis on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Show Correspondence Analysis"))

    dimFrame <- tkframe(top)
    tkgrid(labelRcmdr(dimFrame, text=gettext_("Dimensions to plot:"), fg="blue"), sticky="s")
    tclXDim <- tclVar(1)
    tclYDim <- tclVar(2)
    xSlider <- tkscale(dimFrame, from=1, to=corpusCa$nd,
                      showvalue=TRUE, variable=tclXDim,
		      resolution=1, orient="horizontal")
    ySlider <- tkscale(dimFrame, from=1, to=corpusCa$nd,
                      showvalue=TRUE, variable=tclYDim,
		      resolution=1, orient="horizontal")

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                              selectmode="multiple",
                              title=gettext_("Variables to plot:"),
                              initialSelection=(1:length(vars))-1)

    nFrame <- tkframe(top)
    tkgrid(labelRcmdr(nFrame, text=gettext_("Number of items to plot:"), fg="blue"), sticky="s")
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
    ctrDim1 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="xyDim", text=gettext_("Both axes"))
    ctrDim2 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="xDim", text=gettext_("Horizontal axis"))
    ctrDim3 <- ttkradiobutton(ctrDimFrame, variable=ctrDimVariable, value="yDim", text=gettext_("Vertical axis"))

    checkBoxes(frame="pointsFrame",
               boxes=c("documentsPoints", "termsPoints"),
               initialValues=c(0, 1),
               labels=c(gettext_("Documents"), gettext_("Terms")),
               title=gettext_("Draw point symbols for:"))

    onPlot <- function() {
        x <- tclvalue(tclXDim)
        y <- tclvalue(tclYDim)
        documents <- tclvalue(documentsVariable) == 1
        terms <- tclvalue(termsVariable) == 1
        variables <- if(length(corpusCa$rowsup) == 0) FALSE else tclvalue(variablesVariable) == 1
        vars <- getSelection(varBox)
        nDocs <- tclvalue(tclNDocs)
        nTerms <- tclvalue(tclNTerms)
        ctrDim <- switch(tclvalue(ctrDimVariable), xyDim=paste("c(", x, ", ", y, ")", sep=""), xDim=x, yDim=y)
        documentsPoints <- if(tclvalue(documentsPointsVariable) == 1) 2 else 1
        termsPoints <- if(tclvalue(termsPointsVariable) == 1) 2 else 1

        if(!(documents || terms || variables) || (variables && length(vars) == 0)) {
            errorCondition(recall=showCorpusCaDlg,
                           message=gettext_("Please select something to plot."))
            return()
        }

        doItAndPrint(sprintf("showCorpusCa(corpusCa, %s, %s, %s)", ctrDim, nDocs, nTerms))

        if(documents && variables) {
            rowWhat <- "all"
            varLevels <- unlist(lapply(vars, function(var) {
                levels <- levels(factor(meta(corpus, tag=var)[,1]))
                if(length(levels) == 1)
                    var
                else if(length(levels) > 0)
                    paste(var, levels)
            }))
            varIndexes <- which(corpusCa$rownames %in% varLevels)
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, c(order(rowCtr(corpusCa, ", ctrDim,
                               "), decreasing=TRUE)[1:", nDocs, "], ", paste(varIndexes, collapse=", "), "))", sep=""))
        }
        else if(documents) {
            rowWhat <- "active"
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, order(rowCtr(corpusCa, ", ctrDim,
                               "), decreasing=TRUE)[1:", nDocs, "])", sep=""))
        }
        else if(variables) {
            rowWhat <- "passive"
            varLevels <- unlist(lapply(vars, function(var) {
                levels <- levels(factor(meta(corpus, tag=var)[,1]))
                if(length(levels) == 1)
                    var
                else if(length(levels) > 0)
                    paste(var, levels)
            }))
            varIndexes <- which(corpusCa$rownames %in% varLevels)
            doItAndPrint(paste("plottingCa <- rowSubsetCa(corpusCa, c(",
                               paste(varIndexes, collapse=", "), "))", sep=""))
        }
        else {
            rowWhat <- "none"
        }

        if(terms) {
            colWhat <- "all"
            if(documents || variables)
                doItAndPrint(paste("plottingCa <- colSubsetCa(plottingCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
            else
                doItAndPrint(paste("plottingCa <- colSubsetCa(corpusCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
        }
        else {
            colWhat <- "none"
        }

        doItAndPrint(paste("plotCorpusCa(plottingCa, dim=c(", x, ", ", y, "), what=c(\"", rowWhat, "\", \"",
                           colWhat, "\"), labels=c(", documentsPoints, ", ", termsPoints,
                           "), mass=TRUE)", sep=""))

        activateMenus()
    }

    if(length(corpusCa$rowsup) == 0) {
        checkBoxes(frame="whatFrame",
                   boxes=c("documents", "terms"),
                   initialValues=c(0, 1),
                   labels=c(gettext_("Documents"), gettext_("Terms")),
                   title=gettext_("Items to represent:"))
    }
    else {
        checkBoxes(frame="whatFrame",
                   boxes=c("variables", "documents", "terms"),
                   initialValues=c(0, 0, 1),
                   labels=c(gettext_("Variables"), gettext_("Documents"), gettext_("Terms")),
                   title=gettext_("Items to represent:"))
    }

    # Custom buttons, adapted from OKCancelHelp()
    buttonsFrame <- tkframe(top, borderwidth=5)
    plotButton <- buttonRcmdr(buttonsFrame, text=gettext_("Display"), foreground="darkgreen",
                              command=onPlot, default="active", borderwidth=3)
    onClose <- function() {
        closeDialog()
        tkfocus(CommanderWindow())
    }
    closeButton <- buttonRcmdr(buttonsFrame, text=gettext_("Close"), foreground="red",
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

    tkgrid(labelRcmdr(dimFrame, text=gettext_("Horizontal axis:")), xSlider, sticky="w")
    tkgrid(labelRcmdr(dimFrame, text=gettext_("Vertical axis:")), ySlider, sticky="w")
    tkgrid(dimFrame, sticky="w", pady=6)
    tkgrid(whatFrame, sticky="w", pady=6)
    if(length(corpusCa$rowsup) > 0)
        tkgrid(getFrame(varBox), columnspan=3, sticky="we", pady=6)
    tkgrid(labelRcmdr(nFrame, text=gettext_("Documents:")), docsSlider, sticky="w")
    tkgrid(labelRcmdr(nFrame, text=gettext_("Terms:")), termsSlider, sticky="w")
    tkgrid(nFrame, sticky="w", pady=6)
    tkgrid(labelRcmdr(ctrDimFrame, text=gettext_("Most contributive to:")), sticky="w", columnspan=3, pady=6)
    tkgrid(ctrDim1, ctrDim2, ctrDim3, sticky="w", pady=6)
    tkgrid(ctrDimFrame, sticky="w", pady=6)
    tkgrid.columnconfigure(ctrDimFrame, "all", uniform="a")
    tkgrid(pointsFrame, sticky="w", pady=6)
    tkgrid(buttonsFrame, sticky="w", pady=6)
    nrows <- if(length(corpusCa$rowsup) == 0) 6 else 7
    dialogSuffix(rows=nrows, columns=1, onOK=onPlot, onCancel=onClose,
                 # The grab prevents the user from scrolling the CA text window
                 preventGrabFocus=TRUE)
}

