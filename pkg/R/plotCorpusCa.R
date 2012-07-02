## Helper functions to plot a CA restricted to most contributive points

# Get rows contributions to the dimensions of a CA
rowCtr <- function(obj, dim) {
    I <- dim(obj$rowcoord)[1]
    K <- obj$nd
    svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow=TRUE)
    rpc <- obj$rowcoord[,1:K] * svF
    obj$rowmass * rpc[,dim]^2 / obj$sv[dim]^2
}

# Get columns contributions to the dimensions of a CA
colCtr <- function(obj, dim) {
    J <- dim(obj$colcoord)[1]
    K <- obj$nd
    svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow=TRUE)
    cpc <- obj$colcoord[,1:K] * svG
    obj$colmass * cpc[,dim]^2 / obj$sv[dim]^2
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
    ret$rowcoord <- ca$rowcoord[indices,]
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
    ret$colcoord <- ca$colcoord[indices,]
    ret$colnames <- ca$colnames[indices]
    ret
}


plotCorpusCaDlg <- function() {
    if(!exists("corpusCa") || !class(corpusCa) == "ca") {
        Message(message=gettext_("Please run a correspondence analysis on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Plot Correspondence Analysis"))

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
                              title=gettext_("Meta-data variables to plot:"),
                              initialSelection=(1:length(vars))-1)

    nFrame <- tkframe(top)
    tkgrid(labelRcmdr(nFrame, text=gettext_("Number of items to plot:"), fg="blue"), sticky="s")
    tclNDocs <- tclVar(25)
    tclNTerms <- tclVar(25)
    docsSlider <- tkscale(nFrame, from=1, to=nrow(corpusCa$rowcoord)-length(corpusCa$rowsup),
                          showvalue=TRUE, variable=tclNDocs,
		          resolution=1, orient="horizontal")
    termsSlider <- tkscale(nFrame, from=1, to=nrow(corpusCa$colcoord)-length(corpusCa$colsup),
                           showvalue=TRUE, variable=tclNTerms,
		           resolution=1, orient="horizontal")
    radioButtons(name="ctrDim",
                 buttons=c("xDim", "yDim"),
                 labels=c(gettext_("Horizontal axis"), gettext_("Vertical axis")))

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
        metadata <- if(length(corpusCa$rowsup) == 0) FALSE else tclvalue(metadataVariable) == 1
        vars <- getSelection(varBox)
        nDocs <- tclvalue(tclNDocs)
        nTerms <- tclvalue(tclNTerms)
        ctrDim <- if(tclvalue(ctrDimVariable) == "xDim") x else y
        documentsPoints <- if(tclvalue(documentsPointsVariable) == 1) 2 else 1
        termsPoints <- if(tclvalue(termsPointsVariable) == 1) 2 else 1

        if(!(documents || terms || metadata) || (metadata && length(vars) == 0)) {
            errorCondition(recall=plotCorpusCaDlg,
                           message=gettext_("Please select something to plot."))
            return()
        }

        if(documents && metadata) {
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
        else if(metadata) {
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
            if(documents || metadata)
                doItAndPrint(paste("plottingCa <- colSubsetCa(plottingCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
            else
                doItAndPrint(paste("plottingCa <- colSubsetCa(corpusCa, order(colCtr(corpusCa, ", ctrDim,
                                   "), decreasing=TRUE)[1:", nTerms, "])", sep=""))
        }
        else {
            colWhat <- "none"
        }

        doItAndPrint(paste("plot(plottingCa, dim=c(", x, ", ", y, "), what=c(\"", rowWhat, "\", \"",
                           colWhat, "\"), labels=c(", documentsPoints, ", ", termsPoints,
                           "), contrib=\"relative\", mass=TRUE)", sep=""))
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
                   boxes=c("metadata", "documents", "terms"),
                   initialValues=c(0, 0, 1),
                   labels=c(gettext_("Documents meta-data"), gettext_("Documents"), gettext_("Terms")),
                   title=gettext_("Items to represent:"))
    }

    # Custom buttons, adapted from OKCancelHelp()
    buttonsFrame <- tkframe(top, borderwidth=5)
    plotButton <- buttonRcmdr(buttonsFrame, text=gettext_("Plot"), foreground="darkgreen",
                              command=onPlot, default="active", borderwidth=3)
    onClose <- function() {
        closeDialog()
        tkfocus(CommanderWindow())
    }
    closeButton <- buttonRcmdr(buttonsFrame, text=gettext_("Close"), foreground="red",
                               command=onClose, borderwidth=3)
    onHelp <- function() {
        if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
        print(help("plotCorpusCaDlg"))
    }
    helpButton <- buttonRcmdr(buttonsFrame, text=gettextRcmdr("Help"), width="12",
                              command=onHelp, borderwidth=3)
    tkgrid(plotButton, labelRcmdr(buttonsFrame, text="  "),
           closeButton, labelRcmdr(buttonsFrame, text="            "),
           helpButton, sticky="w")

    tkgrid(labelRcmdr(dimFrame, text=gettext_("Horizontal axis:")), xSlider, sticky="sw")
    tkgrid(labelRcmdr(dimFrame, text=gettext_("Vertical axis:")), ySlider, sticky="sw")
    tkgrid(dimFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(whatFrame, columnspan="2", sticky="w", pady=6)
    if(length(corpusCa$rowsup) > 0)
        tkgrid(getFrame(varBox), sticky="w", pady=6)
    tkgrid(labelRcmdr(nFrame, text=gettext_("Documents:")), docsSlider, sticky="sw")
    tkgrid(labelRcmdr(nFrame, text=gettext_("Terms:")), termsSlider, sticky="sw")
    tkgrid(nFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(labelRcmdr(nFrame, text=gettext_("Most contributive to:")),
           ctrDimFrame, sticky="w", pady=6)
    tkgrid(pointsFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    nrows <- if(length(corpusCa$rowsup) == 0) 5 else 6
    dialogSuffix(rows=nrows, columns=2, onOK=onPlot, onCancel=onClose)
}

