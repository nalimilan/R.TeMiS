varTableDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=gettext("Corpus has no meta-data. Use Text Mining->Set corpus meta-data to add variables."),
                type="error")
        return()
    }

    initializeDialog(title=gettext("Variable Cross Table"))

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                               title=gettext("Variable:"),
                               initialSelection=0)

    radioButtons(name="what",
                 buttons=c("percent", "absolute"),
                 labels=gettext(c("Percent",
                                  "Absolute counts")),
                 title="Measure:",
                 right=FALSE)

    tclPlotType <- tclVar("none")
    plotFrame <- tkframe(top)
    noneButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                 value="none", text=gettext("None"))
    barplotButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                    value="barplot", text=gettext("Bar plot"))
    pieButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                value="pie", text=gettext("Pie chart"))

    tclTitle <- tclVar(gettext("Distribution of documents"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        var <- getSelection(varBox)
        what <- tclvalue(whatVariable)
        title <- tclvalue(tclTitle)
        plotType <- tclvalue(tclPlotType)

        closeDialog()

        doItAndPrint(paste("absVarFreq <- table(meta(corpus, tag=\"", var, "\"))", sep=""))

        if(what == "percent") {
            doItAndPrint("varFreq <- prop.table(absVarFreq)")
            doItAndPrint("varFreq <- round(varFreq*100, d=1)")
            xlab <- gettext("% of documents")
        }
        else {
            doItAndPrint("varFreq <- absVarFreq")
            xlab <- gettext("Number of documents")
        }

        if(plotType == "barplot")
            doItAndPrint(paste("barplot(varFreq, ylab=\"", xlab,
                               "\", beside=TRUE)", sep=""))
        else if(plotType == "pie")
            doItAndPrint("pie(varFreq)")

        if(plotType != "none" && title != "")
            doItAndPrint(paste("title(main=\"", title, "\")", sep=""))

        doItAndPrint("print(varFreq)")

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTableDlg")
    tkgrid(getFrame(varBox), sticky="w", pady=6, columnspan=3)
    tkgrid(whatFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Plot:"), foreground="blue"),
           sticky="w", columnspan=3)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(noneButton, sticky="w", padx=3, column=1, row=4)
    tkgrid(barplotButton, sticky="w", padx=3, column=2, row=4)
    tkgrid(pieButton, sticky="w", padx=3, column=3, row=4)
    tkgrid(labelRcmdr(top, text=gettext("Title:")), sticky="w", column=1, row=4)
    tkgrid(titleEntry, sticky="w", column=2, row=4, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=3)
    dialogSuffix(rows=5, columns=3, focus=varBox)
}

varCrossTableDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=gettext("Corpus has no meta-data. Use Text Mining->Set corpus meta-data to add variables."),
                type="error")
        return()
    }
    else if(nVars == 1) {
        Message(message=gettext("Corpus has only one meta-data variable."),
                type="error")
        return()
    }

    initializeDialog(title=gettext("Variable Cross Table"))

    vars <- colnames(meta(corpus))
    varBox1 <- variableListBox(top, vars,
                               title=gettext("First variable:"),
                               initialSelection=0)

    varBox2 <- variableListBox(top, vars,
                               title=gettext("Second variable:"),
                               initialSelection=1)

    radioButtons(name="what",
                 buttons=c("row", "col", "absolute"),
                 labels=gettext(c("Row %",
                                  "Column %",
                                  "Absolute counts")),
                 title="Measure:",
                 right=FALSE)

    tclPlotType <- tclVar("none")
    plotFrame <- tkframe(top)
    noneButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                 value="none", text=gettext("None"))
    barplotButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                    value="barplot", text=gettext("Bar plot"))
    pieButton <- ttkradiobutton(plotFrame, variable=tclPlotType,
                                value="pie", text=gettext("Pie chart"))

    tclTitle <- tclVar(gettext("Distribution of documents"))
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        var1 <- getSelection(varBox1)
        var2 <- getSelection(varBox2)
        what <- tclvalue(whatVariable)
        title <- tclvalue(tclTitle)
        plotType <- tclvalue(tclPlotType)

        closeDialog()

        doItAndPrint(paste("absVarFreq <- table(cbind(meta(corpus, tag=\"", var1,
                           "\"), meta(corpus, tag=\"", var2, "\")))", sep=""))

        if(what == "row") {
            doItAndPrint("varFreq <- prop.table(absVarFreq, 1)")
            doItAndPrint("varFreq <- round(varFreq*100, d=1)")
            ylab <- gettext("% of documents")
        }
        else if (what == "col") {
            doItAndPrint("varFreq <- prop.table(absVarFreq, 2)")
            doItAndPrint("varFreq <- round(varFreq*100, d=1)")
            ylab <- gettext("% of documents")
        }
        else {
            doItAndPrint("varFreq <- absVarFreq")
            ylab <- gettext("Number of documents")
        }

        if(plotType == "barplot") {
            if(what == "row") {
              doItAndPrint(paste("barplot(t(varFreq), ylab=\"",  ylab,
                                   "\", beside=TRUE, legend.text=colnames(varFreq))", sep=""))
                if(title != "")
                    doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
             }
             else {
                doItAndPrint(paste("barplot(varFreq, ylab=\"",  ylab,
                                   "\", beside=TRUE, legend.text=rownames(varFreq))", sep=""))
                if(title != "")
                    doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
             }
        }
        else if(plotType == "pie") {
            if(what == "col") {
                doItAndPrint(paste("par(mfrow=c(2, ", ceiling(ncol(varFreq)/2), "))", sep=""))
                for(i in 1:ncol(varFreq)) {
                    doItAndPrint(paste("pie(varFreq[,", i, "])", sep=""))
                    if(title != "")
                        doItAndPrint(paste("title(main=\"", names(dimnames(varFreq))[2], " ",
                                           colnames(varFreq)[i], "\")", sep=""))
                }
            }
            else {
                doItAndPrint(paste("par(mfrow=c(2, ", ceiling(nrow(varFreq)/2), "))", sep=""))
                for(i in 1:nrow(varFreq)) {
                    doItAndPrint(paste("pie(varFreq[", i, ",])", sep=""))
                    doItAndPrint(paste("title(main=\"", names(dimnames(varFreq))[1], " ",
                                       rownames(varFreq)[i], "\")", sep=""))
                }
            }
        }

        doItAndPrint("print(varFreq)")

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varCrossTableDlg")
    tkgrid(getFrame(varBox1), sticky="w", pady=6, columnspan=3)
    tkgrid(getFrame(varBox2), sticky="w", pady=6, columnspan=3)
    tkgrid(whatFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(labelRcmdr(plotFrame, text=gettext("Plot:"), foreground="blue"),
           sticky="w", columnspan=3)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(noneButton, sticky="w", padx=3, column=1, row=4)
    tkgrid(barplotButton, sticky="w", padx=3, column=2, row=4)
    tkgrid(pieButton, sticky="w", padx=3, column=3, row=4)
    tkgrid(labelRcmdr(top, text=gettext("Title:")), titleEntry, sticky="w")
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=3)
    dialogSuffix(rows=6, columns=3, focus=varBox1)
}

copyVarFreq <- function() {
  HTML2clip(varFreq)
}

