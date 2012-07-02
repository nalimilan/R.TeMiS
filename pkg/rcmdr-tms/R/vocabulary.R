docVocabularyDlg <- function() {
    initializeDialog(title=gettext_("Vocabulary per Document"))

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    checkBoxes(frame="whatFrame",
               boxes=c("total", "unique", "diversity"),
               initialValues=c(0, 0, 1),
               labels=c(gettext_("Number of terms"),
                        gettext_("Number of unique terms"),
                        gettext_("Terms diversity")))

    onOK <- function() {
        title <- tclvalue(tclTitle)
        plot <- tclvalue(tclPlotVar)
        total <- tclvalue(totalVariable) == 1
        unique <- tclvalue(uniqueVariable) == 1
        diversity <- tclvalue(diversityVariable) == 1

        closeDialog()

        doItAndPrint("voc <- rbind(row_sums(dtm), rowSums(as.matrix(dtm) > 0), rowSums(as.matrix(dtm) > 0)/row_sums(dtm)*100)")
        doItAndPrint(paste("voc <- cbind(voc, \"", gettext_("Corpus mean"),
                           "\"=c(mean(voc[1,]), mean(voc[2,]), mean(voc[3,], na.rm=TRUE)))", sep=""))
        doItAndPrint(paste("voc <- cbind(voc, \"", gettext_("Corpus total"),
                           "\"=c(sum(voc[1,-ncol(voc)]), sum(voc[2,-ncol(voc)]), sum(voc[2,-ncol(voc)])/sum(voc[1,-ncol(voc)])*100))", sep=""))

        doItAndPrint("voc <- as.table(round(voc, d=1))")
        doItAndPrint(paste("rownames(voc) <- c(\"", gettext_("Number of terms"), "\", \"",
                                                    gettext_("Number of unique terms"), "\", \"",
                                                    gettext_("Terms diversity"), "\")", sep=""))

        # Plot
        if(plot == 1) {
            indexes <- paste(which(c(total, unique, diversity)), collapse=", ")

            if(total || unique) # Don't plot the total, since it will always be too high
                exclude <- " -ncol(voc)"
            else
                exclude <- " "

            if(sum(total, unique, diversity) > 1)
                doItAndPrint(paste("barplot(voc[c(", indexes, "),", exclude, "], beside=TRUE, legend.text=rownames(voc)[c(", indexes, ")])", sep=""))
            else
                doItAndPrint(paste("barplot(voc[c(", indexes, "),", exclude, "], beside=TRUE)", sep=""))

            if(title != "")
                doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
        }

        doItAndPrint("print(voc)")

        activateMenus()
        tkfocus(CommanderWindow())
    }


    OKCancelHelp(helpSubject="docVocabularyDlg")
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=3, columns=2)
}

varVocabularyDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Vocabulary per Variable"))

    vars <- colnames(meta(corpus))
    varBox <- variableListBox(top, vars,
                              title=gettext_("Variable:"),
                              initialSelection=0)

    radioButtons(name="unit",
                 buttons=c("doc", "global"),
                 labels=c(gettext_("Document (mean)"),
                          gettext_("Category (sum)")),
                 title=gettext_("Unit:"),
                 right=FALSE)

    tclPlotVar <- tclVar(0)
    plotFrame <- tkframe(top)
    plotButton <- tkcheckbutton(plotFrame, text=gettext_("Draw plot"), variable=tclPlotVar)

    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    checkBoxes(frame="whatFrame",
               boxes=c("total", "unique", "diversity"),
               initialValues=c(0, 0, 1),
               labels=c(gettext_("Number of terms"),
                        gettext_("Number of unique terms"),
                        gettext_("Terms diversity")))

    onOK <- function() {
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        unit <- tclvalue(unitVariable)
        plot <- tclvalue(tclPlotVar)
        total <- tclvalue(totalVariable) == 1
        unique <- tclvalue(uniqueVariable) == 1
        diversity <- tclvalue(diversityVariable) == 1

        closeDialog()

        doItAndPrint("unique <- rowSums(as.matrix(dtm) > 0)")
        doItAndPrint("total <- row_sums(dtm)")

        if(unit == "doc") {
            doItAndPrint(paste("total <- tapply(row_sums(dtm), meta(corpus, tag=\"", var, "\"), mean)", sep=""))
            doItAndPrint(paste("unique <- tapply(rowSums(as.matrix(dtm) > 0), meta(corpus, tag=\"",
                               var, "\"), mean)", sep=""))
            doItAndPrint(paste("div <- tapply(rowSums(as.matrix(dtm) > 0)/row_sums(dtm), meta(corpus, tag=\"",
                               var, "\"), mean, na.rm=TRUE)", sep=""))
            doItAndPrint("voc <- rbind(total, unique, div*100)")
            doItAndPrint(paste("voc <- cbind(voc, \"", gettext_("Corpus"),
                               "\"=c(mean(row_sums(dtm)), mean(rowSums(as.matrix(dtm) > 0)), mean(rowSums(as.matrix(dtm) > 0)/row_sums(dtm), na.rm=TRUE)*100))", sep=""))
            lab <- gettext_("Per document mean:")
        }
        else {
            doItAndPrint(paste("total <- tapply(row_sums(dtm), meta(corpus, tag=\"", var, "\"), sum)", sep=""))
            doItAndPrint(paste("unique <- tapply(rowSums(as.matrix(dtm) > 0), meta(corpus, tag=\"", var, "\"), sum)", sep=""))
            doItAndPrint("div <- unique/total")
            doItAndPrint("voc <- rbind(total, unique, div*100)")
            doItAndPrint(paste("voc <- cbind(voc, \"", gettext_("Corpus"),
                               "\"=c(sum(total), sum(unique), sum(unique)/sum(total)*100))", sep=""))
            lab <- gettext_("Per category total:")
        }

        doItAndPrint("voc <- as.table(round(voc, d=1))")
        doItAndPrint(paste("dimnames(voc) <- list(\"", lab, "\"=c(\"",
                           gettext_("Number of terms"), "\", \"",
                           gettext_("Number of unique terms"), "\", \"",
                           gettext_("Terms diversity"), "\"), \"", var, "\"=colnames(voc))", sep=""))

        # Plot
        if(plot == 1) {
            indexes <- paste(which(c(total, unique, diversity)), collapse=", ")

            if(unit == "doc") {
                if(sum(total, unique, diversity) > 1)
                    doItAndPrint(paste("barplot(voc[c(", indexes, "),], beside=TRUE, legend.text=rownames(voc)[c(", indexes, ")])", sep=""))
                else
                    doItAndPrint(paste("barplot(voc[c(", indexes, "),], beside=TRUE)", sep=""))
            }
            else {
                if(total || unique) # Don't plot the total, since it will always be too high
                    exclude <- " -ncol(voc)"
                else
                    exclude <- " "

                if(sum(total, unique, diversity) > 1)
                    doItAndPrint(paste("barplot(voc[c(", indexes, "),", exclude, "], beside=TRUE, legend.text=rownames(voc)[c(", 
                                       indexes, ")])", sep=""))
                else
                    doItAndPrint(paste("barplot(voc[c(", indexes, "),", exclude, "], beside=TRUE)", sep=""))
            }

            if(title != "")
                doItAndPrint(paste("title(main=\"", title, "\")", sep=""))
        }

        doItAndPrint("print(voc)")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTermFreqDlg")
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(unitFrame, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Plot:"), foreground="blue"), sticky="w", columnspan=2)
    tkgrid(plotButton, sticky="w", columnspan=2)
    tkgrid(labelRcmdr(plotFrame, text=gettext_("Title:")), titleEntry, sticky="w", padx=6)
    tkgrid(plotFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=3, columns=2)
}

copyVocabulary <- function() {
    R2HTML::HTML2clip(voc)
}

