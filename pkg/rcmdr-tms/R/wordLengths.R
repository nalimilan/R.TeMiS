wordLengthsTable <- function(lengthsDtm, variable=NULL, unit=c("document", "global")) {
    unit <- match.arg(unit)
    var <- meta(corpus, tag=variable)

    totalPerDoc <- row_sums(lengthsDtm)
    longPerDoc <- row_sums(lengthsDtm[,nchar(colnames(lengthsDtm)) >= 7])
    veryLongPerDoc <- row_sums(lengthsDtm[,nchar(colnames(lengthsDtm)) >= 10])
    weightedLengths <- rowSums(sweep(as.matrix(lengthsDtm), 2, nchar(colnames(lengthsDtm)), "*"))

    # Per-document statistics
    if(is.null(variable)) {
        lengths <- rbind(totalPerDoc,
                     longPerDoc, longPerDoc/totalPerDoc*100,
                     veryLongPerDoc, veryLongPerDoc/totalPerDoc*100,
                     weightedLengths/totalPerDoc)

        lengths <- cbind(lengths, c(mean(lengths[1,]),
                            mean(lengths[2,]), mean(lengths[3,], na.rm=TRUE),
                            mean(lengths[4,]), mean(lengths[5,], na.rm=TRUE),
                            mean(lengths[6,], na.rm=TRUE)),
                           c(sum(lengths[1,]),
                             sum(lengths[2,]), sum(lengths[2,])/sum(lengths[1,])*100,
                             sum(lengths[4,]), sum(lengths[4,])/sum(lengths[1,])*100,
                             sum(weightedLengths)/sum(lengthsDtm)))
        colnames(lengths)[c(ncol(lengths)-1, ncol(lengths))] <- c(gettext_("Corpus mean"), gettext_("Corpus total"))
        lab <- ""
    }
    # Per-category statistics
    else if(unit == "document") {
        total <- tapply(totalPerDoc, var, mean)
        long <- tapply(longPerDoc, var, mean)
        veryLong <- tapply(veryLongPerDoc, var, mean)
        avgLengthPerDoc <- weightedLengths/totalPerDoc
        avgLength <- tapply(avgLengthPerDoc, var, mean, na.rm=TRUE)
        lengths <- rbind(total, long, long/total*100, veryLong, veryLong/total*100, avgLength)
        lengths <- cbind(lengths, c(mean(total),
                            mean(longPerDoc), mean(longPerDoc/totalPerDoc, na.rm=TRUE)*100,
                            mean(veryLongPerDoc), mean(veryLongPerDoc/totalPerDoc, na.rm=TRUE)*100,
                            mean(avgLengthPerDoc, na.rm=TRUE)))
        colnames(lengths)[ncol(lengths)] <- gettext_("Corpus")
        lab <- gettext_("Per document mean:")
    }
    else {
        total <- tapply(totalPerDoc, var, sum)
        long <- tapply(longPerDoc, var, sum)
        veryLong <- tapply(veryLongPerDoc, var, sum)
        avgLength <- tapply(weightedLengths, var, sum, na.rm=TRUE)/total
        lengths <- rbind(total, long, long/total*100, veryLong, veryLong/total*100, avgLength)
        lengths <- cbind(lengths, c(sum(total), sum(long), sum(long)/sum(total)*100, sum(veryLong),
                            sum(veryLong)/sum(total)*100, sum(weightedLengths)/sum(total)))
        colnames(lengths)[ncol(lengths)] <- gettext_("Corpus")
        lab <- gettext_("Per category total:")
    }


    lengths <- as.table(round(lengths, d=1))
    rownames(lengths) <- c(gettext_("Number of words"),
                       gettext_("Number of long words"),
                       gettext_("Percent of long words"),
                       gettext_("Number of very long words"),
                       gettext_("Percent of very long words"),
                       gettext_("Average word length"))
    names(dimnames(lengths)) <- c(lab, "")

    lengths
}

docWordLengthsDlg <- function() {
    initializeDialog(title=gettext_("Word Lengths per Document"))

    checkBoxes(frame="whatFrame",
               title=gettext_("Draw plot for:"),
               boxes=c("total", "longc", "longp", "vlongc", "vlongp", "longavg"),
               initialValues=c(0, 0, 1, 0, 0, 0, 0),
               labels=c(gettext_("Number of words"),
                        gettext_("Number of long words"),
                        gettext_("Percent of long words"),
                        gettext_("Number of very long words"),
                        gettext_("Percent of very long words"),
                        gettext_("Average word length")))

    radioButtons(name="corpusMeasure",
                 title=gettext_("Plot global measure:"),
                 buttons=c("none", "mean", "total"),
                 initialValue="mean",
                 labels=c(gettext_("None"),
                          gettext_("Corpus mean"),
                          gettext_("Corpus total")),
                 right=FALSE)

    titleFrame <- tkframe(top)
    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        title <- tclvalue(tclTitle)
        total <- tclvalue(totalVariable) == 1
        longc <- tclvalue(longcVariable) == 1
        longp <- tclvalue(longpVariable) == 1
        vlongc <- tclvalue(vlongcVariable) == 1
        vlongp <- tclvalue(vlongpVariable) == 1
        longavg <- tclvalue(longavgVariable) == 1
        corpusMeasure <- tclvalue(corpusMeasureVariable)

        closeDialog()

        # Only compute the dtm the first time this operation is run
        if(!exists("lengthsDtm")) {
            doItAndPrint("dtmCorpus <- corpus")

            if(meta(corpus, type="corpus", tag="language") == "french")
                doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"[\'\U2019-]\", \" \", x))")

            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removePunctuation)")
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removeNumbers)")
            doItAndPrint("lengthsDtm <- DocumentTermMatrix(dtmCorpus, control=list(wordLengths=c(2, Inf)))")
            doItAndPrint("rm(dtmCorpus)")
        }

        doItAndPrint("lengths <- wordLengthsTable(lengthsDtm)")

        # Plot
        if(any(total, longc, longp, vlongc, vlongp, longavg)) {
            indexes <- paste(which(c(total, longc, longp, vlongc, vlongp, longavg)), collapse=", ")

            if(corpusMeasure == "mean")
                exclude <- sprintf(" -%s", ncol(lengths))
            else if(corpusMeasure == "total")
                exclude <- sprintf(" -%s", ncol(lengths)-1)
            else
                exclude <- sprintf(" -c(%s, %s)", ncol(lengths)-1, ncol(lengths))

            if(sum(total, longc, longp, vlongc, vlongp, longavg) > 1)
                doItAndPrint(sprintf('barchart(t(lengths[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", auto.key=list(space="bottom"), ylim=c(0, max(lengths[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))
            else
                doItAndPrint(sprintf('barchart(t(lengths[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", ylim=c(0, max(lengths[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))
        }

        doItAndPrint("print(lengths)")

        activateMenus()
        tkfocus(CommanderWindow())
    }


    OKCancelHelp(helpSubject="docWordLengthsDlg")
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=2)
    tkgrid(corpusMeasureFrame, sticky="w", pady=6, padx=6, columnspan=2)
    tkgrid(labelRcmdr(titleFrame, text=gettext_("Plot title:")), titleEntry, sticky="w", padx=6)
    tkgrid(titleFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", columnspan=2, pady=6)
    dialogSuffix(rows=4, columns=2)
}

varWordLengthsDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Word Lengths per Variable"))

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

    checkBoxes(frame="whatFrame",
               title=gettext_("Draw plot for:"),
               boxes=c("total", "longc", "longp", "vlongc", "vlongp", "longavg", "corpus"),
               initialValues=c(0, 0, 1, 0, 0, 0, 1),
               labels=c(gettext_("Number of words"),
                        gettext_("Number of long words"),
                        gettext_("Percent of long words"),
                        gettext_("Number of very long words"),
                        gettext_("Percent of very long words"),
                        gettext_("Average word length"),
                        gettext_("Corpus global measure")))

    titleFrame <- tkframe(top)
    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        unit <- tclvalue(unitVariable)
        total <- tclvalue(totalVariable) == 1
        longc <- tclvalue(longcVariable) == 1
        longp <- tclvalue(longpVariable) == 1
        vlongc <- tclvalue(vlongcVariable) == 1
        vlongp <- tclvalue(vlongpVariable) == 1
        longavg <- tclvalue(longavgVariable) == 1
        corpusMeasure <- tclvalue(corpusVariable) == 1

        closeDialog()

        # Only compute the dtm the first time this operation is run
        if(!exists("lengthsDtm")) {
            doItAndPrint("dtmCorpus <- corpus")

            if(meta(corpus, type="corpus", tag="language") == "french")
                doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"[\'\U2019-]\", \" \", x))")

            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removePunctuation)")
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removeNumbers)")
            doItAndPrint("lengthsDtm <- DocumentTermMatrix(dtmCorpus, control=list(wordLengths=c(2, Inf)))")
            doItAndPrint("rm(dtmCorpus)")
        }

        doItAndPrint(sprintf('lengths <- wordLengthsTable(lengthsDtm, "%s", "%s")', var, unit))

        # Plot
        if(any(total, longc, longp, vlongc, vlongp, longavg)) {
            indexes <- paste(which(c(total, longc, longp, vlongc, vlongp, longavg)), collapse=", ")

            if(corpusMeasure)
                exclude <- ""
            else
                exclude <- sprintf(" -%s", ncol(lengths))

            if(sum(total, longc, longp, vlongc, vlongp, longavg) > 1)
                doItAndPrint(sprintf('barchart(t(lengths[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", auto.key=list(space="bottom"), ylim=c(0, max(lengths[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))
            else
                doItAndPrint(sprintf('barchart(t(lengths[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", ylim=c(0, max(lengths[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))

        }

        doItAndPrint("print(lengths)")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varWordLengthsDlg")
    tkgrid(getFrame(varBox), sticky="w", columnspan=2, pady=6)
    tkgrid(unitFrame, sticky="w", columnspan=2)
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=2)
    tkgrid(labelRcmdr(titleFrame, text=gettext_("Plot title:")), titleEntry, sticky="w", padx=6)
    tkgrid(titleFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=5, columns=2)
}

copyWordLengths <- function() {
    R2HTML::HTML2clip(lengths)
}

