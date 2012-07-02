vocabularyTable <- function(termsDtm, wordsDtm, variable=NULL, unit=c("document", "global")) {
    unit <- match.arg(unit)
    var <- meta(corpus, tag=variable)

    termsMat <- as.matrix(termsDtm)
    totaltPerDoc <- row_sums(termsDtm)
    uniquePerDoc <- rowSums(termsMat > 0)
    hapaxPerDoc <- rowSums(termsMat == 1)
    totalwPerDoc <- row_sums(wordsDtm)
    longPerDoc <- row_sums(wordsDtm[,nchar(colnames(wordsDtm)) >= 7])
    veryLongPerDoc <- row_sums(wordsDtm[,nchar(colnames(wordsDtm)) >= 10])
    weightedLengths <- rowSums(sweep(as.matrix(wordsDtm), 2, nchar(colnames(wordsDtm)), "*"))
    rm(termsMat)

    # Per-document statistics
    if(is.null(variable)) {
        voc <- rbind(totaltPerDoc,
                     uniquePerDoc, uniquePerDoc/totaltPerDoc*100,
                     hapaxPerDoc, hapaxPerDoc/totaltPerDoc*100,
                     totalwPerDoc,
                     longPerDoc, longPerDoc/totalwPerDoc*100,
                     veryLongPerDoc, veryLongPerDoc/totalwPerDoc*100,
                     weightedLengths/totalwPerDoc)

        voc <- cbind(voc, c(mean(totaltPerDoc),
                            mean(uniquePerDoc), mean(uniquePerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(hapaxPerDoc), mean(hapaxPerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(totalwPerDoc),
                            mean(longPerDoc), mean(longPerDoc/totalwPerDoc, na.rm=TRUE)*100,
                            mean(veryLongPerDoc), mean(veryLongPerDoc/totalwPerDoc, na.rm=TRUE)*100,
                            mean(weightedLengths/totalwPerDoc, na.rm=TRUE)),
                          c(sum(totaltPerDoc),
                            sum(uniquePerDoc), sum(uniquePerDoc)/sum(totaltPerDoc)*100,
                            sum(hapaxPerDoc), sum(hapaxPerDoc)/sum(totaltPerDoc)*100,
                            sum(totalwPerDoc),
                            sum(longPerDoc), sum(longPerDoc)/sum(totalwPerDoc)*100,
                            sum(veryLongPerDoc), sum(veryLongPerDoc)/sum(totalwPerDoc)*100,
                            sum(weightedLengths)/sum(wordsDtm)))

        colnames(voc)[c(ncol(voc)-1, ncol(voc))] <- c(gettext_("Corpus mean"), gettext_("Corpus total"))
        lab <- ""
    }
    # Per-category statistics
    else if(unit == "document") {
        totalt <- tapply(totaltPerDoc, var, mean)
        unique <- tapply(uniquePerDoc, var, mean)
        hapax <- tapply(hapaxPerDoc, var, mean)
        totalw <- tapply(totalwPerDoc, var, mean)
        long <- tapply(longPerDoc, var, mean)
        veryLong <- tapply(veryLongPerDoc, var, mean)
        avgLengthPerDoc <- weightedLengths/totalwPerDoc
        avgLength <- tapply(avgLengthPerDoc, var, mean, na.rm=TRUE)
        voc <- rbind(totalt, unique, unique/totalt*100,
                             hapax, hapax/totalt*100,
                     totalw, long, long/totalw*100,
                             veryLong, veryLong/totalw*100,
                             avgLength)
        voc <- cbind(voc, c(mean(totalt),
                            mean(uniquePerDoc), mean(uniquePerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(hapaxPerDoc), mean(hapaxPerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(totalw),
                            mean(longPerDoc), mean(longPerDoc/totalwPerDoc, na.rm=TRUE)*100,
                            mean(veryLongPerDoc), mean(veryLongPerDoc/totalwPerDoc, na.rm=TRUE)*100,
                            mean(avgLengthPerDoc, na.rm=TRUE)))
        colnames(voc)[ncol(voc)] <- gettext_("Corpus")
        lab <- gettext_("Per document mean:")
    }
    else {
        totalt <- tapply(totaltPerDoc, var, sum)
        unique <- tapply(uniquePerDoc, var, sum)
        hapax <- tapply(hapaxPerDoc, var, sum)
        totalw <- tapply(totalwPerDoc, var, sum)
        long <- tapply(longPerDoc, var, sum)
        veryLong <- tapply(veryLongPerDoc, var, sum)
        avgLength <- tapply(weightedLengths, var, sum, na.rm=TRUE)/totalw
        voc <- rbind(totalt, unique, unique/totalt*100,
                             hapax, hapax/totalt*100,
                     totalw, long, long/totalw*100,
                             veryLong, veryLong/totalw*100,
                             avgLength)
        voc <- cbind(voc, c(sum(totalt), sum(unique), sum(unique)/sum(totalt)*100,
                                         sum(hapax), sum(hapax)/sum(totalt)*100,
                            sum(totalw), sum(long), sum(long)/sum(totalw)*100,
                                         sum(veryLong), sum(veryLong)/sum(totalw)*100,
                                         sum(weightedLengths)/sum(totalw)))
        colnames(voc)[ncol(voc)] <- gettext_("Corpus")
        lab <- gettext_("Per category total:")
    }


    voc <- as.table(voc)
    rownames(voc) <- c(gettext_("Number of terms"),
                       gettext_("Number of unique terms"),
                       gettext_("Percent of unique terms"),
                       gettext_("Number of hapax legomena"),
                       gettext_("Percent of hapax legomena"),
                       gettext_("Number of words"),
                       gettext_("Number of long words"),
                       gettext_("Percent of long words"),
                       gettext_("Number of very long words"),
                       gettext_("Percent of very long words"),
                       gettext_("Average word length"))
    names(dimnames(voc)) <- c(lab, "")

    voc
}

docVocabularyDlg <- function() {
    initializeDialog(title=gettext_("Vocabulary Summary per Document"))

    checkBoxes(frame="whatFrame",
               title=gettext_("Draw plot for:"),
               boxes=c("totalt", "unique", "hapax",
                       "totalw", "long", "vlong", "longavg"),
               initialValues=c(0, 0, 1, 0, 0, 0, 0),
               labels=c(gettext_("All terms"),
                        gettext_("Unique terms"),
                        gettext_("Hapax legomena"),
                        gettext_("All words"),
                        gettext_("Long words"),
                        gettext_("Very long words"),
                        gettext_("Average word length")))

    measureVariable <- tclVar("percent")
    measureFrame <- tkframe(top)
    measure1 <- ttkradiobutton(measureFrame, variable=measureVariable, value="percent", text=gettext_("Percent"))
    measure2 <- ttkradiobutton(measureFrame, variable=measureVariable, value="count", text=gettext_("Absolute counts"))

    radioButtons(name="corpusMeasure",
                 title=gettext_("Plot global corpus value:"),
                 buttons=c("none", "mean", "total"),
                 initialValue="mean",
                 labels=c(gettext_("Nothing"),
                          gettext_("Corpus mean"),
                          gettext_("Corpus total")),
                 right=FALSE)

    titleFrame <- tkframe(top)
    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        title <- tclvalue(tclTitle)
        totalt <- tclvalue(totaltVariable) == 1
        unique <- tclvalue(uniqueVariable) == 1
        hapax <- tclvalue(hapaxVariable) == 1
        totalw <- tclvalue(totalwVariable) == 1
        long <- tclvalue(longVariable) == 1
        vlong <- tclvalue(vlongVariable) == 1
        longavg <- tclvalue(longavgVariable) == 1
        corpusMeasure <- tclvalue(corpusMeasureVariable)
        measure <- tclvalue(measureVariable)

        closeDialog()

        # Only compute the dtm the first time this operation is run
        if(!exists("wordsDtm")) {
            doItAndPrint("dtmCorpus <- corpus")

            if(meta(corpus, type="corpus", tag="language") == "fr")
                doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"[\'\U2019-]\", \" \", x))")

            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removePunctuation)")
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removeNumbers)")
            doItAndPrint("wordsDtm <- DocumentTermMatrix(dtmCorpus, control=list(wordLengths=c(2, Inf)))")
            doItAndPrint("rm(dtmCorpus)")
        }

        doItAndPrint("voc <- vocabularyTable(dtm, wordsDtm)")

        # Plot
        if(measure == "percent")
            measures <- c(FALSE, FALSE, unique, FALSE, hapax, FALSE,
                          FALSE, long, FALSE, vlong, longavg)
        else
            measures <- c(totalt, unique, FALSE, hapax, FALSE, totalw,
                          long, FALSE, vlong, FALSE, longavg)

        if(any(measures)) {
            indexes <- paste(which(measures), collapse=", ")

            if(corpusMeasure == "mean")
                exclude <- sprintf(" -%s", ncol(voc))
            else if(corpusMeasure == "total")
                exclude <- sprintf(" -%s", ncol(voc)-1)
            else
                exclude <- sprintf(" -c(%s, %s)", ncol(voc)-1, ncol(voc))

            if(sum(measures) > 1)
                doItAndPrint(sprintf('barchart(t(voc[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", auto.key=list(space="bottom"), ylim=c(0, max(voc[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))
            else
                doItAndPrint(sprintf('barchart(t(voc[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", ylim=c(0, max(voc[c(%s), %s])*1.1))',
                                     indexes, exclude, rownames(voc)[measures], title, indexes, exclude))
        }

        doItAndPrint("print(round(voc, d=1))")

        activateMenus()
        tkfocus(CommanderWindow())
    }


    OKCancelHelp(helpSubject="docVocabularyDlg")
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=3)
    tkgrid(labelRcmdr(measureFrame, text=gettext_("Plotting measure:")),
           measure1, measure2, sticky="w", padx=6)
    tkgrid(measureFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(labelRcmdr(titleFrame, text=gettext_("Plot title:")), titleEntry, sticky="w", padx=6)
    tkgrid(corpusMeasureFrame, sticky="w", pady=6, padx=6, columnspan=3)
    tkgrid(titleFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(buttonsFrame, sticky="w", columnspan=3, pady=6)
    dialogSuffix(rows=5, columns=3)
}

varVocabularyDlg <- function() {
    if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Vocabulary Summary per Variable"))

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
               boxes=c("totalt", "unique", "hapax",
                       "totalw", "long", "vlong", "longavg"),
               initialValues=c(0, 0, 1, 0, 0, 0, 0),
               labels=c(gettext_("All terms"),
                        gettext_("Unique terms"),
                        gettext_("Hapax legomena"),
                        gettext_("All words"),
                        gettext_("Long words"),
                        gettext_("Very long words"),
                        gettext_("Average word length")))

    measureVariable <- tclVar("percent")
    measureFrame <- tkframe(top)
    measure1 <- ttkradiobutton(measureFrame, variable=measureVariable, value="percent",
                               text=gettext_("Percent"))
    measure2 <- ttkradiobutton(measureFrame, variable=measureVariable, value="count",
                               text=gettext_("Absolute counts"))

    corpusMeasureVar <- tclVar("0")
    checkCorpusMeasure <- tkcheckbutton(top, text=gettext_("Plot global corpus value"),
                                        variable=corpusMeasureVar)

    titleFrame <- tkframe(top)
    tclTitle <- tclVar("")
    titleEntry <- ttkentry(top, width="20", textvariable=tclTitle)

    onOK <- function() {
        var <- getSelection(varBox)
        title <- tclvalue(tclTitle)
        unit <- tclvalue(unitVariable)
        totalt <- tclvalue(totaltVariable) == 1
        unique <- tclvalue(uniqueVariable) == 1
        hapax <- tclvalue(hapaxVariable) == 1
        totalw <- tclvalue(totalwVariable) == 1
        long <- tclvalue(longVariable) == 1
        vlong <- tclvalue(vlongVariable) == 1
        longavg <- tclvalue(longavgVariable) == 1
        measure <- tclvalue(measureVariable)
        corpusMeasure <- tclvalue(corpusMeasureVar) == 1

        closeDialog()

        # Only compute the dtm the first time this operation is run
        if(!exists("wordsDtm")) {
            doItAndPrint("dtmCorpus <- corpus")

            if(meta(corpus, type="corpus", tag="language") == "french")
                doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"[\'\U2019-]\", \" \", x))")

            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removePunctuation)")
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removeNumbers)")
            doItAndPrint("wordsDtm <- DocumentTermMatrix(dtmCorpus, control=list(wordLengths=c(2, Inf)))")
            doItAndPrint("rm(dtmCorpus)")
        }

        doItAndPrint(sprintf('voc <- vocabularyTable(dtm, wordsDtm, "%s", "%s")', var, unit))

        # Plot
        if(measure == "percent")
            measures <- c(FALSE, FALSE, unique, FALSE, hapax, FALSE,
                          FALSE, long, FALSE, vlong, longavg)
        else
            measures <- c(totalt, unique, FALSE, hapax, FALSE, totalw,
                          long, FALSE, vlong, FALSE, longavg)

        if(any(measures)) {
            indexes <- paste(which(measures), collapse=", ")

            if(corpusMeasure)
                exclude <- ""
            else
                exclude <- sprintf(" -%s", ncol(voc))

            if(sum(measures) > 1)
                doItAndPrint(sprintf('barchart(t(voc[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="", main="%s", auto.key=list(space="bottom"), ylim=c(0, max(voc[c(%s), %s])*1.1))',
                                     indexes, exclude, title, indexes, exclude))
            else
                doItAndPrint(sprintf('barchart(t(voc[c(%s),%s, drop=FALSE]), stack=FALSE, horizontal=FALSE, scales=list(rot=90), ylab="%s", main="%s", ylim=c(0, max(voc[c(%s), %s])*1.1))',
                                     indexes, exclude, rownames(voc)[measures], title, indexes, exclude))

        }

        doItAndPrint("print(round(voc, d=1))")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varVocabularyDlg")
    tkgrid(getFrame(varBox), sticky="w", columnspan=3, pady=6, padx=6)
    tkgrid(unitFrame, sticky="w", columnspan=3, padx=6)
    tkgrid(whatFrame, sticky="w", pady=6, padx=6, columnspan=3)
    tkgrid(labelRcmdr(measureFrame, text=gettext_("Plotting measure:")),
           measure1, measure2, sticky="w", padx=6)
    tkgrid(measureFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(checkCorpusMeasure, sticky="w", pady=6, columnspan=3)
    tkgrid(labelRcmdr(titleFrame, text=gettext_("Plot title:")), titleEntry, sticky="w", padx=6)
    tkgrid(titleFrame, sticky="w", pady=6, columnspan=3)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=3)
    dialogSuffix(rows=7, columns=3)
}

copyVocabularyTable <- function() {
    R2HTML::HTML2clip(round(voc, d=1))
}

