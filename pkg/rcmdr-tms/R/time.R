varTimeSeriesDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=gettext_("No corpus variables have been set. Use Text mining->Manage corpus->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Corpus Temporal Evolution"))

    vars <- colnames(meta(corpus))[colnames(meta(corpus)) != "MetaID"]

    datevar <- which(vars %in% gettext_("Date")) - 1
    timeVarBox <- variableListBox(top, vars,
                                  title=gettext_("Time variable:"),
                                  initialSelection=if(length(datevar) > 0) datevar else 0)

    tclFormat <- tclVar("%Y-%m-%d")
    formatEntry <- ttkentry(top, width="20", textvariable=tclFormat)

    # We cannot use variableListBox as it is not meant for changing levels
    varsFrame <- tkframe(top)
    varsBox <- tklistbox(varsFrame, height=getRcmdr("variable.list.height"),
                         selectmode="single", export=FALSE)
    varsScrollbar <- ttkscrollbar(varsFrame, command=function(...) tkyview(varsBox, ...))
    tkconfigure(varsBox, yscrollcommand=function(...) tkset(varsScrollbar, ...))
    for(var in c(gettext_("None (one curve)"), vars)) tkinsert(varsBox, "end", var)
    datevar <- which(vars %in% gettext_("Date"))
    tkselection.set(varsBox, 0)

    levelsFrame <- tkframe(top)
    levelsBox <- tklistbox(levelsFrame, height=getRcmdr("variable.list.height"),
                           selectmode=getRcmdr("multiple.select.mode"), export=FALSE)
    levelsScrollbar <- ttkscrollbar(levelsFrame, command=function(...) tkyview(levelsBox, ...))
    tkconfigure(levelsBox, yscrollcommand=function(...) tkset(levelsScrollbar, ...))
    tkselection.set(levelsBox, 0)

    onSelect <- function() {
        var <- c("", vars)[as.numeric(tkcurselection(varsBox))+1]
        tkdelete(levelsBox, "0", "end")

        if(var == "")
            return()

        levs <- unique(meta(corpus, var)[[1]])
        for(level in levs) tkinsert(levelsBox, "end", level)

        tkselection.set(levelsBox, 0, "end")
    }

    tkbind(varsBox, "<<ListboxSelect>>", onSelect)


    tclMean <- tclVar(0)
    meanButton <- tkcheckbutton(top, text=gettext_("Apply rolling mean"), variable=tclMean)

    tclWindow <- tclVar(7)
    sliderWindow <- tkscale(top, from=1, to=30,
                            showvalue=TRUE, variable=tclWindow,
	                    resolution=1, orient="horizontal")

    tclTitle <- tclVar(gettext_("Temporal evolution of the corpus"))
    titleEntry <- ttkentry(top, width="30", textvariable=tclTitle)

    onOK <- function() {
        timeVar <- getSelection(timeVarBox)
        groupVar <- c("", vars)[as.numeric(tkcurselection(varsBox))+1]
        rollmean <- tclvalue(tclMean) == 1
        window <- as.numeric(tclvalue(tclWindow))
        title <- tclvalue(tclTitle)

        if(nchar(groupVar) > 0)
            groupLevs <- unique(meta(corpus, groupVar)[[1]])[as.numeric(tkcurselection(levelsBox))+1]

        format <- tclvalue(tclFormat)

        # Check that format is more or less correct before running the code
        time <- meta(corpus, timeVar)[[1]]
        time <- strptime(unique(time[!is.na(time)]), format)
        if(all(is.na(time))) {
            Message(message=sprintf(gettext_('Incorrect date format: no values of "%s" could be converted to a time index.'), timeVar),
                    type="error")
            return()
        }
        else if(any(is.na(time))) {
            Message(message=sprintf(gettext_('Some values of "%s" could be converted to a time index and will be missing.'), timeVar),
                    type="warning")
        }

        closeDialog()

        if(nchar(groupVar) == 0) {
            doItAndPrint(sprintf('tab <- table(meta(corpus, "%s"))', timeVar))
            doItAndPrint(sprintf('time <- as.POSIXct(strptime(names(tab), "%s"))', format))
        }
        else {
            if(length(groupLevs) < length(unique(meta(corpus, var)[[1]])))
                doItAndPrint(sprintf('tab <- table(meta(corpus, c("%s", "%s")))[,c("%s")]',
                                     timeVar, groupVar, paste(groupLevs, collapse='", "')))
            else
                doItAndPrint(sprintf('tab <- table(meta(corpus, c("%s", "%s")))', timeVar, groupVar))

            doItAndPrint(sprintf('time <- as.POSIXct(strptime(rownames(tab), "%s"))', format))
        }

        # Trick to get a regular time series, since rollmeans() uses preceding and following points,
        # without considering the spacing
        doItAndPrint('docSeries <- merge(zoo(tab, order.by=time), zoo(, seq(min(time), max(time), min(diff(time)))), fill=0)')

        if(rollmean) {
            if(window >= length(docSeries))
                Message(message=gettext_("Chosen roll mean window is longer than the range of the time variable, rolling mean was not applied."),
                        type="warning")
            else
                doItAndPrint(sprintf('docSeries <- rollmean(docSeries, %s, align="left")', window))
        }

        doItAndPrint(sprintf('xyplot(docSeries, superpose=TRUE, xlab="", ylab="%s", main="%s", auto.key=%s)',
                             gettext_("Number of documents"), title,
                             if(NCOL(docSeries) > 1) 'list(space="bottom")' else "NULL"))

        doItAndPrint("rm(tab, time)")
        doItAndPrint("print(docSeries)")

        # Used by saveTableToOutput()
        last.table <<- "docSeries"
        attr(docSeries, "title") <<- title

        activateMenus()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="varTimeSeriesDlg")
    tkgrid(getFrame(timeVarBox), sticky="ewns", pady=6, row=0, rowspan=3)
    tkgrid(labelRcmdr(top, text=gettext_("Time format:")), pady=c(6, 0), sticky="w", row=0, column=1)
    tkgrid(formatEntry, sticky="w", row=1, column=1)
    tkgrid(labelRcmdr(top, text=gettext_('%Y: year - %m: month - %d: day\nClick the "Help" button for more codes.')), sticky="w", row=2, column=1, pady=6)
    tkgrid(labelRcmdr(varsFrame, text=gettext_("Group by variable:"), foreground="blue"), sticky="w", pady=c(12, 0))
    tkgrid(varsBox, varsScrollbar, sticky="ewns", pady=6)
    tkgrid(varsFrame, levelsFrame, sticky="ewns", pady=6)
    tkgrid(labelRcmdr(levelsFrame, text=gettext_("Only plot levels:")), sticky="w", pady=c(12, 0))
    tkgrid(levelsBox, levelsScrollbar, sticky="ewns", pady=6)
    tkgrid(labelRcmdr(top, text=gettext_("Rolling mean:"), foreground="blue"), sticky="w", pady=c(6, 0))
    tkgrid(meanButton, sticky="w")
    tkgrid(labelRcmdr(top, text=gettext_("Time window for mean (in days):")), sliderWindow, sticky="w",
           padx=6, pady=c(0, 6))
    tkgrid(labelRcmdr(top, text=gettext_("Title:"), foreground="blue"), sticky="w", pady=c(6, 0))
    tkgrid(titleEntry, sticky="w", padx=6, pady=c(0, 6), columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=12, columns=2, focus=timeVarBox$listbox)
}

