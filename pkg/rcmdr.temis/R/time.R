varTimeSeriesDlg <- function() {
    nVars <- ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"])
    if(nVars == 0) {
        Message(message=.gettext("No corpus variables have been set. Use Text mining->Manage corpus->Set corpus variables to add them."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Corpus Temporal Evolution"))

    vars <- colnames(meta(corpus))[colnames(meta(corpus)) != "MetaID"]

    datevar <- which(vars == .gettext("Date")) - 1
    timevar <- which(vars == .gettext("Time")) - 1
    datetimevar <- if(length(datevar) > 0) datevar
                   else if(length(timevar) > 0) timevar
                   else 0

    timeVarBox <- variableListBox(top, vars,
                                  title=.gettext("Date/Time variable:"),
                                  initialSelection=datetimevar)

    tclFormat <- if(length(timevar) == 0) tclVar("%Y-%m-%d") else tclVar("%Y-%m-%d %H:%M")
    formatEntry <- ttkentry(top, width="20", textvariable=tclFormat)

    # We cannot use variableListBox as it is not meant for changing levels
    varsFrame <- tkframe(top)
    varsBox <- tklistbox(varsFrame, height=getRcmdr("variable.list.height"),
                         selectmode="single", export=FALSE)
    varsScrollbar <- ttkscrollbar(varsFrame, command=function(...) tkyview(varsBox, ...))
    tkconfigure(varsBox, yscrollcommand=function(...) tkset(varsScrollbar, ...))
    for(var in c(.gettext("None (one curve)"), vars)) tkinsert(varsBox, "end", var)
    tkselection.set(varsBox, 0)

    levelsFrame <- tkframe(top)
    levelsBox <- tklistbox(levelsFrame, height=getRcmdr("variable.list.height"),
                           selectmode=getRcmdr("multiple.select.mode"), export=FALSE)
    levelsScrollbar <- ttkscrollbar(levelsFrame, command=function(...) tkyview(levelsBox, ...))
    tkconfigure(levelsBox, yscrollcommand=function(...) tkset(levelsScrollbar, ...))
    tkselection.set(levelsBox, 0)

    onSelectTimeVar <- function() {
        var <- getSelection(timeVarBox)

        if(var == .gettext("Date")) {
            tkdelete(formatEntry, "0", "end")
            tkinsert(formatEntry, "end", "%Y-%m-%d")
        }
        else if (var == .gettext("Time")) {
            tkdelete(formatEntry, "0", "end")
            tkinsert(formatEntry, "end", "%Y-%m-%d %H:%M")
        }
    }

    tkbind(timeVarBox$listbox, "<<ListboxSelect>>", onSelectTimeVar)

    onSelectGroup <- function() {
        var <- c("", vars)[as.numeric(tkcurselection(varsBox))+1]
        tkdelete(levelsBox, "0", "end")

        if(var == "")
            return()

        levs <- unique(meta(corpus, var)[[1]])
        for(level in levs) tkinsert(levelsBox, "end", level)

        tkselection.set(levelsBox, 0, "end")
    }

    tkbind(varsBox, "<<ListboxSelect>>", onSelectGroup)

    radioButtons(name="what",
                 buttons=c("number", "percent"),
                 labels=c(.gettext("Number of documents per time unit"),
                          .gettext("% of documents per time unit")),
                 title=.gettext("Measure:"),
                 right.buttons=FALSE)

    tclMean <- tclVar(0)
    meanButton <- tkcheckbutton(top, text=.gettext("Apply rolling mean"), variable=tclMean)

    tclWindow <- tclVar(7)
    sliderWindow <- tkscale(top, from=1, to=30,
                            showvalue=TRUE, variable=tclWindow,
	                    resolution=1, orient="horizontal")

    tclTitle <- tclVar(.gettext("Temporal evolution of the corpus"))
    titleEntry <- ttkentry(top, width="30", textvariable=tclTitle)

    onOK <- function() {
        timeVar <- getSelection(timeVarBox)
        groupVar <- c("", vars)[as.numeric(tkcurselection(varsBox))+1]
        what <- tclvalue(whatVariable)
        rollmean <- tclvalue(tclMean) == 1
        window <- as.numeric(tclvalue(tclWindow))
        title <- tclvalue(tclTitle)

        if(what == "percent" && nchar(groupVar) == 0) {
            Message(message=.gettext("Plotting percents of documents with only one curve does not make sense: all points would be 100%."),
                    type="error")
            return()
        }

        if(nchar(groupVar) > 0)
            groupLevs <- unique(meta(corpus, groupVar)[[1]])[as.numeric(tkcurselection(levelsBox))+1]

        format <- tclvalue(tclFormat)

        # Check that format is more or less correct before running the code
        time <- meta(corpus, timeVar)[[1]]
        time <- strptime(unique(time[!is.na(time)]), format)
        if(all(is.na(time))) {
            Message(message=sprintf(.gettext("Incorrect time format: no values of \"%s\" could be converted to a time index."), timeVar),
                    type="error")
            return()
        }
        else if(any(is.na(time))) {
            Message(message=sprintf(.gettext("Some values of \"%s\" could not be converted to a time index and will be missing."), timeVar),
                    type="warning")
        }

        if(nchar(groupVar) == 0) {
            doItAndPrint(sprintf('tab <- table(as.character(strptime(meta(corpus, "%s")[[1]], "%s")))', timeVar, format))
            doItAndPrint(sprintf('time <- as.POSIXct(strptime(names(tab), "%s"))', format))
            doItAndPrint("docSeries <- zoo(tab, order.by=time)")
        }
        else {
            doItAndPrint(sprintf('tab <- table(as.character(strptime(meta(corpus, "%s")[[1]], "%s"), meta(corpus, "%s")[[1]]))',
                                               timeVar, format, groupVar))

            if(what == "percent")
                doItAndPrint("tab <- prop.table(tab, 1)*100")

            doItAndPrint(sprintf('time <- as.POSIXct(strptime(rownames(tab), "%s"))', format))

            if(length(groupLevs) < length(unique(meta(corpus, var)[[1]])))
                doItAndPrint(sprintf('docSeries <- zoo(tab[,c("%s")], order.by=time)',
                                     paste(groupLevs, collapse='", "')))
            else
                doItAndPrint("docSeries <- zoo(tab, order.by=time)")
        }


        # We need to be sure we have a common time unit, i.e. we have a zooreg object
        if(!is.regular(docSeries))
            doItAndPrint('docSeries <- aggregate(docSeries, list(as.POSIXct(trunc(time, units(diff(time))))), regular=TRUE)')


        # For some reason, computing this after merging returns 24 "hours" instead of 1 "day" as unit
        unitsLoc <- c(secs=.gettext("per second"), mins=.gettext("per minute"), hours=.gettext("per hour"),
                      days=.gettext("per day"), weeks=.gettext("per week"))
        unit <- unitsLoc[units(diff(time(docSeries)))]

        # Trick to get a strictly regular time series with 0 where no document was observed
        # difftime chooses the unit so that all differences are > 1, which is what we want
        if(!is.regular(docSeries, strict=TRUE)) {
            # seq() will specify different if we pass "day"
            byUnit <- units(diff(time(docSeries)))
            if(byUnit == "days") byUnit <- "DSTday"

            doItAndPrint(sprintf('docSeries <- merge(docSeries, zoo(, seq(start(docSeries), end(docSeries), "%s")), fill=0)',
                                 byUnit))
        }

        if(rollmean) {
            if(window >= NROW(docSeries))
                Message(message=.gettext("Chosen roll mean window is longer than the range of the time variable, rolling mean was not applied."),
                        type="warning")
            else
                # For percents, the days with no observation get 0/0 == NaN, and we need to skip them
                doItAndPrint(sprintf('docSeries <- rollapply(docSeries, %s, align="left", mean, na.rm=TRUE)', window))
        }

        ylab <- if(what == "number") .gettext("Number of documents") else .gettext("% of documents")
        doItAndPrint(sprintf('xyplot(docSeries, superpose=TRUE, xlab="", ylab="%s", main="%s", auto.key=%s, par.settings=simpleTheme(lwd=1.5))',
                             paste(ylab, unit), title,
                             if(NCOL(docSeries) > 1) 'list(space="bottom")' else "NULL"))

        doItAndPrint("rm(tab, time)")

        if(what == "number")
            doItAndPrint("print(docSeries)")
        else
            doItAndPrint("round(docSeries, digits=2)")

        # Used by saveTableToOutput()
        last.table <<- "docSeries"
        attr(docSeries, "title") <<- paste(title, " (", ylab, " ", unit, ")", sep="")

        activateMenus()
        tkfocus(CommanderWindow())
    }

    buttonsFrame <- tkframe(top, borderwidth=5)
    plotButton <- buttonRcmdr(buttonsFrame, text=.gettext("Draw plot"), foreground="darkgreen",
                              command=onOK, default="active", borderwidth=3)
    onClose <- function() {
        closeDialog()
        tkfocus(CommanderWindow())
    }
    closeButton <- buttonRcmdr(buttonsFrame, text=.gettext("Close"), foreground="red",
                               command=onClose, borderwidth=3)
    onHelp <- function() {
        if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
        print(help("varTimeSeriesDlg"))
    }
    helpButton <- buttonRcmdr(buttonsFrame, text=gettextRcmdr("Help"), width="12",
                              command=onHelp, borderwidth=3)
    tkgrid(plotButton, labelRcmdr(buttonsFrame, text="  "),
           closeButton, labelRcmdr(buttonsFrame, text="            "),
           helpButton, sticky="w")

    tkgrid(getFrame(timeVarBox), sticky="ewns", pady=6, row=0, rowspan=3)
    tkgrid(labelRcmdr(top, text=.gettext("Time format:")), pady=c(6, 0), sticky="w", row=0, column=1)
    tkgrid(formatEntry, sticky="w", row=1, column=1)
    tkgrid(labelRcmdr(top, text=.gettext("%Y: year - %m: month - %d: day\nClick the \"Help\" button for more codes.")),
           sticky="w", row=2, column=1, pady=6)
    tkgrid(labelRcmdr(varsFrame, text=.gettext("Group by variable:"), foreground="blue"), sticky="w", pady=c(12, 0))
    tkgrid(varsBox, varsScrollbar, sticky="ewns", pady=6)
    tkgrid(varsFrame, levelsFrame, sticky="ewns", pady=6)
    tkgrid(labelRcmdr(levelsFrame, text=.gettext("Only plot levels:")), sticky="w", pady=c(12, 0))
    tkgrid(levelsBox, levelsScrollbar, sticky="ewns", pady=6)
    tkgrid(whatFrame, sticky="w", pady=6, columnspan=2)
    tkgrid(labelRcmdr(top, text=.gettext("Rolling mean:"), foreground="blue"), sticky="w", pady=c(6, 0))
    tkgrid(meanButton, sticky="w")
    tkgrid(labelRcmdr(top, text=.gettext("Time window for mean (in time units):")), sliderWindow, sticky="w",
           padx=6, pady=c(0, 6))
    tkgrid(labelRcmdr(top, text=.gettext("Title:"), foreground="blue"), sticky="w", pady=c(6, 0))
    tkgrid(titleEntry, sticky="w", padx=6, pady=c(0, 6), columnspan=2)
    tkgrid(buttonsFrame, sticky="w", pady=6, columnspan=2)
    dialogSuffix(rows=13, columns=2, focus=timeVarBox$listbox, onCancel=onClose)
}

