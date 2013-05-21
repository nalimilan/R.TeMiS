if (getRversion() >= '2.15.1') globalVariables(c(
    "tl", ".HTML.file",
    "last.table", "top", "dtm", "corpus", "corpusCa", "buttonsFrame",
    "clustDtm", "corpusClust", "clusters", "corpusVars", "diss",
    "freqTerms", "sourceVariable", "lowercaseVariable",
    "punctuationVariable", "numbersVariable", "stopwordsVariable",
    "stemmingVariable", "sourceFrame", "processingFrame", "foreign",
    "channel", "corpusDataset", "messages",
    "messageBox", "whatVariable", "whatFrame",
    "wordsDtm", "docLabelsVariable", "termLabelsVariable",
    "varLabelsVariable", "docPointsVariable", "termPointsVariable",
    "varPointsVariable", "labelsFrame", "pointsFrame",
    "preventDoubleClick", "termFreqs", "termSeries",
    "coocs", "specTerms", "varFreqs", "docSeries", "docButton",
    "globalButton", "unitVariable", "totaltVariable", "uniqueVariable",
    "hapaxVariable", "totalwVariable", "longVariable", "vlongVariable",
    "longavgVariable", "voc", "unitFrame", "digitsVariable",
    "exclRetweetsVariable", "removeNamesVariable", "removeHashtagsVariable",
    "optionsFrame"
    ))


.setBusyCursor <- function() {
    .commander <- CommanderWindow()
    .menu <- tkcget(.commander, menu=NULL)
    .log <- LogWindow()
    .output <- OutputWindow()
    .messages <- MessagesWindow()

    tkconfigure(.commander, cursor="watch")
    tkconfigure(.menu, cursor="watch")
    tkconfigure(.log, cursor="watch")
    tkconfigure(.output, cursor="watch")
    tkconfigure(.messages, cursor="watch")
}

.setIdleCursor <- function() {
    .commander <- CommanderWindow()
    .menu <- tkcget(.commander, menu=NULL)
    .log <- LogWindow()
    .output <- OutputWindow()
    .messages <- MessagesWindow()

    tkconfigure(.commander, cursor="")
    tkconfigure(.menu, cursor="")
    tkconfigure(.log, cursor="xterm")
    tkconfigure(.output, cursor="xterm")
    tkconfigure(.messages, cursor="xterm")
}

.getCorpusWindow <- function() {
    if(exists("corpusTxt", envir=Rcmdr:::RcmdrEnv()) &&
       !is.null(getRcmdr("corpusTxt"))) {
        window <- getRcmdr("corpusWindow")
        txt <- getRcmdr("corpusTxt")
        listbox <- getRcmdr("corpusList")
        tkdelete(txt, "0.0", "end")
        tkdelete(listbox, 0, "end")
    }
    else {
        window <- tktoplevel(class="Rcommander")
        tkwm.geometry(window, "-0+20")
        scr1 <- tkscrollbar(window, repeatinterval=5,
                           command=function(...) tkyview(txt,...))
        txt <- tktext(window, bg="white", font="courier 11", wrap="word",
                      width=getOption("width", 80),
                      yscrollcommand=function(...) tkset(scr1, ...))

        tktag.configure(txt, "body", font="times")
        tktag.configure(txt, "heading", font="sans 13 bold")
        tktag.configure(txt, "articlehead", font="sans 12 bold")
        tktag.configure(txt, "details", font="sans 10 italic")
        tktag.configure(txt, "small", font="sans 5")
        tktag.configure(txt, "fixed", font="courier 11")

        tkpack(txt, side="left", fill="both", expand=TRUE)
        tkpack(scr1, side="left", fill="y")

        scr2 <- tkscrollbar(window, repeatinterval=5,
                            command=function(...) tkyview(listbox, ...))
        listbox <- tklistbox(window, selectmode="single",
                             yscrollcommand=function(...) tkset(scr2,...))
        tkpack(listbox, side="left", fill="y")
        tkpack(scr2, side="left", fill="y")

        tkbind(listbox, "<<ListboxSelect>>", function() {
            tkyview(txt, paste("mark", tkcurselection(listbox), sep=""))
        })

        putRcmdr("corpusWindow", window)
        putRcmdr("corpusTxt", txt)
        putRcmdr("corpusList", listbox)

	tkwm.protocol(window, "WM_DELETE_WINDOW", function() {
            tkdestroy(getRcmdr("corpusWindow"))
            putRcmdr("corpusWindow", NULL)
            putRcmdr("corpusTxt", NULL)
            putRcmdr("corpusList", NULL)
        })
    }

    list(window=window, txt=txt, listbox=listbox)
}

.checkAndInstall <- function(package, message, repos=NULL) {
    if(!package %in% rownames(installed.packages())) {
            # Create a function because dialog does not close until function returns
            msgbox <- function() tkmessageBox(title=.gettext("Package required"), message=message,
                                              icon="question", type="yesno")

            if (tclvalue(msgbox()) != "yes")
                return(FALSE)

            .setBusyCursor()
            on.exit(.setIdleCursor())

            # If repos is specified, assumed the caller knows that the package is available
            if(!is.null(repos) || package %in% available.packages()[,1]) {
                install.packages(package, repos=if(is.null(repos)) getOption("repos") else repos)
            }
            else {
                tkmessageBox(title=.gettext("Package not available"),
                             message=sprintf(.gettext("Package %s is not available. Please check your Internet connection, restart R and try again."),
                                             package),
                             icon="error", type="ok")
                return(FALSE)
            }
    }

    if(!require(package, character.only=TRUE)) {
        if(package == "Snowball")
            tkmessageBox(title=.gettext("Could not load package"),
                         message=.gettext("Package Snowball could not be loaded. See errors in the \"Messages\" area.\n\nThis is usually due to Java problems. Please download the most recent version of Java, and check that you are using the 32-bit version of R if you have a 32-bit Java, and the 64-bit version otherwise."),
                         icon="error", type="ok")
        else
            tkmessageBox(title=.gettext("Could not load package"),
                         message=sprintf(.gettext("Package %s could not be loaded. See errors in the \"Messages\" area."), package),
                         icon="error", type="ok")

        return(FALSE)
    }

    return(TRUE)
}

