setBusyCursor <- function() {
    .commander <- CommanderWindow()
    .log <- LogWindow()
    .output <- OutputWindow()
    .messages <- MessagesWindow()

    tkconfigure(.commander, cursor="watch")
    tkconfigure(.log, cursor="watch")
    tkconfigure(.output, cursor="watch")
    tkconfigure(.messages, cursor="watch")
}

setIdleCursor <- function() {
    .commander <- CommanderWindow()
    .log <- LogWindow()
    .output <- OutputWindow()
    .messages <- MessagesWindow()

    tkconfigure(.commander, cursor="")
    tkconfigure(.log, cursor="xterm")
    tkconfigure(.output, cursor="xterm")
    tkconfigure(.messages, cursor="xterm")
}

getCorpusWindow <- function() {
    if(exists("corpusTxt", "RcmdrEnv") &&
       !is.null(get("corpusTxt", "RcmdrEnv"))) {
        window <- getRcmdr("corpusWindow")
        txt <- getRcmdr("corpusTxt")
        listbox <- getRcmdr("corpusList")
        tkwm.title(window, gettext_("Hierarchical Clustering"))
        tkdelete(txt, "0.0", "end")
        tkdelete(listbox, 0, "end")
    }
    else {
        window <- tktoplevel(class="Rcommander")
        tkwm.title(window, gettext_("Hierarchical Clustering"))
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

