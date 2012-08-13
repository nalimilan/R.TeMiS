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
    if(exists("corpusTxt", "RcmdrEnv") &&
       !is.null(get("corpusTxt", "RcmdrEnv"))) {
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

.checkAndInstall <- function(package, message) {
    if(!suppressWarnings(require(package, character.only=TRUE, quietly=TRUE))) {
            # Create a function because dialog does not close until function returns
            msgbox <- function() tkmessageBox(message=message, icon="question", type="yesno")

            if (tclvalue(msgbox()) == "yes") {
                .setBusyCursor()

                if(package %in% available.packages()[,1]) {
                    install.packages(package)
                    .setIdleCursor()
                    return(TRUE)
                }
                else {
                    Message(.gettext("Package not available. Please check your Internet connection, restart R and try again."),
                            type="error")
                }
            }

        .setIdleCursor()
        return(FALSE)
    }

    return(TRUE)
}

