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
