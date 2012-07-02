# This function is a convenience wrapper to gettext_() which
# passes the domain name and avoid repeating it in the code
gettext_ <- function(msg) {
    gettext(msg, domain="R-RcmdrPlugin.TextMiningSuite")
}

ngettext_ <- function(n, msg1, msg2) {
    ngettext(n, msg1, msg2, domain="R-RcmdrPlugin.TextMiningSuite")
}

