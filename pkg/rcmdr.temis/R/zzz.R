.onAttach <- function(libname, pkgname){
        if (!interactive()) return()
        Rcmdr <- options()$Rcmdr
        plugins <- Rcmdr$plugins
        if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
                Rcmdr$plugins <- c(plugins, pkgname)
                options(Rcmdr=Rcmdr)
                closeCommander(ask=FALSE, ask.save=TRUE)

                # Work around a bug with Java on Mac OS blocking Snowball and RWeka
                if (Sys.info()["sysname"] == "Darwin")  {
                        Sys.setenv(NOAWT="true")
                }

                # Work around a bug in JGR: restore the correct locale
                # https://www.rforge.net/bugzilla/show_bug.cgi?id=244
                if (Sys.info()["sysname"] == "Darwin" && Sys.getlocale() == "C")  {
                        loc <- system("defaults read -g AppleLocale", intern=TRUE)
                        Sys.setlocale("LC_ALL", loc)
                        rm(loc)
                }

                Commander()


                # HTML.matrix() does not allow passing scientific separately,
                # and vocabulary summary tables often end up printed in scientific notation
                doItAndPrint(.gettext("# Prefer fixed to scientific notation"))
                doItAndPrint('options(scipen=5)')

                # Keep in sync with disableBlackAndWhite()
                doItAndPrint("")
                doItAndPrint(.gettext("# Set a nice color palette for plots"))
                doItAndPrint('lattice.options(default.theme=custom.theme(symbol=brewer.pal(8, "Set1")[c(2:1, 3:5, 7:9)], fill=brewer.pal(8, "Set1")[c(2:1, 3:5, 7:9)]))')
        }
}

