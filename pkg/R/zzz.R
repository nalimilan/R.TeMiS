.onAttach <- function(libname, pkgname){
        if (!interactive()) return()
        Rcmdr <- options()$Rcmdr
        plugins <- Rcmdr$plugins
        if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
                Rcmdr$plugins <- c(plugins, pkgname)
                options(Rcmdr=Rcmdr)
                closeCommander(ask=FALSE, ask.save=TRUE)

                # Work around a bug in JGR: restore the correct locale
                # https://www.rforge.net/bugzilla/show_bug.cgi?id=244
                if (Sys.info()["sysname"] == "Darwin" && Sys.getlocale() == "C")  {
                        loc <- system("defaults read -g AppleLocale", intern=TRUE)
                        Sys.setlocale("LC_ALL", loc)
                        rm(loc)
                }

                Commander()
        }
}

