setOutputFile <- function() {
    if(exists(".HTML.file"))
        dir <- dirname(.HTML.file)
    else
        dir <- "."

    file <- tclvalue(tkgetSaveFile(title=gettext_("Select a file to save results"),
                                   filetypes=sprintf("{{%s} {.html}}",
                                                     gettext_("HTML file")),
                                   defaultextension=".html",
                                   initialdir=dir,
                                   parent=CommanderWindow()))

    if (file == "") return(FALSE)

    doItAndPrint(sprintf('.HTML.file <- "%s"', file))
    doItAndPrint(sprintf('HTML.title("%s", 1, append=FALSE)', gettext_("Text Mining Analysis Results")))

    # Set options for good formatting
    options(R2HTML.format.decimal.mark=gettext_("."))

    # The openOutputFile menu needs to notice the new file
    activateMenus()

    return(TRUE)
}

openOutputFile <- function() {
    if(!exists(".HTML.file")) {
        Message(gettext_("No output file has been created yet."), type="error")
        return()
    }
    else if(!file.exists(.HTML.file)) {
        Message(gettext_("Output file does not exist (it was probably removed)."), type="error")
        return()
    }

    doItAndPrint("browseURL(.HTML.file)")
}

copyTableToOutput <- function() {
    if(!exists("last.table") || !exists(last.table)) {
        Message(gettext_("No table has been built. Please create a table first."), type="error")
        return()
    }

    html.on <- exists(".HTML.file") && file.exists(.HTML.file)
    if(!(html.on || setOutputFile()))
        return()


    title <- attr(get(last.table), "title")
    if(length(title) > 0)
        doItAndPrint(sprintf("HTML.title('%s', 3)", title))

    # zoo objects are printed as plain text by default
    if(inherits(get(last.table), "zoo"))
        doItAndPrint(sprintf('HTML(as.matrix(%s), Border=NULL, align="left")', last.table))
    # align="left" does not work for lists, it goes to cat() and appears in plain text
    else if(class(get(last.table)) == "list")
        doItAndPrint(sprintf('HTML(%s, Border=NULL)', last.table))
    else
        doItAndPrint(sprintf('HTML(%s, Border=NULL, align="left")', last.table))

    # Open file in browser when creating it
    if(!html.on)
        doItAndPrint("browseURL(.HTML.file)")

    # If output file was removed, we recreate it, and the openOutputFile menu needs to notice it
    activateMenus()
}

copyPlotToOutput <- function() {
    if(length(dev.list()) == 0) {
        Message(gettext_("No plot has been drawn. Please create a plot first."), type="error")
        return()
    }

    html.on <- exists(".HTML.file") && file.exists(.HTML.file)
    if(!(html.on || setOutputFile()))
        return()

    file <- gsub(".html$", paste(format(Sys.time(), gettext_(" - plot %Y-%m-%d %H:%M")),
                                 ".png", sep=""), .HTML.file)

    i <- 1
    testfile <- file
    while(file.exists(testfile)) {
        i <- i + 1
        testfile <- gsub(".html$", paste(format(Sys.time(), gettext_(" - plot %Y-%m-%d %H:%M")),
                                         "-", i, ".png", sep=""), .HTML.file)
    }

    if(file.exists(file))
        file <- testfile

    doItAndPrint(sprintf('dev.print(png, width=7, height=7, unit="in", res=200, filename="%s")', file))
    doItAndPrint(sprintf('HTMLInsertGraph("%s", "", 0, "left")', file))

    # Open file in browser when creating it
    if(!html.on)
        doItAndPrint("browseURL(.HTML.file)")

    # If output file was removed, we recreate it, and the openOutputFile menu needs to notice it
    activateMenus()
}

