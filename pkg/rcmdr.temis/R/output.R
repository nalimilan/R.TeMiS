setOutputFile <- function() {
    if(exists(".HTML.file"))
        dir <- dirname(.HTML.file)
    else
        dir <- "."

    file <- tclvalue(tkgetSaveFile(title=.gettext("Select a file to save results"),
                                   filetypes=sprintf("{{%s} {.html}}",
                                                     .gettext("HTML file")),
                                   defaultextension=".html",
                                   initialdir=dir,
                                   parent=CommanderWindow()))

    if (file == "") return(FALSE)

    doItAndPrint(sprintf('initOutputFile("%s")', file))

    # Set options for good formatting
    options(R2HTML.format.decimal.mark=.gettext("."))

    # The openOutputFile menu needs to notice the new file
    activateMenus()

    return(TRUE)
}

initOutputFile <- function(file) {
    title <- .gettext("Text Mining Analysis Results")

    # R2HTML uses cat() to output text, which in turns uses the value of getOption("encoding")
    # By default, this corresponds to native.enc returned by localeToCharset()
    enc <- getOption("encoding", "")

    if(enc %in% c("", "native.enc"))
        enc <- localeToCharset()[1]

    if(is.na(enc)) # In case system encoding could not be detected
       enc <- "UTF-8"

    # R2HTML does not add encoding information to the HTML headers, even when using HTMLInitFile
    header <- sprintf('<head>\n<meta http-equiv="Content-Type" content="text/html; charset=%s"/>\n<title>%s</title>\n</head>\n',
                      enc, title)
    writeLines(header, file)

    .HTML.file <<- file
    HTML.title(title, 1, append=TRUE)
}

openOutputFile <- function() {
    if(!exists(".HTML.file")) {
        Message(.gettext("No report file has been created yet."), type="error")
        return()
    }
    else if(!file.exists(.HTML.file)) {
        Message(.gettext("Report file does not exist (it was probably removed)."), type="error")
        return()
    }

    doItAndPrint("browseURL(.HTML.file)")
}

copyTableToOutput <- function() {
    if(!exists("last.table") || !exists(last.table)) {
        Message(.gettext("No table has been built yet. Please create a table first."), type="error")
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
        doItAndPrint(sprintf('HTML(as.matrix(%s), Border=NULL, align="left", scientific=4)', last.table))
    # Arrays returned by tapply() (i.e. freqTerms) are not printed correclty by HTML.array
    else if(class(get(last.table)) == "array")
        doItAndPrint(sprintf('HTML(c(%s), Border=NULL, align="left", scientific=4)', last.table))
    else
        doItAndPrint(sprintf('HTML(%s, Border=NULL, align="left", scientific=4)', last.table))

    # Open file in browser when creating it
    if(!html.on)
        doItAndPrint("browseURL(.HTML.file)")

    # If output file was removed, we recreate it, and the openOutputFile menu needs to notice it
    activateMenus()
}

copyPlotToOutput <- function() {
    if(length(dev.list()) == 0) {
        Message(.gettext("No plot has been drawn yet. Please create a plot first."), type="error")
        return()
    }

    html.on <- exists(".HTML.file") && file.exists(.HTML.file)
    if(!(html.on || setOutputFile()))
        return()

    # Only the filename within the folder is needed, this allows moving HTML and PNG files to another folder
    htmlFilename <- gsub(sprintf(".*%s", .Platform$file.sep), "", .HTML.file)

    file <- gsub(".html$", paste(format(Sys.time(), .gettext(" - plot %Y-%m-%d %H-%M")),
                                 ".png", sep=""), htmlFilename)

    i <- 1
    testfile <- file
    while(file.exists(testfile)) {
        i <- i + 1
        testfile <- gsub(".html$", paste(format(Sys.time(), .gettext(" - plot %Y-%m-%d %H-%M")),
                                         "-", i, ".png", sep=""), htmlFilename)
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

enableBlackAndWhite <- function() {
    doItAndPrint("lattice.options(default.theme=standard.theme(color=FALSE))")

    # Update current plot if there is one
    if(dev.cur() > 1) {
        doItAndPrint("trellis.device(new=FALSE)")
        doItAndPrint("trellis.last.object()")
    }

    options(bw.plots=TRUE)

    activateMenus()
}

disableBlackAndWhite <- function() {
    # Keep in sync with .onAttach()
    doItAndPrint('lattice.options(default.theme=custom.theme(symbol=brewer.pal(8, "Set1")[c(2:1, 3:5, 7:9)], fill=brewer.pal(8, "Set1")[c(2:1, 3:5, 7:9)]))')

    # Update current plot if there is one
    if(dev.cur() > 1) {
        doItAndPrint("trellis.device(new=FALSE)")
        doItAndPrint("trellis.last.object()")
    }

    options(bw.plots=FALSE)

    activateMenus()
}

# The default HTML.list function does not print element names,
# and redirects align="left" to cat(), which prints it to the file
HTML.list <- function (x, file = get(".HTML.file"), first = TRUE, append = TRUE, ...) 
{
    cat("\n", file = file, append = append)
    if (first)
        HTML("<hr class='hr'>", file = file, append = TRUE, sep = "\n")

    for (i in 1:length(x)) {
        cat("<ul>", file = file, append = TRUE, sep = "\n")
        cat("</center><li>", file = file, append = TRUE, sep = "\n")
        HTML(paste(names(x)[i], "\n", sep=""), file = file, first = FALSE, ...)

        if(length(x[[i]]) > 0)
            HTML(x[[i]], file = file, first = FALSE, ...)
        else
            HTML(.gettext("No items."), file = file, first = FALSE, ...)

        cat("</ul>", file = file, append = TRUE, sep = "\n")
    }
    cat("\n<br><hr class='hr'>", file = file, append = TRUE,
        sep = "\n")
}

# This function is a slightly modified version of print.ca() from package ca.
# Released under the GPL (no version specified), Copyright Michael Greenacre
# and Oleg Nenadic <onenadi at uni-goettingen.de>.
# http://cran.r-project.org/web/packages/ca/index.html
HTML.ca <- function(obj, ...) {
    nd0 <- length(obj$sv)
    nd  <- obj$nd
    if (is.na(nd)){
        nd <- 2
    }
    else {
        if (nd > length(obj$sv)) nd <- length(obj$sv)
    }

    # Eigenvalues:
    Dimension <- 1:nd
    Value <- round(obj$sv[1:nd]^2, 6)
    Percentage <- paste(as.character(round(100 * Value / sum(Value), 2)), "%", sep = "")

    tmp <- rbind(Value = as.character(Value), Percentage = as.character(Percentage))
    dimnames(tmp)[[2]] <- Dimension
    Eigenvalues <- tmp

    # Row Profiles:
    tmp <- rbind(obj$rowmass, obj$rowdist, obj$rowinertia, t(obj$rowcoord[,1:nd]))
    tmpnames <- obj$rownames
    if (!is.na(obj$rowsup[1])) {
        tmpnames[obj$rowsup] <- paste(tmpnames[obj$rowsup],"(*)")
    }
    dimnames(tmp)[[2]] <- tmpnames
    dn <- paste("Dim.", 1:nd)
    dimnames(tmp)[[1]] <- c("Mass", "ChiDist", "Inertia", dn)
    Row.profiles <- tmp

    # Column Profiles:
    tmp <- rbind(obj$colmass, obj$coldist, obj$colinertia, t(obj$colcoord[,1:nd]))
    tmpnames <- obj$colnames
    if (!is.na(obj$colsup[1])) {
        tmpnames[obj$colsup] <- paste(tmpnames[obj$colsup],"(*)")
    }
    dimnames(tmp)[[2]] <- tmpnames
    dn <- paste("Dim.", 1:nd)
    dimnames(tmp)[[1]] <- c("Mass", "ChiDist", "Inertia", dn)
    Column.profiles <- tmp

    HTML("Principal inertias (eigenvalues):")
    HTML(Eigenvalues, ...)
    HTML("Documents and variables:")
    HTML(Row.profiles, ...)
    HTML("Terms:")
    HTML(Column.profiles, ...)
}

