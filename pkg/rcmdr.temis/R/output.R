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

    doItAndPrint("browseURL(.HTML.file)")

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

    HTML(sprintf(.gettext("Corpus imported on %s. Language: %s."),
                 # c() is needed to get rid of the timezone attribute, set to GMT by tm
                 format(c(meta(corpus, type="corpus", tag="create_date")), "%c"),
                 meta(corpus, type="corpus", tag="language")))
    HTML(sprintf(.gettext("Source: %s."), meta(corpus, type="corpus", tag="source")))
    HTML(sprintf(.gettext("%i documents and %i terms."), nrow(dtm), ncol(dtm)))

    cat(.gettext("Processing options:\n"), file=.HTML.file, append=TRUE)
    processing <- meta(corpus, type="corpus", tag="processing")
    # Keep in sync with strings in importCorpusDlg()
    HTMLli(paste(c(.gettext("Ignore case"), .gettext("Remove punctuation"),
                   .gettext("Remove digits"), .gettext("Remove stopwords"),
                   .gettext("Apply stemming")),
                 .gettext(": "),
                 ifelse(processing[c("lowercase", "punctuation", "digits", "stopwords", "stemming")],
                        .gettext("enabled"), .gettext("disabled")),
                 ".", sep=""))
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

    # Needed when copying CA, HTML.ca() is too late to update the GUI
    .setBusyCursor()
    on.exit(.setIdleCursor())

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
    filename <- gsub(".html$", "", basename(.HTML.file))

    file <- paste(filename, format(Sys.time(), .gettext(" - plot %Y-%m-%d %H-%M")), ".png", sep="")

    i <- 1
    testfile <- file
    while(file.exists(testfile)) {
        i <- i + 1
        testfile <- paste(filename, format(Sys.time(), .gettext(" - plot %Y-%m-%d %H-%M")),
                          "-", i, ".png", sep="")
    }

    if(file.exists(file))
        file <- testfile

    doItAndPrint(sprintf('dev.print(png, width=7, height=7, unit="in", res=200, filename="%s")',
                         paste(dirname(.HTML.file), .Platform$file.sep, file, sep="")))
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

# This function uses parts from summary.ca() from package ca, version 0.53.
# Released under the GPL (no version specified), Copyright Michael Greenacre
# and Oleg Nenadic <onenadi at uni-goettingen.de>.
# http://cran.r-project.org/web/packages/ca/index.html
HTML.ca <- function(x, ...) {
  object <- summary.ca(x)

  if (!is.na(object$scree)[1]){
    cat("\n")
   # init:
    nchars <- 25
    Dim    <- object$scree[,1]
    ev     <- object$scree[,2]
    rev    <- object$scree[,3]
    crev   <- object$scree[,4]
    Value  <- ev[Dim]
    EV     <- rev[Dim]
    CUMEV  <- crev[Dim]
    if (length(rev)>1) {
      st <- round(nchars * (rev - min(rev)) / diff(range(rev)), 0)
      } else {
      st <- nchars
      }

    scree <- character(length(Dim))
    for (q in Dim) {
      s1 <- paste(rep("*", st[q]), collapse = "")
      s2 <- paste(rep(" ", nchars - st[q]), collapse = "")
      scree[q] <- paste(" ", s1, s2, sep = "")
      }
    temp0 <- c(" "," ------"," "," "," ")
    temp1 <- c("Total:", sum(EV), "", "", "")
   # some processing:
    Value0 <- round(Value, 6)
    Value1 <- round(sum(Value), 6)
    EV1    <- round(EV, 1)
    EV2    <- round(sum(EV), 1)
    gluezero <- function(item, dig = 8, point = NA){
      item0 <- paste(item, paste(rep(0, dig), collapse=""), sep = "")
      item1 <- strsplit(item0,"", fixed = TRUE)
      pastebit <- function(x, digits = dig, poin = point){
        if(!is.na(poin)) { 
          x[poin] <- "."
          }
        paste(x[1:dig], collapse = "")
        }
      unlist(lapply(item1, pastebit))
      }
    remzero <- function(x, doub = FALSE){
      x0 <- strsplit(x, "", fixed = TRUE)
      pastebit2 <- function(x, doubl = doub){
        if (doubl){
           if (x[1]==0 & x[2]==0){
             x[1] <- " "
             x[2] <- " "
             }
          }
        if (x[1]==0) x[1] <- " "
        paste(x, collapse = "")
        }
      unlist(lapply(x0, pastebit2))
      }
    EV.1 <- floor(log(EV1, base = 10))
    EV.1[EV.1 < 0] <- 0
    EV.2 <- as.character(EV.1)
    EV.2[EV.1 == 1] <- ""
    EV.2[EV.1 == 0] <- "0"
    EV1 <- remzero(gluezero(paste(EV.2, EV1, sep = ""), 4, 3))
    EV.sp <- paste(rep(" ", ifelse(max(EV.1==2), 0, 1)), collapse = "", 
                   sep = "")
    EV1    <- paste(EV.sp, EV1, sep = "")
    CUMEV1 <- round(CUMEV, 1)
    CUMEV.1 <- floor(log(CUMEV1, base = 10))
    CUMEV.1[CUMEV.1 < 0] <- 0
    CUMEV.2 <- as.character(CUMEV.1)
    CUMEV.2[CUMEV.1 == 2] <- ""
    CUMEV.2[CUMEV.1 == 1] <- "0"
    CUMEV.2[CUMEV.1 == 0] <- "00"
    CUMEV1 <- remzero(gluezero(paste(CUMEV.2, CUMEV1, sep = ""), 5, 4), 
                      doub = TRUE)


    scree.out <- data.frame(Dim   = c(Dim, "", "Total:"), 
                            Value = c(gluezero(Value0), "--------", 
                                      gluezero(Value1)), 
                            EV    = c(EV1, "-----", gluezero(EV2, 5, 4)), 
                            CUMEV = c(CUMEV1, "", ""), 
                            scree = c(scree, "", ""))

    colnames(scree.out) <- c(.gettext("Dimension"), .gettext("Value"), .gettext("%"),
                             .gettext("Cum. %"), "")
    HTML(.gettext("Principal inertias (eigenvalues):\n\n"))
    scree.out <- as.matrix(scree.out)
   # colnames(scree.out) <- rep(1, dim(scree.out)[1])
   # print(as.matrix(scree.out), quote = FALSE)
   # fix for rownames showing up in scree-plot
   # dimnames(scree.out)[[1]] <- rep("", length(dimnames(scree.out)[[1]]))
    rownames(scree.out) <- rep("", nrow(scree.out))
    HTML(scree.out, ...)
  }

  rownames(object$row) <- object$row[[1]]
  rownames(object$col) <- object$col[[1]]

  object$row <- object$row[-1]
  object$col <- object$col[-1]

  names(object$row) <- names(object$col) <- c(.gettext("Mass"), .gettext("Quality"), .gettext("Inertia"),
                                              outer(c(.gettext("Coord"), .gettext("Quality"), .gettext("Contr")),
                                                    seq(x$nd), paste, sep=""))

  HTML(.gettext("Documents and variables:"))
  HTML(object$row,  ...)

  HTML(.gettext("Terms:"))
  HTML(object$col, ...)
}

