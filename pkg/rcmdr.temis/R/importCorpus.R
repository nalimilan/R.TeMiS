importCorpusDlg <- function() {
    # Let the user select processing options
    initializeDialog(title=.gettext("Import Corpus"))

    radioButtons(name="source",
                 buttons=c("dir", "file", "factiva", "twitter"),
                 labels=c(.gettext("Directory containing plain text files"),
                          .gettext("Spreadsheet file (CSV, XLS, ODS...)"),
                          .gettext("Factiva XML or HTML file(s)"),
                          .gettext("Twitter search")),
                 title=.gettext("Load corpus from:"),
                 right.buttons=FALSE)

    # TRANSLATORS: replace 'en' with your language's ISO 639 two-letter code
    tclLang <- tclVar(.gettext("en"))
    entryLang <- ttkentry(top, width="12", textvariable=tclLang)
    checkBoxes(frame="processingFrame",
               boxes=c("lowercase", "punctuation", "numbers", "stopwords", "stemming"),
               initialValues=rep(1, 5),
               labels=c(.gettext("Ignore case"), .gettext("Remove punctuation"),
                        .gettext("Remove numbers"), .gettext("Remove stopwords"),
                        .gettext("Stem words")),
               title=.gettext("Text processing:"))

    chunksFrame <- tkframe(top)
    tclChunks <- tclVar(0)
    tclNParagraphs <- tclVar(1)
    chunksButton <- tkcheckbutton(chunksFrame, variable=tclChunks,
                                  text=.gettext("Split texts into smaller documents"))
    chunksSlider <- tkscale(chunksFrame, from=1, to=20, showvalue=TRUE, variable=tclNParagraphs,
		            resolution=1, orient="horizontal")
    

    onOK <- function() {
        closeDialog()

        # Remove objects left from a previous analysis to avoid confusion
        # (we assume later existing objects match the current corpus)
        objects <- c("corpus", "corpusVars", "dtm", "wordsDtm", "lengthsDtm", "voc", "lengths",
                     "termFreqs", "absTermFreqs", "varTermFreqs", "freqTerms", "typicalTerms", "docSeries",
                     ".last.table", ".HTML.file", "corpusClust", "corpusSubClust", "corpusCa", "plottingCa")
        if(any(sapply(objects, exists))) {
            doItAndPrint(paste("rm(", paste(objects[sapply(objects, exists)], collapse=", "), ")", sep=""))
            gc()
        }


        # Set language
        lang <- tclvalue(tclLang)

        # Import corpus
        source <- tclvalue(sourceVariable)
        success <- switch(source,
                          dir=importCorpusFromDir(lang),
                          file=importCorpusFromFile(lang),
                          factiva=importCorpusFromFactiva(lang),
                          twitter=importCorpusFromTwitter(lang))

        # If loading failed, do not add errors to errors
        if(!success || length(corpus) == 0)
            return()

        # Language is used again when creating the dtm to analyse word lengths
        doItAndPrint(sprintf('meta(corpus, type="corpus", tag="language") <- "%s"', lang))

        # Create chunks
        if(tclvalue(tclChunks) == 1) {
            doItAndPrint(sprintf("corpus <- splitTexts(corpus, %s)", tclvalue(tclNParagraphs)))
            doItAndPrint('meta(corpus, type="corpus", tag="split") <- TRUE')
        }

        # Process texts
        twitter <- source == "twitter"
        lowercase <- tclvalue(lowercaseVariable) == 1
        punctuation <- tclvalue(punctuationVariable) == 1
        numbers <- tclvalue(numbersVariable) == 1
        stopwords <- tclvalue(stopwordsVariable) == 1
        stemming <- tclvalue(stemmingVariable) == 1

        if(twitter || lowercase || punctuation || numbers || stopwords || stemming)
            doItAndPrint("dtmCorpus <- corpus")

        if(twitter)
            doItAndPrint('dtmCorpus <- tm_map(dtmCorpus, function(x) gsub("http://[[:alnum:]/\\\\.\\\\-\\\\?=&#_;,]*|[@#][[:alnum:]]+|RT:", "", x))')

        if(lowercase)
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, tolower)")
        if(punctuation) {
            # The default tokenizer does not get rid of punctuation *and of line breaks!*, which
            # get concatenated with surrounding words
            # This also avoids French articles and dash-linked words from getting concatenated with their noun
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"([\'\U2019\\n]|[[:punct:]])+\", \" \", x))")
        }
        if(numbers)
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removeNumbers)")
        if(stopwords)
            doItAndPrint(paste("dtmCorpus <- tm_map(dtmCorpus, removeWords, stopwords(\"",
                               lang, "\"))", sep=""))
        if(stemming)
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, stemDocument)")

        if(lowercase || punctuation || numbers || stopwords || stemming) {
            doItAndPrint("dtm <- DocumentTermMatrix(dtmCorpus)")
            doItAndPrint("rm(dtmCorpus)")
        }
        else {
            doItAndPrint("dtm <- DocumentTermMatrix(corpus)")
        }

        doItAndPrint("corpus")
        doItAndPrint("dtm")

        activateMenus()

        .setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="importCorpusDlg")
    tkgrid(sourceFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Language of texts in the corpus:")), entryLang, sticky="w")
    tkgrid(labelRcmdr(chunksFrame, text=.gettext("Text splitting:"), fg="blue"), sticky="ws")
    tkgrid(chunksButton, columnspan="2", sticky="w", pady=6)
    tkgrid(labelRcmdr(chunksFrame, text=.gettext("Size of new documents (in paragraphs):")),
           chunksSlider, sticky="w", pady=6, padx=6)
    tkgrid(chunksFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(processingFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=6, columns=2, focus=entryLang)
}

# Choose a directory to load texts from
importCorpusFromDir <- function(language=NA) {
    dir <- tclvalue(tkchooseDirectory(initialdir=getwd(),
                                      parent=CommanderWindow()))
    if (dir == "") return()

    .setBusyCursor()

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    doItAndPrint(sprintf('corpus <- Corpus(DirSource("%s", encoding=""), readerControl=list(language=%s))', dir, language))

    # Because of a bug in Rcmdr, filling the first column with NAs prevents entering data in this columns:
    # use "" instead
    assign("corpusVars", data.frame(var1=rep("", length(corpus)), row.names=names(corpus)), envir=.GlobalEnv)
    activeDataSet("corpusVars")

    return(TRUE)
}

# Choose a CSV file to load texts and variables from
importCorpusFromFile <- function(language=NA) {
    file <- tclvalue(tkgetOpenFile(filetypes=sprintf("{{%s} {.csv .CSV}} {{%s} {.tsv .TSV}} {{%s} {.dbf .DBF}} {{%s} {.ods .ODS}} {{%s} {.xls .XLS}} {{%s} {.xlsx .XLSX}} {{%s} {.mdb .MDB}} {{%s} {.accdb .ACCDB}} {{%s} {.csv .CSV .tsv .TSV .dbf .DBF .ods .ODS .xls .XLS .xlsx .XLSX .mdb .MDB .accdb .ACCDB}}",
                                                     .gettext("CSV file"),
                                                     .gettext("TSV file"),
                                                     .gettext("dBase file"),
                                                     .gettext("ODS file"),
                                                     .gettext("Excel file"),
                                                     .gettext("Excel 2007 file"),
                                                     .gettext("Access database"),
                                                     .gettext("Access 2007 database"),
                                                     .gettext("All supported types")),
                                   parent=CommanderWindow()))

    if (file == "") return(FALSE)

    # Code adapted from Rcommander's data-menu.R file
    # The following function was contributed by Matthieu Lesnoff
    # (added with small changes by J. Fox, 20 July 06 & 30 July 08)
    # Licensed under GNU GPL (version ≥ 2) 
    sop <- match(".", rev(strsplit(file, NULL)[[1]]))[1]
    ext <- tolower(substring(file, nchar(file) - sop + 2, nchar(file)))

    if(ext == "csv") {
        doItAndPrint(paste("corpusDataset <- read.csv(\"", file, "\")", sep=""))
    }
    else if(ext == "tsv") {
        doItAndPrint(paste("corpusDataset <- read.tsv(\"", file, "\")", sep=""))
    }
    else if(ext == "dbf") {
        Library(foreign)
        doItAndPrint(paste("corpusDataset <- read.dbf(\"", file, "\")", sep=""))
    }
    else if(ext == "ods") {
        # ROpenOffice is not available as binary, thus most likely to fail on Windows and Mac OS
        if(!"ROpenOffice" %in% rownames(available.packages(contrib.url("http://www.omegahat.org/R/")))) {
	    Message(.gettext("Loading OpenDocument spreadsheets (.ods) is not supported on your system.\nYou should save your data set as a CSV file or as an Excel spreadsheet (.xls)."),
                    type="error")
            return(FALSE)
        }
	else if(!require(ROpenOffice)) {
            response <- tkmessageBox(message=.gettext("Loading OpenDocument spreadsheets (.ods) requires the ROpenOffice package.\nDo you want to install it?"),
                                     icon="question", type="yesno")

            if (tclvalue(response) == "yes")
	        install.packages("ROpenOffice", repos="http://www.omegahat.org/R", type="source")
            else
                return(FALSE)
        }

        doItAndPrint(paste("corpusDataset <- read.ods(\"", file, "\")", sep=""))
    }
    else {
        if(.Platform$OS.type != "windows") {
	    Message(.gettext("Loading Excel and Access files is only supported on Windows.\nYou should save your data set as a CSV file or as an OpenDocument spreadsheet (.ods)."),
                    type="error")
            return(FALSE)
        }
	else if(!.checkAndInstall("RODBC", .gettext("The RODBC package is needed to read Excel and Access files.\nDo you want to install it?"))) {
            return(FALSE)
        }
        else if(!any(grepl(ext, odbcDataSources()))) {
	    Message(.gettext("No ODBC driver for this file type was found.\nYou probably need to install Excel or Access, or separate ODBC drivers."),
                    type="error")
            return(FALSE)
        }

        channelStr <- switch(EXPR = ext,
        	             xls = "odbcConnectExcel",
        	             xlsx = "odbcConnectExcel2007",
        	             mdb = "odbcConnectAccess",
        	             accdb = "odbcConnectAccess2007")
        doItAndPrint(paste("channel <- ", channelStr, "(\"", file, "\")", sep=""))

        # For Excel and Access, need to select a particular sheet or table
        tabdat <- sqlTables(channel)
        names(tabdat) <- tolower(names(tabdat))

        if(ext == "mdb" || ext == "accdb")
            tabdat <- tabdat[tabdat$table_type == "TABLE", 3]

        if(ext == "xls" || ext == "xlsx") {
            tabname <- tabdat$table_name
            tabdat <- ifelse(tabdat$table_type == "TABLE",
                             substring(tabname, 2, nchar(tabname) - 2),
                             substring(tabname, 1, nchar(tabname) - 1))
        }

        # If there are several tables
        if(length(tabdat) > 1)
            fil <- tk_select.list(sort(tabdat), title=.gettextRcmdr("Select one table"))
        else
            fil <- tabdat

        if(fil == "") {
            Message(.gettextRcmdr("No table selected"), type="error")
            return(FALSE)
        }

        if(ext == "xls" || ext == "xlsx")
            fil <- paste("[", fil, "$]", sep = "")

        # Retrieve the data
        command <- paste("sqlQuery(channel=channel, select * from ", fil,")",
        	         sep = "")
        doItAndPrint(paste("corpusDataset <- ", command, sep = ""))
        doItAndPrint("odbcCloseAll()")
    }

    # In case something went wrong, no point in continuing
    if(is.null(corpusDataset))
        return(FALSE)

    .setBusyCursor()

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    doItAndPrint(sprintf("corpus <- Corpus(DataframeSource(corpusDataset[1]), readerControl=list(language=%s))",
                         language))

    if(ncol(corpusDataset) > 1) {
        doItAndPrint("corpusVars <- corpusDataset[-1]")
        doItAndPrint("activeDataSet(\"corpusVars\")")
        doItAndPrint("setCorpusVariables()")
    }
    else {
        assign("corpusVars", data.frame(var1=factor(rep(NA, length(corpus))), row.names=names(corpus)), envir=.GlobalEnv)
        activeDataSet("corpusVars")
    }

    return(TRUE)
}

# Choose a Factiva XML or HTML file to load texts and variables from
importCorpusFromFactiva <- function(language=NA) {
    if(!.checkAndInstall("tm.plugin.factiva",
                         .gettext("The tm.plugin.factiva package is needed to import corpora from Factiva files.\nDo you want to install it?")))
        return(FALSE)

    filestr <- tclvalue(tkgetOpenFile(filetypes=sprintf("{{%s} {.xml .htm .html .aspx .XML .HTM .HTML .ASPX}}",
                                                        .gettext("Factiva XML and HTML files")),
                                      multiple=TRUE,
                                      parent=CommanderWindow()))

    if (filestr == "") return(FALSE)

    .setBusyCursor()

    # tkgetOpenFile() is terrible: if path contains a space, file paths are surrounded by {}
    # If no spaces are present, they are not, but in both cases the separator is a space
    if(substr(filestr, 0, 1) == "{")
        files <- gsub("\\{|\\}", "", strsplit(filestr, "\\} \\{")[[1]])
    else
        files <- strsplit(filestr, " ")[[1]]

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    doItAndPrint(sprintf("corpus <- Corpus(FactivaSource(\"%s\"), readerControl=list(language=%s))",
                         files[1], language))
    lapply(files[-1], function(file) doItAndPrint(sprintf(
        "corpus <- c(corpus, Corpus(FactivaSource(\"%s\"), readerControl=list(language=%s)), recursive=TRUE)",
                                                          file, language)))

    if(!exists("corpus") || length(corpus) == 0) {
        Message(.gettext("Reading the specified file failed. Are you sure this file is in the correct format?"),
                type="error")

        .setIdleCursor()

        return(FALSE)
    }

    # Set document names from the IDs since it's not always done by sources (XMLSource...)
    # We rely on this later e.g. in showCorpusCa() because we cannot use indexes when documents are skipped
    # In rare cases, duplicated IDs can happen since Factiva plugin truncates them: ensure they are unique
    doItAndPrint("names(corpus) <- make.unique(sapply(corpus, ID))")

    # Extract local per-document meta-data
    dates <- lapply(corpus, meta, "DateTimeStamp")
    dates <- sapply(dates, function(x) if(length(x) > 0) as.character(x) else NA)
    vars <- data.frame(Origin=NA, Date=dates, Author=NA, Section=NA)

    tags <- c("Origin", "Author", "Section")
    for(tag in tags) {
        var <- lapply(corpus, meta, tag)
        var <- lapply(var, function(x) if(length(x) > 0) x else NA)
        vars[[tag]] <- unlist(var)
    }

    colnames(vars) <- c(.gettext("Origin"), .gettext("Date"), .gettext("Author"), .gettext("Section"))

    tags <- c("Subject", "Coverage")
    for(tag in tags) {
        var <- lapply(corpus, meta, tag)
        levs <- unique(unlist(var))
        levs <- levs[!is.na(levs)]

        if(length(levs) == 0)
            next

        for(lev in levs)
            vars[[make.names(lev)]] <- sapply(var, function(x) lev %in% x)
    }

    rownames(vars) <- names(corpus)

    assign("corpusVars", vars, envir=.GlobalEnv)
    activeDataSet("corpusVars")
    doItAndPrint("setCorpusVariables()")

    return(TRUE)
}

# Choose a Twitter hashtag to search for messages
importCorpusFromTwitter <- function(language=NA) {
    if(!.checkAndInstall("twitteR",
                         .gettext("The twitteR package is needed to import corpora from Twitter.\nDo you want to install it?")))
        return(FALSE)

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    initializeDialog(title=.gettext("Import Corpus From Twitter"))

    # TRANSLATORS: replace 'en' with your language's ISO 639 two-letter code
    tclText <- tclVar("")
    entryText <- ttkentry(top, width="12", textvariable=tclText)

    tclNMess <- tclVar(100)
    tclNSlider <- tkscale(top, from=1, to=1500,
                          showvalue=TRUE, variable=tclNMess,
                          resolution=1, orient="horizontal")

    result <- tclVar()

    onOK <- function() {
        text <- tclvalue(tclText)
        nmess <- tclvalue(tclNMess)

        if(text == "") {
            Message(gettext.("Please enter valid text to search for."), type="error")
            return()
        }

        closeDialog()
        .setBusyCursor()
        on.exit(.setIdleCursor())

        # In case something goes wrong
        tclvalue(result) <- "error"

        doItAndPrint("library(twitteR)")
        doItAndPrint(sprintf('messages <- searchTwitter("%s", %s, %s)', text, nmess, language))

        if(length(messages) == 0) {
            Message(sprintf(.gettext("No recent tweets match the specified search criteria in the chosen language (%s)."), language),
                    type="error")
            return()
        }

        doItAndPrint("corpusDataset <- twListToDF(messages)")
        doItAndPrint("rownames(corpusDataset) <- make.unique(paste(corpusDataset$screenName, corpusDataset$created))")
        doItAndPrint(sprintf('corpus <- Corpus(DataframeSource(corpusDataset[1]), readerControl=list(language=%s))',
                             language))
        doItAndPrint("rm(messages)")

        if(!exists("corpus") || length(corpus) == 0) {
            Message(.gettext("Retrieving messages from Twitter failed."),
                    type="error")
            return()
        }

        doItAndPrint('corpusVars <- corpusDataset[c("screenName", "created", "truncated", "statusSource")]')
        doItAndPrint("rm(corpusDataset)")
        doItAndPrint(sprintf('colnames(corpusVars) <- c("%s", "%s", "%s", "%s")',
                             .gettext("Author"), .gettext("Time"), .gettext("Truncated"), .gettext("StatusSource")))
        doItAndPrint("activeDataSet(\"corpusVars\")")
        doItAndPrint("setCorpusVariables()")

        tclvalue(result) <- "success"

        return()
    }

    onCancel <- function() {
        if (GrabFocus()) tkgrab.release(messageBox)
        tkdestroy(top)
        tkfocus(CommanderWindow())
        tclvalue(result) <- "cancel"
    }

    OKCancelHelp(helpSubject="importCorpusDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Text to search for:")),
           entryText, sticky="w", pady=6)
    tkgrid(labelRcmdr(top, text=.gettext("Maximum number of tweets to download:")),
           tclNSlider, sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan=2, sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2, focus=entryText)

    return(tclvalue(result) == "success")
}

# Adapted version of tm's makeChunks() remembering which chunk comes from which document,
# preserving corpus meta-data, and skipping empty chunks.
# Copyright Ingo Feinerer, Licence: GPL (≥ 2).
# http://tm.r-forge.r-project.org/
splitTexts <- function (corpus, chunksize, preserveMetadata=TRUE) 
{
    chunks <- list(length(corpus))
    origins <- list(length(corpus))

    for (k in seq_along(corpus)) {
        chunks_k <- tapply(corpus[[k]], rep(seq(1, length(corpus[[k]])),
                                            each=chunksize, length.out=length(corpus[[k]])), c)

        # Skeep empty chunks
        keep <- nchar(gsub("[\n[:space:][:punct:]]+", "", sapply(chunks_k, paste, collapse=""))) > 0

        chunks[[k]] <- chunks_k[keep]
        origins[[k]] <- rep(k, sum(keep))
    }

    # Merge only the per-document lists of chunks at the end to reduce the number of copies
    chunks <- do.call(c, chunks)
    origins <- do.call(c, origins)

    newCorpus <- Corpus(VectorSource(chunks))

    names1 <- names(corpus)
    names2 <- make.unique(names1[origins])

    # Copy meta data from old documents
    if(preserveMetadata) {
        DMetaData(newCorpus) <- DMetaData(corpus)[origins,, drop=FALSE]
        docs <- list(length(newCorpus))

        for(i in seq_along(corpus)) {
            attrs <- attributes(corpus[[i]])

            for(j in which(origins == i)) {
                doc <- newCorpus[[j]]
                attr(doc, "ID") <- names2[j]
                attr(doc, "Document") <- names1[i]
                attr(doc, "Author") <- attrs$Author
                attr(doc, "DateTimeStamp") <- attrs$DateTimeStamp
                attr(doc, "Description") <- attrs$Description
                attr(doc, "Heading") <- attrs$Heading
                attr(doc, "Language") <- attrs$Language
                attr(doc, "LocalMetaData") <- attrs$LocalMetaData
                attr(doc, "Origin") <- attrs$Origin
                docs[[j]] <- doc
            }
        }

        # [[<-.VCorpus is terribly slow: it is incredibly faster to work on a list of documents,
        # and only assign them in one shot at the end
        newCorpus[] <- docs
    }

    meta(newCorpus, .gettext("Doc ID")) <- names1[origins]
    meta(newCorpus, .gettext("Doc N")) <- origins
    names(newCorpus) <- names2

    newCorpus
}
