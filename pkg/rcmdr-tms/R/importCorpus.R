importCorpusDlg <- function() {
    # Let the user select processing options
    initializeDialog(title=gettext_("Import Corpus"))

    radioButtons(name="source",
                 buttons=c("dir", "file", "factiva"),
                 labels=c(gettext_("Directory containing plain text files"),
                          gettext_("Spreadsheet file (CSV, XLS, ODS...)"),
                          gettext_("Factiva XML file")),
                 title=gettext_("Load corpus from:"),
                 right=FALSE)

    # TRANSLATORS: replace 'en' with your language's ISO 639 two-letter code
    tclLang <- tclVar(gettext_("en"))
    entryLang <- ttkentry(top, width="12", textvariable=tclLang)
    checkBoxes(frame="processingFrame",
               boxes=c("lowercase", "punctuation", "numbers", "stopwords", "stemming"),
               initialValues=rep(1, 5),
               labels=c(gettext_("Ignore case"), gettext_("Remove punctuation"),
                        gettext_("Remove numbers"), gettext_("Remove stopwords"),
                        gettext_("Stem words")),
               title=gettext_("Text processing:"))

    onOK <- function() {
        closeDialog()

        # Remove objects left from a previous analysis to avoid confusion
        # (we assume later existing objects match the current corpus)
        objects <- c("corpus", "dtm", "lengthsDtm", "voc", "lengths", "absTermFreq", "varTermFreq",
                     "corpusClust", "corpusSubClust", "corpusCa", "plottingCa")
        doItAndPrint(paste('rm(list=c("', paste(objects[sapply(objects, exists)], collapse='", "'), '"))', sep=""))
        gc()

        # Set language
        lang <- tclvalue(tclLang)

        # Import corpus
        source <- tclvalue(sourceVariable)
        success <- switch(source,
                          dir=importCorpusFromDir(lang),
                          file=importCorpusFromFile(lang),
                          factiva=importCorpusFromFactiva(lang))

        # If loading failed, do not add errors to errors
        if(!success || length(corpus) == 0)
            return()

        # Language is used again when creating the dtm to analyse word lengths
        doItAndPrint(sprintf('meta(corpus, type="corpus", tag="language") <- "%s"', lang))

        # Process texts
        lowercase <- tclvalue(lowercaseVariable) == 1
        punctuation <- tclvalue(punctuationVariable) == 1
        numbers <- tclvalue(numbersVariable) == 1
        stopwords <- tclvalue(stopwordsVariable) == 1
        stemming <- tclvalue(stemmingVariable) == 1

        if(lowercase || punctuation || numbers || stopwords || stemming)
            doItAndPrint("dtmCorpus <- corpus")

        if(lowercase)
            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, tolower)")
        if(punctuation) {
            # Workaround to avoid French articles from getting concatenated with their noun
            if(lang == "fr")
                doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, function(x) gsub(\"[\'\U2019]\", \" \", x))")

            doItAndPrint("dtmCorpus <- tm_map(dtmCorpus, removePunctuation)")
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

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="importCorpusDlg")
    tkgrid(sourceFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(labelRcmdr(top, text=gettext_("Language of texts in the corpus:")), entryLang, sticky="w")
    tkgrid(processingFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=4, columns=2, focus=entryLang)
}

# Choose a directory to load texts from
importCorpusFromDir <- function(language=NA) {
    dir <- tclvalue(tkchooseDirectory(initialdir=getwd(),
                                      parent=CommanderWindow()))
    if (dir == "") return()

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    doItAndPrint(sprintf("corpus <- Corpus(DirSource(\"%s\"), readerControl=list(language=%s))", dir, language))

    assign("corpusVars", data.frame(var1=rep(NA, length(corpus)), row.names=names(corpus)), envir=.GlobalEnv)
    activeDataSet("corpusVars")

    return(TRUE)
}

# Choose a CSV file to load texts and variables from
importCorpusFromFile <- function(language=NA) {
    file <- tclvalue(tkgetOpenFile(filetypes=sprintf("{{%s} {.csv .CSV}} {{%s} {.tsv .TSV}} {{%s} {.dbf .DBF}} {{%s} {.ods .ODS}} {{%s} {.xls .XLS}} {{%s} {.xlsx .XLSX}} {{%s} {.mdb .MDB}} {{%s} {.accdb .ACCDB}} {{%s} {.csv .CSV .tsv .TSV .dbf .DBF .ods .ODS .xls .XLS .xlsx .XLSX .mdb .MDB .accdb .ACCDB}}",
                                                     gettext_("CSV file"),
                                                     gettext_("TSV file"),
                                                     gettext_("dBase file"),
                                                     gettext_("ODS file"),
                                                     gettext_("Excel file"),
                                                     gettext_("Excel 2007 file"),
                                                     gettext_("Access database"),
                                                     gettext_("Access 2007 database"),
                                                     gettext_("All supported types")),
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
	    Message(gettext_("Loading OpenDocument spreadsheets (.ods) is not supported on your system.\nYou should save your data set as a CSV file or as an Excel spreadsheet (.xls)."),
                    type="error")
            return(FALSE)
        }
	else if(!require(ROpenOffice)) {
            response <- tkmessageBox(message=gettext_("Loading OpenDocument spreadsheets (.ods) requires the ROpenOffice package.\nDo you want to install it?"),
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
	    Message(gettext_("Loading Excel and Access files is only supported on Windows.\nYou should save your data set as a CSV file or as an OpenDocument spreadsheet (.ods)."),
                    type="error")
            return(FALSE)
        }
	else if(!require(RODBC)) {
            response <- tkmessageBox(message=gettext_("The RODBC package is needed to read Excel and Access files.\nDo you want to install it?"),
                                     icon="question", type="yesno")

            if (tclvalue(response) == "yes")
	        install.packages("RODBC")
            else
                return(FALSE)
        }
        else if(!any(grepl(ext, odbcDataSources()))) {
	    Message(gettext_("No ODBC driver for this file type was found.\nYou probably need to install Excel or Access, or separate ODBC drivers."),
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
            fil <- tk_select.list(sort(tabdat), title=gettext_Rcmdr("Select one table"))
        else
            fil <- tabdat

        if(fil == "") {
            Message(gettext_Rcmdr("No table selected"), type="error")
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
        assign("corpusVars", data.frame(var1=rep(NA, length(corpus)), row.names=names(corpus)), envir=.GlobalEnv)
        activeDataSet("corpusVars")
    }

    return(TRUE)
}

# Choose a Factiva XML file to load texts and variables from
importCorpusFromFactiva <- function(language=NA) {
    if(!require(tm.plugin.factiva)) {
            response <- tkmessageBox(message=gettext_("The tm.plugin.factiva package is needed to import corpora from Factiva XML files.\nDo you want to install it?"),
                                     icon="question", type="yesno")

            if (tclvalue(response) == "yes")
	        install.packages("tm.plugin.factiva")
            else
                return(FALSE)
    }

    filestr <- tclvalue(tkgetOpenFile(filetypes=sprintf("{{%s} {.xml .XML}}",
                                                        gettext_("Factiva XML file")),
                                      multiple=TRUE,
                                      parent=CommanderWindow()))

    if (filestr == "") return(FALSE)

    files <- gsub("\\{|\\}", "", strsplit(filestr, "\\} \\{")[[1]])

    if(!is.na(language))
        language <- paste("\"", language, "\"", sep="")

    doItAndPrint(sprintf("corpus <- Corpus(FactivaSource(\"%s\"), readerControl=list(language=%s))",
                         files[1], language))
    lapply(files[-1], function(file) doItAndPrint(sprintf(
        "corpus <- c(corpus, Corpus(FactivaSource(\"%s\"), readerControl=list(language=%s)), recursive=TRUE)",
                                                          file, language)))

    # Set document names from the IDs if missing (XMLSource limitation)
    if(length(names(corpus)) < length(corpus))
        doItAndPrint("names(corpus) <- sapply(corpus, ID)")

    # Extract local per-document meta-data
    dates <- lapply(corpus, meta, "DateTimeStamp")
    dates <- sapply(dates, function(x) if(length(x) > 0) as.character(x) else NA)
    vars <- data.frame(Date=dates)

    tags <- c("Origin", "Author", "Heading", "Language", "WordCount", "Pages",
              "Section", "Edition", "Publisher")
    for(tag in tags) {
        var <- lapply(corpus, meta, tag)
        var <- lapply(var, function(x) if(length(x) > 0) x else NA)
        vars <- cbind(vars, unlist(var))
    }

    colnames(vars) <- c(gettext_("Date"), gettext_("Origin"), gettext_("Author"), gettext_("Heading"),
                        gettext_("Language"), gettext_("WordCount"), gettext_("Pages"),
                        gettext_("Section"), gettext_("Edition"), gettext_("Publisher"))
    rownames(vars) <- names(corpus)

    assign("corpusVars", vars, envir=.GlobalEnv)
    activeDataSet("corpusVars")
    doItAndPrint("setCorpusVariables()")

    return(TRUE)
}

