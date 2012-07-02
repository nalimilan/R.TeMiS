# Choose a directory to load texts from
importCorpusFromDir <- function() {
    dir <- tclvalue(tkchooseDirectory(initialdir=getwd(),
                                      parent=CommanderWindow()))
    if (dir == "") return()

    doItAndPrint(paste("corpus <- Corpus(DirSource(\"", dir, "\"))", sep=""))

    assign("corpusVars", data.frame(var1=rep(NA, length(corpus)), row.names=names(corpus)), envir=.GlobalEnv)
    activeDataSet("corpusVars")

    processCorpusDlg()
}

# Choose a CSV file to load texts and variables from
importCorpusFromFile <- function() {
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

    if (file == "") return()

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
            return()
        }
	else if(!require(ROpenOffice)) {
            response <- tkmessageBox(message=gettext_("Loading OpenDocument spreadsheets (.ods) requires the ROpenOffice package.\nDo you want to install it?"),
                                     icon="question", type="yesno")

            if (tclvalue(response) == "yes")
	        install.packages("ROpenOffice", repos="http://www.omegahat.org/R", type="source")
            else
                return()
        }

        doItAndPrint(paste("corpusDataset <- read.ods(\"", file, "\")", sep=""))
    }
    else {
        if(.Platform$OS.type != "windows") {
	    Message(gettext_("Loading Excel and Access files is only supported on Windows.\nYou should save your data set as a CSV file or as an OpenDocument spreadsheet (.ods)."),
                    type="error")
            return()
        }
	else if(!require(RODBC)) {
            response <- tkmessageBox(message=gettext_("The RODBC package is needed to read Excel and Access files.\nDo you want to install it?"),
                                     icon="question", type="yesno")

            if (tclvalue(response) == "yes")
	        install.packages("RODBC")
            else
                return()
        }
        else if(!any(grepl(ext, odbcDataSources()))) {
	    Message(gettext_("No ODBC driver for this file type was found.\nYou probably need to install Excel or Access, or separate ODBC drivers."),
                    type="error")
            return()
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
            return()
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
        return()

    doItAndPrint("corpus <- Corpus(DataframeSource(corpusDataset[1]))")
    doItAndPrint("corpusVars <- corpusDataset[-1]")
    doItAndPrint("activeDataSet(\"corpusVars\")")
    doItAndPrint("setCorpusVariables()")

    processCorpusDlg()
}

processCorpusDlg <- function() {
    # Let the user select processing options
    initializeDialog(title=gettext_("Import Corpus"))
    # TRANSLATORS: replace 'english' with your language's ISO 639-1 English name
    tclLang <- tclVar(gettext_("english"))
    entryLang <- ttkentry(top, width="12", textvariable=tclLang)
    checkBoxes(frame="checkBoxFrame",
               boxes=c("lowercase", "punctuation", "numbers", "stopwords", "stemming"),
               initialValues=rep(1, 5),
               labels=c(gettext_("Ignore case"), gettext_("Remove punctuation"),
                        gettext_("Remove numbers"), gettext_("Remove stopwords"),
                        gettext_("Stem words")),
               title=gettext_("Text processing:"))

    onOK <- function() {
        closeDialog()

        # Set language
        lang <- tclvalue(tclLang)
        if(lang == "")
            Message(message=gettext_("No language has been chosen, using English."),
                    type="warning")

        # Process texts
        lowercase <- tclvalue(lowercaseVariable) == 1
        punctuation <- tclvalue(punctuationVariable) == 1
        numbers <- tclvalue(numbersVariable) == 1
        stopwords <- tclvalue(stopwordsVariable) == 1
        stemming <- tclvalue(stemmingVariable) == 1

        if(lowercase)
            doItAndPrint("corpus <- tm_map(corpus, tolower)")
        if(punctuation) {
            # Workaround to avoid French articles from getting concatenated with their noun
            if(lang == "french")
                doItAndPrint("corpus <- tm_map(corpus, function(x) gsub(\"[\'\U2019]\", \" \", x))")

            doItAndPrint("corpus <- tm_map(corpus, removePunctuation)")
        }
        if(numbers)
            doItAndPrint("corpus <- tm_map(corpus, removeNumbers)")
        if(stopwords)
            doItAndPrint(paste("corpus <- tm_map(corpus, removeWords, stopwords(\"", lang, "\"))", sep=""))
        if(stemming)
            doItAndPrint(paste("corpus <- tm_map(corpus, function(x) stemDocument(x, language=\"", lang, "\"))", sep=""))

        # Extract terms
        doItAndPrint("dtm <- DocumentTermMatrix(corpus)")

        activateMenus()

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="processCorpusDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Language of texts in the corpus:")), entryLang, sticky="w")
    tkgrid(checkBoxFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2, focus=entryLang)
}
