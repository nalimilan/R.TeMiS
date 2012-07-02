# Choose a directory to load texts from
importCorpusFromDir <- function() {
    dir <- tclvalue(tkchooseDirectory(initialdir=getwd()))
    if (dir == "") return()

    doItAndPrint(paste("corpus <- Corpus(DirSource(\"", dir, "\"))", sep=""))

    processCorpusDlg()

    assign("corpusMetaData", data.frame(var1=rep(NA, length(corpus)), row.names=names(corpus)), envir=.GlobalEnv)
    activeDataSet("corpusMetaData")
}

# Choose a CSV file to load texts and meta-data from
importCorpusFromFile <- function() {
    file <- tclvalue(tkgetOpenFile(filetypes=gettext('{"CSV file" {".csv" ".CSV"}}')))
    if (file == "") return()

    doItAndPrint(paste("corpusDataset <- read.csv(\"", file, "\")", sep=""))
    doItAndPrint("corpus <- Corpus(DataframeSource(corpusDataset[1]))")

    processCorpusDlg()

    doItAndPrint("corpusMetadata <- corpusDataset[-1]")
    doItAndPrint("activeDataSet(\"corpusMetadata\")")
    doItAndPrint("setCorpusMetadata()")
}

processCorpusDlg <- function() {
    # Let the user select processing options
    initializeDialog(title=gettext("Import Corpus"))
    tclLang <- tclVar(gettext("english"))
    entryLang <- ttkentry(top, width="12", textvariable=tclLang)
    checkBoxes(frame="checkBoxFrame",
               boxes=c("lowercase", "punctuation", "numbers", "stopwords", "stemming"),
               initialValues=rep(1, 5),
               labels=gettext(c("Ignore case", "Remove punctuation", "Remove numbers", "Remove stopwords", "Stem words")),
               title=gettext("Text processing:"))

    onOK <- function() {
        closeDialog()

        # Set language
        lang <- tclvalue(tclLang)
        if(lang == "")
            Message(message=gettext("No language has been chosen, using English."),
                    type="warning")

        # Process texts
        lowercase <- tclvalue(lowercaseVariable) == 1
        punctuation <- tclvalue(punctuationVariable) == 1
        numbers <- tclvalue(numbersVariable) == 1
        stopwords <- tclvalue(stopwordsVariable) == 1
        stemming <- tclvalue(stemmingVariable) == 1

        if(lowercase)
            doItAndPrint("corpus <- tm_map(corpus, tolower)")
        if(punctuation)
            doItAndPrint("corpus <- tm_map(corpus, removePunctuation)")
        if(numbers)
            doItAndPrint("corpus <- tm_map(corpus, removeNumbers)")
        if(stopwords)
            doItAndPrint(paste("corpus <- tm_map(corpus, removeWords, stopwords(\"", lang, "\"))", sep=""))
        if(stemming)
            doItAndPrint(paste("corpus <- tm_map(corpus, function(x) stemDocument(x, language=\"", lang, "\"))", sep=""))

        # Extract terms
        doItAndPrint("dtm <- DocumentTermMatrix(corpus)")

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="processCorpusDlg")
    tkgrid(labelRcmdr(top, text=gettext("Language of texts in the corpus:")), entryLang, sticky="w")
    tkgrid(checkBoxFrame, columnspan="2", sticky="w", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2, focus=entryLang)
}
