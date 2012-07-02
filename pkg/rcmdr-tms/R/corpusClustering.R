corpusClustDlg <- function() {
    initializeDialog(title=gettext_("Run Hierarchical Clustering"))
    tclSparsity <- tclVar(95)
    sliderSparsity <- tkscale(top, from=1, to=100,
                              showvalue=TRUE, variable=tclSparsity,
		              resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        sparsity <- as.numeric(tclvalue(tclSparsity))

        doItAndPrint(paste("corpusClust <- hclust(dist(removeSparseTerms(dtm, ",
                           sparsity/100, ")), method = \"ward\")", sep=""))

        # For the Create classes item
        activateMenus()

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="corpusClustDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Remove terms missing from more than (% of documents):")),
           sliderSparsity, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2)
}

createClassesDlg <- function() {
    if(!(exists("corpusClust") && class(corpusClust) == "hclust")) {
        Message(message=gettext_("Please run a hierarchical clustering on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Create Classes"))
    totalHeight <- round(max(corpusClust$height)/10)*10
#    totalHeight <- attr(corpusClust, "height")
    tclHeight <- tclVar(totalHeight/10)
    sliderHeight <- tkscale(top, from=0, to=totalHeight,
                            showvalue=TRUE, variable=tclHeight,
		            resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        height <- as.numeric(tclvalue(tclHeight))

        doItAndPrint(paste("meta(corpus, tag=\"", gettext_("class"),
                           "\") <- cutree(corpusClust, h=", height, ")", sep=""))

        if(exists("corpusVars"))
            doItAndPrint(paste("corpusVars$",  gettext_("class"),
                               " <- meta(corpus, tag=\"", gettext_("class"), "\")", sep=""))
        else
            doItAndPrint(paste("corpusVars <- data.frame(",  gettext_("class"),
                               "=meta(corpus, tag=\"", gettext_("class"), "\"))", sep=""))

        doItAndPrint(paste("corpusSubClust <- cut(as.dendrogram(corpusClust), h=",
                           height, ")", sep=""))
        doItAndPrint("plot(corpusSubClust$upper)")
        doItAndPrint(paste("tapply(names(corpus), meta(corpus, tag=\"", gettext_("class"),
                           "\"), paste)", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="createClassesDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Cut height:")), sliderHeight, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2)
}

