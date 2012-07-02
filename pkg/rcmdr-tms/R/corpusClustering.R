corpusClustDlg <- function() {
    initializeDialog(title=gettext_("Run Hierarchical Clustering"))
    tclSparsity <- tclVar(95)
    sliderSparsity <- tkscale(top, from=1, to=100,
                              showvalue=TRUE, variable=tclSparsity,
		              resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        setBusyCursor()

        sparsity <- as.numeric(tclvalue(tclSparsity))/100

        # removeSparseTerms() does not accept 1
        if(sparsity < 1) {
            doItAndPrint(sprintf("clustDtm <- removeSparseTerms(dtm, %s)", sparsity))

            if(any(row_sums(clustDtm) == 0)) {
                msg <- sprintf(ngettext_(sum(row_sums(clustDtm) == 0),
                             "Document %s has been skipped because it does not include any occurrence of the terms retained in the final document-term matrix.\nIncrease the value of the 'sparsity' parameter to fix this warning.",
                             "Documents %s have been skipped because they do not include any occurrence of the terms retained in the final document-term matrix.\nIncrease the value of the 'sparsity' parameter to fix this warning."),
                             paste(rownames(clustDtm)[row_sums(clustDtm) == 0], collapse=", "))
                Message(msg, type="warning")

                doItAndPrint('clustDtm <- clustDtm[row_sums(clustDtm) > 0,]')
            }

            doItAndPrint('chisqDist <- dist(sweep(clustDtm/row_sums(clustDtm), 2, sqrt(sum(clustDtm)/col_sums(clustDtm)), "*"))')
            doItAndPrint('corpusClust <- hclust(chisqDist, method="ward")')
            doItAndPrint("rm(clustDtm, chisqDist)")
            gc()
        }
        else {
            doItAndPrint('chisqDist <- dist(sweep(dtm/row_sums(dtm), 2, sqrt(sum(dtm)/col_sums(dtm)), "*"))')
            doItAndPrint('corpusClust <- hclust(chisqDist, method="ward")')
            doItAndPrint("rm(chisqDist)")
        }

        doItAndPrint(sprintf('plot(as.dendrogram(corpusClust), nodePar=list(pch=NA, lab.cex=0.8), %sylab="%s", main="%s")',
                             if(length(corpus) > 20) 'leaflab="none", ' else "",
                             gettext_("Within-cluster variance"),
                             gettext_("Full cluster dendrogram")))

        # For the Create clusters item
        activateMenus()

        setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="corpusClustDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Remove terms missing from more than (% of documents):")),
           sliderSparsity, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2)
}

createClustersDlg <- function() {
    if(!(exists("corpusClust") && class(corpusClust) == "hclust")) {
        Message(message=gettext_("Please run a hierarchical clustering on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=gettext_("Create Clusters"))
    tclNClust <- tclVar(5)
    sliderNClust <- tkscale(top, from=2, to=min(15, length(corpusClust$order)),
                            showvalue=TRUE, variable=tclNClust,
		            resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        nclust <- as.numeric(tclvalue(tclNClust))
        height <- floor(rev(corpusClust$height)[nclust-1] * 1e4)/1e4

        doItAndPrint(paste("meta(corpus, tag=\"", gettext_("Cluster"),
                           "\") <- cutree(corpusClust, h=", height, ")", sep=""))

        # If corpus was split, we cannot add cluster back into corpusVars
        if(exists("corpusVars")) {
            # If corpus was split, we cannot add cluster back into corpusVars
            if(nrow(corpusVars) == length(corpus))
                doItAndPrint(paste("corpusVars$",  gettext_("Cluster"),
                                   " <- meta(corpus, tag=\"", gettext_("Cluster"), "\")[[1]]", sep=""))
        }
        else {
            doItAndPrint(paste("corpusVars <- data.frame(",  gettext_("Cluster"),
                               "=meta(corpus, tag=\"", gettext_("Cluster"), "\")[[1]])", sep=""))
        }

        doItAndPrint(paste("corpusSubClust <- cut(as.dendrogram(corpusClust), h=",
                           height, ")", sep=""))
        doItAndPrint(sprintf('plot(corpusSubClust$upper, nodePar=list(pch=NA, lab.cex=0.8), ylab="%s", main="%s")',
                             gettext_("Within-cluster variance"),
                             gettext_("Cluster dendrogram")))
        doItAndPrint(paste("tapply(names(corpus), meta(corpus, tag=\"", gettext_("Cluster"),
                           "\"), paste)", sep=""))

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="createClustersDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Number of clusters:")), sliderNClust, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=2, columns=2)
}

