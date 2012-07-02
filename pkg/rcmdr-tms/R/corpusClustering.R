showCorpusClustering <- function(corpusSubClust, ndocs=10, nterms=20) {
    objects <- getCorpusWindow()
    window <- objects$window
    txt <- objects$txt
    listbox <- objects$listbox

    mark <- 0

    tktag.configure(txt, "heading", font="sans 13 bold")
    tktag.configure(txt, "articlehead", font="sans 12 bold")
    tktag.configure(txt, "details", font="sans 10 italic")
    tktag.configure(txt, "small", font="sans 5")
    tktag.configure(txt, "fixed", font="courier 11")

    tkinsert(txt, "end", paste(gettext_("Clusters summary:"), "\n", sep=""), "heading")
    tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
    tkinsert(listbox, "end", gettext_("Clusters summary"))
    mark <- mark + 1

    val <- round(rbind(sapply(corpusSubClust$lower, attr, "members"),
                       sapply(corpusSubClust$lower, attr, "members")/sum(!is.na(meta(corpus, gettext_("Cluster")))) * 100,
                       sapply(corpusSubClust$lower, attr, "height")), digits=1)
    rownames(val) <- c(gettext_("Number of documents"), gettext_("% of documents"), gettext_("Intra-class inertia"))
    colnames(val) <- seq.int(ncol(val))
    names(dimnames(val)) <- c("", gettext_("Cluster"))
    tkinsert(txt, "end", paste(capture.output(val), collapse="\n"), "fixed")

    meta <- meta(corpus)[!colnames(meta(corpus)) %in% c("MetaID", gettext_("Cluster"))]
    clusters <- meta(corpus, gettext_("Cluster"))[[1]]

    # Set by createClassesDlg()
    # It is more correct to use exactly the same matrix, and it is more efficient
    if(length(attr(corpusSubClust, "sparsity")) > 0)
        dtm <- removeSparseTerms(dtm, attr(corpusSubClust, "sparsity"))

    if(nterms > 0) {
        # Get most contributive terms for each cluster
        # Same code as in typicalTermsDlg()
        clusterDtm <- rollup(dtm, 1, clusters)
        expected <- row_sums(clusterDtm) %o% col_sums(clusterDtm)/sum(clusterDtm)
        chisq <- sign(as.matrix(clusterDtm - expected)) *  as.matrix((clusterDtm - expected)^2/expected)
        termsCtr <- sapply(rownames(clusterDtm), simplify=FALSE, USE.NAMES=TRUE, function(x)
                           round(chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[seq(1, min(nterms, length(chisq[x,])))]]))

        rowTot <- as.matrix(row_sums(clusterDtm))[,1]
        colTot <- as.matrix(col_sums(clusterDtm))[,1]
    }

    for(j in 1:ncol(val)) {
        if(nterms > 0) {
            tkinsert(txt, "end",
                     paste("\n\n", sprintf(gettext_("Terms most typical of cluster %i:"), j), "\n", sep=""),
                     "heading")
            tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
            tkinsert(listbox, "end", sprintf(gettext_("Cluster %i:"), j))
            tkitemconfigure(listbox, mark, background="grey")
            mark <- mark + 1

            termsNames <- names(termsCtr[[j]])
            df <- data.frame(row.names=termsNames,
                             round(as.numeric(as.matrix(clusterDtm[j, termsNames])/rowTot[j] * 100), digits=2),
                             round(as.numeric(as.matrix(clusterDtm[j, termsNames])/colTot[termsNames] * 100), digits=2),
                             round(termsCtr[[j]]))
            colnames(df) <- c(gettext_("Prevalence (%)"), gettext_("Share of occur. (%)"), gettext_("Chi2 contr."))

            tkinsert(txt, "end", paste(capture.output(df), collapse="\n"), "fixed")
        }

        if(ndocs > 0) {
            # Remove terms that do not appear in the cluster
            keep <- as.matrix(clusterDtm[j,] > 0)
            docDtm <- dtm[clusters %in% j, keep]
            dev <- sweep(as.matrix(docDtm)/row_sums(docDtm), 2,
                         as.matrix(clusterDtm[j, keep])/sum(clusterDtm[j, keep]), "-")
            chisq <- rowSums(sweep(dev^2, 2, sum(dtm[,keep])/col_sums(dtm[,keep]), "*"))
            chisq <- sort(chisq)[seq.int(1, min(length(chisq), ndocs))]
            docs <- names(chisq)

            tkinsert(txt, "end",
                     paste("\n\n", sprintf(gettext_("Documents most typical of cluster %i:"), j), "\n", sep=""),
                     "heading")

            df <- data.frame(row.names=docs,
                             round(chisq))
            colnames(df) <- gettext_("Chi2 distance to cluster average")

            tkinsert(txt, "end", paste(capture.output(df), collapse="\n"), "fixed")

            # We need to use IDs rather than indexes to access documents in the corpus
            # since some documents may have been skipped in the clustering
            for(id in docs) {
                tkinsert(txt, "end", paste("\n\n", id, "\n", sep=""),
                         "articlehead")
                tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
                mark <- mark + 1
                tkinsert(listbox, "end", id)

                origin <- meta(corpus[[id]], "Origin")
                date <- meta(corpus[[id]], "DateTimeStamp")
                if(length(origin) > 0 && length(date) > 0)
                    tkinsert(txt, "end", paste(origin, " - ", date, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(origin, "\n", sep=""), "details")
                else if(length(origin) > 0)
                    tkinsert(txt, "end", paste(date, "\n", sep=""), "details")

                if(length(origin) > 0 || length(date) > 0)
                    tkinsert(txt, "end", "\n", "small")

                tkinsert(txt, "end", paste(paste(corpus[[id]], collapse="\n"), "\n\n"))
            }
        }
    }

    if(ncol(meta) > 0) {
        tkinsert(txt, "end",
                  paste("\n\n", gettext_("Distribution of variables among clusters:"), "\n", sep=""),
                 "heading")
        tkinsert(txt, "end", paste(gettext_("Row %"), "\n", sep=""), "details")
        tkinsert(listbox, "end", gettext_("Variables"))
        tkitemconfigure(listbox, mark, background="grey")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        mark <- mark + 1

        dupLevels <- any(duplicated(unlist(lapply(meta,
            function(x) substr(unique(as.character(x[!is.na(x)])), 0, 30)))))

        tab <- lapply(colnames(meta),
                      function(var) {
                          mat <- with(meta(corpus), table(get(var), get(gettext_("Cluster"))))

                          # Handle names like in corpusCa()
                          # If only one level is present, don't add the level name (e.g. TRUE or YES)
                          if(nrow(mat) == 1)
                              rownames(mat) <- substr(var, 0, 20)
                          # In case of ambiguous levels of only numbers in levels, add variable names everywhere
                          else if(dupLevels || !any(is.na(suppressWarnings(as.numeric(rownames(mat))))))
                              rownames(mat) <- make.unique(paste(substr(var, 0, 10), substr(rownames(mat), 0, 30)))
                          else # Most general case: no need to waste space with variable names
                              rownames(mat) <- substr(rownames(mat), 0, 30)

                          mat
                       })

        tab <- do.call(rbind, tab)
        tab <- rbind(tab, colSums(tab))
        rownames(tab)[nrow(tab)] <- gettext_("Corpus")
        names(dimnames(tab)) <- c("", gettext_("Cluster"))
        tab <- round(prop.table(tab, 1) * 100, digits=1)

        tkinsert(txt, "end", paste(capture.output(tab), collapse="\n"), "fixed")
    }

    # Only raise the window when we're done, as filling it may take some time
    tkraise(window)
}

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
            # Used by createClustersDialog() and showCorpusClust() to recreate the dtm
            doItAndPrint(sprintf('attr(corpusClust, "sparsity") <- %s', sparsity))
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

    tclNDocs <- tclVar(5)
    sliderNDocs <- tkscale(top, from=0, to=20,
                           showvalue=TRUE, variable=tclNDocs,
		           resolution=1, orient="horizontal")

    tclNTerms <- tclVar(20)
    sliderNTerms <- tkscale(top, from=0, to=100,
                            showvalue=TRUE, variable=tclNTerms,
		            resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        setBusyCursor()

        nclust <- as.numeric(tclvalue(tclNClust))
        ndocs <- as.numeric(tclvalue(tclNDocs))
        nterms <- as.numeric(tclvalue(tclNTerms))
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

        # Set by corpusClustDlg() and used by showCorpusClustering() to recreate dtm
        if(length(attr(corpusClust, "sparsity")) > 0)
            doItAndPrint(sprintf('attr(corpusSubClust, "sparsity") <- %s', attr(corpusClust, "sparsity")))

        doItAndPrint(sprintf('plot(corpusSubClust$upper, nodePar=list(pch=NA, lab.cex=0.8), ylab="%s", main="%s")',
                             gettext_("Within-cluster variance"),
                             gettext_("Cluster dendrogram")))
        doItAndPrint(sprintf("showCorpusClustering(corpusSubClust, %i, %i)", ndocs, nterms))

        setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="createClustersDlg")
    tkgrid(labelRcmdr(top, text=gettext_("Number of clusters to create:")), sliderNClust, sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=gettext_("Number of documents to show:")), sliderNDocs, sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=gettext_("Number of terms to show:")), sliderNTerms, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

