showCorpusClustering <- function(corpusSubClust, ndocs=10, nterms=20) {
    .setBusyCursor()

    objects <- .getCorpusWindow()
    window <- objects$window
    txt <- objects$txt
    listbox <- objects$listbox

    tkwm.title(window, .gettext("Hierarchical Clustering"))

    mark <- 0

    tktag.configure(txt, "heading", font="sans 13 bold")
    tktag.configure(txt, "articlehead", font="sans 12 bold")
    tktag.configure(txt, "details", font="sans 10 italic")
    tktag.configure(txt, "small", font="sans 5")
    tktag.configure(txt, "fixed", font="courier 11")

    tkinsert(txt, "end", paste(.gettext("Clusters summary:"), "\n", sep=""), "heading")
    tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
    tkinsert(listbox, "end", .gettext("Clusters summary"))
    mark <- mark + 1

    val <- rbind(sapply(corpusSubClust$lower, attr, "members"),
                 sapply(corpusSubClust$lower, attr, "members")/sum(!is.na(meta(corpus, .gettext("Cluster")))) * 100,
                 sapply(corpusSubClust$lower, attr, "height"))
    rownames(val) <- c(.gettext("Number of documents"), .gettext("% of documents"), .gettext("Within-cluster variance"))
    colnames(val) <- seq.int(ncol(val))
    names(dimnames(val)) <- c("", .gettext("Cluster"))
    tkinsert(txt, "end", paste(capture.output(format(as.data.frame(val), nsmall=1, digits=2, width=6)),
                               collapse="\n"), "fixed")

    meta <- meta(corpus)[!colnames(meta(corpus)) %in% c("MetaID", .gettext("Cluster"))]
    clusters <- meta(corpus, .gettext("Cluster"))[[1]]

    # Set by createClassesDlg()
    # It is more correct to use exactly the same matrix, and it is more efficient
    if(length(attr(corpusSubClust, "sparsity")) > 0)
        dtm <- removeSparseTerms(dtm, attr(corpusSubClust, "sparsity"))

    if(nterms > 0) {
        # Get most contributive terms for each cluster
        # Same code as in typicalTermsDlg()
        clusterDtm <- suppressWarnings(rollup(dtm, 1, clusters))
        expected <- row_sums(clusterDtm) %o% col_sums(clusterDtm)/sum(clusterDtm)
        chisq <- sign(as.matrix(clusterDtm - expected)) *  as.matrix((clusterDtm - expected)^2/expected)
        termsCtr <- sapply(rownames(clusterDtm), simplify=FALSE, USE.NAMES=TRUE, function(x)
                           chisq[x,order(abs(chisq[x,]), decreasing=TRUE)[seq(1, min(nterms, length(chisq[x,])))]])

        rowTot <- as.matrix(row_sums(clusterDtm))[,1]
        colTot <- as.matrix(col_sums(clusterDtm))[,1]
    }

    for(j in 1:ncol(val)) {
        if(nterms > 0) {
            tkinsert(txt, "end",
                     paste("\n\n", sprintf(.gettext("Terms most typical of cluster %i:"), j), "\n", sep=""),
                     "heading")
            tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
            tkinsert(listbox, "end", sprintf(.gettext("Cluster %i:"), j))
            tkitemconfigure(listbox, mark, background="grey")
            mark <- mark + 1

            termsNames <- names(termsCtr[[j]])
            df <- data.frame(row.names=termsNames,
                             as.numeric(as.matrix(clusterDtm[j, termsNames])/rowTot[j] * 100),
                             as.numeric(as.matrix(clusterDtm[j, termsNames])/colTot[termsNames] * 100),
                             termsCtr[[j]])
            colnames(df) <- c(.gettext("Prevalence (%)"), .gettext("Distribution (%)"), .gettext("Chi2 contr."))

            tkinsert(txt, "end", paste(capture.output(format(df, nsmall=2, digits=2,
                                                             width=max(nchar(colnames(df), "width")))),
                                       collapse="\n"), "fixed")
        }

        if(ndocs > 0) {
            # Remove terms that do not appear in the cluster
            keep <- as.matrix(clusterDtm[j,] > 0)
            docDtm <- dtm[clusters %in% j, keep]
            clustDtm <- clusterDtm[j, keep]
            dev <- sweep(as.matrix(docDtm)/row_sums(docDtm), 2,
                         as.matrix(clustDtm)/sum(clustDtm), "-")
            chisq <- rowSums(sweep(dev^2, 2, col_sums(dtm[,keep])/sum(dtm), "/"))
            chisq <- sort(chisq)[seq.int(1, min(length(chisq), ndocs))]
            docs <- names(chisq)

            tkinsert(txt, "end",
                     paste("\n\n", sprintf(.gettext("Documents most typical of cluster %i:"), j), "\n", sep=""),
                     "heading")

            df <- data.frame(row.names=docs, chisq)
            colnames(df) <- .gettext("Chi2 distance to cluster average")

            tkinsert(txt, "end", paste(capture.output(format(df, nsmall=1, digits=2)), collapse="\n"), "fixed")

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
                  paste("\n\n", .gettext("Distribution of variables among clusters:"), "\n", sep=""),
                 "heading")
        tkinsert(txt, "end", paste(.gettext("Row %"), "\n", sep=""), "details")
        tkinsert(listbox, "end", .gettext("Variables"))
        tkitemconfigure(listbox, mark, background="grey")
        tkmark.set(txt, paste("mark", mark, sep=""), tkindex(txt, "insert-1c"))
        mark <- mark + 1

        dupLevels <- any(duplicated(unlist(lapply(meta,
            function(x) substr(unique(as.character(x[!is.na(x)])), 0, 30)))))

        tab <- lapply(colnames(meta),
                      function(var) {
                          # We call factor() to drop empty levels, if any
                          mat <- with(meta(corpus), table(factor(get(var)), factor(get(.gettext("Cluster")))))

                          # Keep in sync with corpusCa()

                          # For boolean variables, only show the TRUE when no NA is present
                          # Rationale: TRUE and FALSE are symmetric except when missing values appear
                          if(nrow(mat) == 2 && all(c("TRUE", "FALSE") %in% rownames(mat)) &&
                             !any(is.na(meta(corpus, .gettext("Cluster"))[[1]]))) {
                              mat<-mat["TRUE", , drop=FALSE]
                              rownames(mat)<-substr(var, 0, 20)
                          }
                          # If only one level is present, don't add the level name (e.g. YES),
                          # except if all values are the same (in which case variable is useless but is more obvious that way)
                          else if(nrow(mat) == 1 && any(is.na(meta(corpus, .gettext("Cluster"))[[1]])))
                              rownames(mat)<-substr(var, 0, 20)
                          # In case of ambiguous levels of only numbers in levels, add variable names everywhere
                          else if(dupLevels || !any(is.na(suppressWarnings(as.numeric(rownames(mat))))))
                              rownames(mat)<-make.unique(paste(substr(var, 0, 10), substr(rownames(mat), 0, 30)))
                          else # Most general case: no need to waste space with variable names
                              rownames(mat)<-substr(levs, 0, 30)

                          mat
                       })

        tab <- do.call(rbind, tab)
        tab <- rbind(tab, colSums(tab))
        rownames(tab)[nrow(tab)] <- .gettext("Corpus")
        names(dimnames(tab)) <- c("", .gettext("Cluster"))
        tab <- prop.table(tab, 1) * 100

        tkinsert(txt, "end", paste(capture.output(format(as.data.frame(tab), nsmall=1, digits=2, width=5)),
                                   collapse="\n"), "fixed")
    }

    .setIdleCursor()

    # Only raise the window when we're done, as filling it may take some time
    tkraise(window)
}

corpusClustDlg <- function() {
    initializeDialog(title=.gettext("Run Hierarchical Clustering"))

    labelNDocs <- labelRcmdr(top)

    labels <- c(.gettext("(Terms present in at least %s documents will be retained in the analysis.)"),
                .gettext("(Terms present in at least one document will be retained in the analysis.)"),
                .gettext("(All terms will be retained in the analysis.)"))

    tkconfigure(labelNDocs, width=max(nchar(labels)))

    updateNDocs <- function(...) {
        ndocs <- floor((1 - as.numeric(tclvalue(tclSparsity))/100) * nrow(dtm))

        if(ndocs > 1)
            tkconfigure(labelNDocs, text=sprintf(labels[1], ndocs))
        else if(ndocs == 1)
            tkconfigure(labelNDocs, text=labels[2])
        else
            tkconfigure(labelNDocs, text=labels[3])
    }

    tclSparsity <- tclVar(95)
    sliderSparsity <- tkscale(top, from=1, to=100,
                              showvalue=TRUE, variable=tclSparsity,
		              resolution=1, orient="horizontal",
                              command=updateNDocs)
     updateNDocs()

    onOK <- function() {
        closeDialog()

        .setBusyCursor()

        sparsity <- as.numeric(tclvalue(tclSparsity))/100

        # removeSparseTerms() does not accept 1
        if(sparsity < 1) {
            doItAndPrint(sprintf("clustDtm <- removeSparseTerms(dtm, %s)", sparsity))

            if(any(row_sums(clustDtm) == 0)) {
                msg <- sprintf(.ngettext(sum(row_sums(clustDtm) == 0),
                             "Document %s has been skipped because it does not include any occurrence of the terms retained in the final document-term matrix.\nIncrease the value of the 'sparsity' parameter to fix this warning.",
                             "Documents %s have been skipped because they do not include any occurrence of the terms retained in the final document-term matrix.\nIncrease the value of the 'sparsity' parameter to fix this warning."),
                             paste(rownames(clustDtm)[row_sums(clustDtm) == 0], collapse=", "))
                Message(msg, type="warning")

                doItAndPrint('clustDtm <- clustDtm[row_sums(clustDtm) > 0,]')
            }

            doItAndPrint('chisqDist <- dist(sweep(clustDtm/row_sums(clustDtm), 2, sqrt(col_sums(clustDtm)/sum(clustDtm)), "/"))')
            doItAndPrint('corpusClust <- hclust(chisqDist, method="ward")')
            # Used by createClustersDialog() and showCorpusClust() to recreate the dtm
            doItAndPrint(sprintf('attr(corpusClust, "sparsity") <- %s', sparsity))
            doItAndPrint("rm(clustDtm, chisqDist)")
            gc()
        }
        else {
            doItAndPrint('chisqDist <- dist(sweep(dtm/row_sums(dtm), 2, sqrt(col_sums(dtm)/sum(dtm)), "/"))')
            doItAndPrint('corpusClust <- hclust(chisqDist, method="ward")')
            doItAndPrint("rm(chisqDist)")
        }

        doItAndPrint(sprintf('plot(as.dendrogram(corpusClust), nodePar=list(pch=NA, lab.cex=0.8), %sylab="%s", main="%s")',
                             if(length(corpus) > 20) 'leaflab="none", ' else "",
                             .gettext("Within-cluster variance"),
                             .gettext("Full cluster dendrogram")))

        # For the Create clusters item
        activateMenus()

        .setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="corpusClustDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Remove terms missing from more than (% of documents):")),
           sliderSparsity, sticky="sw", pady=6)
    tkgrid(labelNDocs, sticky="sw", pady=6, columnspan=2)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

createClustersDlg <- function() {
    if(!(exists("corpusClust") && class(corpusClust) == "hclust")) {
        Message(message=.gettext("Please run a hierarchical clustering on the corpus first."),
                type="error")
        return()
    }

    initializeDialog(title=.gettext("Create Clusters"))

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

        .setBusyCursor()

        nclust <- as.numeric(tclvalue(tclNClust))
        ndocs <- as.numeric(tclvalue(tclNDocs))
        nterms <- as.numeric(tclvalue(tclNTerms))
        height <- floor(rev(corpusClust$height)[nclust-1] * 1e4)/1e4

        doItAndPrint(sprintf("clusters <- cutree(corpusClust, h=%s)", height))

        # If some documents were skipped, we need to skip them and put NA
        if(length(clusters) == length(corpus)) {
            doItAndPrint(paste("meta(corpus, \"", .gettext("Cluster"), "\") <- clusters", sep=""))
        }
        else {
            doItAndPrint(paste("meta(corpus, \"", .gettext("Cluster"), "\") <- NA", sep=""))
            doItAndPrint(paste("meta(corpus, \"", .gettext("Cluster"),
                               "\")[match(names(corpus), names(clusters), nomatch=0),] <- clusters", sep=""))
        }

        # If corpus was split, we cannot add cluster back into corpusVars
        if(exists("corpusVars")) {
            # If corpus was split, we cannot add cluster back into corpusVars
            if(nrow(corpusVars) == length(corpus))
                doItAndPrint(paste("corpusVars$",  .gettext("Cluster"),
                                   " <- meta(corpus, tag=\"", .gettext("Cluster"), "\")[[1]]", sep=""))
        }
        else {
            doItAndPrint(paste("corpusVars <- data.frame(",  .gettext("Cluster"),
                               "=meta(corpus, tag=\"", .gettext("Cluster"), "\")[[1]])", sep=""))
        }

        doItAndPrint(paste("corpusSubClust <- cut(as.dendrogram(corpusClust), h=",
                           height, ")", sep=""))

        # Set by corpusClustDlg() and used by showCorpusClustering() to recreate dtm
        if(length(attr(corpusClust, "sparsity")) > 0)
            doItAndPrint(sprintf('attr(corpusSubClust, "sparsity") <- %s', attr(corpusClust, "sparsity")))

        doItAndPrint(sprintf('plot(corpusSubClust$upper, nodePar=list(pch=NA, lab.cex=0.8), ylab="%s", main="%s")',
                             .gettext("Within-cluster variance"),
                             .gettext("Cluster dendrogram")))
        doItAndPrint(sprintf("showCorpusClustering(corpusSubClust, %i, %i)", ndocs, nterms))
        doItAndPrint("rm(clusters)")

        .setIdleCursor()
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject="createClustersDlg")
    tkgrid(labelRcmdr(top, text=.gettext("Clusters creation:"), foreground="blue"),
           sticky="sw", pady=0)
    tkgrid(labelRcmdr(top, text=.gettext("Number of clusters to retain:")), sliderNClust,
           sticky="sw", pady=c(0, 6), padx=c(6, 0))
    tkgrid(labelRcmdr(top, text=.gettext("Number of items to show (for each cluster):"), foreground="blue"),
           sticky="sw", pady=c(24, 0))
    tkgrid(labelRcmdr(top, text=.gettext("Most typical documents:")), sliderNDocs,
           sticky="sw", pady=c(0, 6), padx=c(6, 0))
    tkgrid(labelRcmdr(top, text=.gettext("Most typical terms:")), sliderNTerms,
           sticky="sw", pady=6, padx=c(6, 0))
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=5, columns=2)
}

