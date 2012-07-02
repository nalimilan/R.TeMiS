corpusCa <- function(corpus, sparsity=0.9, ...) {
    if(!exists("dtm"))
        dtm<-DocumentTermMatrix(corpus)

    dtm<-as.matrix(removeSparseTerms(dtm, sparsity))
    invalid<-which(apply(dtm,1,sum)==0)
    if(length(invalid) > 0) {
        dtm<-dtm[-invalid,]
        corpus<-corpus[-invalid]
        msg<-sprintf(ngettext_(length(invalid),
                     "Document %s has been skipped because it does not include any occurrence of the terms retained in the final document-term matrix.\nLower the value of the 'sparsity' parameter to fix this warning.",
                     "Documents %s have been skipped because they do not include any occurrence of the terms retained in the final document-term matrix.\nLower the value of the 'sparsity' parameter to fix this warning."),
                     paste(names(invalid), collapse=", "))
        Message(msg, type="warning")
    }

    ndocs<-nrow(dtm)
    nterms<-ncol(dtm)
    meta<-meta(corpus)[colnames(meta(corpus)) != "MetaID"]

    # Create mean dummy variables as rows
    if(ncol(meta) > 0) {
        for(i in 1:ncol(meta)) {
            levels<-levels(factor(meta[,i]))
            totNLevels<-nlevels(factor(meta(corpus)[colnames(meta(corpus)) != "MetaID"][,i]))

            if(length(levels) == 0) {
                Message(sprintf(gettext_("Variable %s has been skipped since it contains only missing values for retained documents."),
                                colnames(meta)[i]),
                        type="note")
                break
            }
            else if(length(levels) < totNLevels) {
                Message(sprintf(gettext_("Some levels of variable %s has been skipped since they contain only missing values for retained documents."),
                                colnames(meta)[i]),
                        type="note")
                break
            }

            mat<-aggregate(dtm[1:ndocs,], meta[i], sum)[,-1]

            # If only one level is present, don't add the level name
            # (probably something like TRUE or YES)
            if(totNLevels == 1)
                rownames(mat)<-colnames(meta)[i]
            else
                rownames(mat)<-paste(colnames(meta)[i], levels)

            dtm<-rbind(dtm, mat)
        }
    }

    Message(sprintf(gettext_("Running correspondence analysis using %i documents, %i terms and %i variables."),
                            ndocs, nterms, ncol(meta)),
            type="note")

    if(ncol(meta) > 0)
        ca(dtm, suprow=(ndocs+1):nrow(dtm), ...)
    else
        ca(dtm, ...)
}

corpusCaDlg <- function() {
    initializeDialog(title=gettext_("Run Correspondence Analysis"))
    tclSparsity <- tclVar(95)
    sliderSparsity <- tkscale(top, from=1, to=100,
                              showvalue=TRUE, variable=tclSparsity,
		              resolution=1, orient="horizontal")
    tclDim <- tclVar(5)
    sliderDim <- tkscale(top, from=1, to=100,
                         showvalue=TRUE, variable=tclDim,
	                 resolution=1, orient="horizontal")

    onOK <- function() {
        closeDialog()

        if(ncol(meta(corpus)[colnames(meta(corpus)) != "MetaID"]) == 0)
            Message(message=gettext_("No corpus variables have been set. Use Text mining->Set corpus variables to add them."),
                    type="note")

        sparsity <- as.numeric(tclvalue(tclSparsity))
        dim <- as.numeric(tclvalue(tclDim))

        doItAndPrint(paste("corpusCa <- corpusCa(corpus, sparsity=", sparsity/100, ", nd=", dim, ")", sep=""))
        doItAndPrint("print(corpusCa)")

        activateMenus()

        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject=corpusCaDlg)
    tkgrid(labelRcmdr(top, text=gettext_("Only keep terms present in more than (% of documents):")),
           sliderSparsity, sticky="sw", pady=6)
    tkgrid(labelRcmdr(top, text=gettext_("Number of dimensions to retain:")),
           sliderDim, sticky="sw", pady=6)
    tkgrid(buttonsFrame, columnspan="2", sticky="w", pady=6)
    dialogSuffix(rows=3, columns=2)
}

