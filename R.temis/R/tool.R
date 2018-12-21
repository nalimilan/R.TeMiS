specificTerms <- function(dtm, variable=NULL, p=0.1, n.max=25, sparsity=0.95, min.occ=2) {
  #if(!is.null(variable) && length(unique(variable)) < 2)
    #stop(.gettext("Please provide a variable with at least two levels."))

  if(!is.null(variable))
    dtm <- rollup(dtm, 1, variable)

  # We need to compute these statistics before removing terms so that they are stable
  rs <- row_sums(dtm)
  tot <- sum(rs)

  if(sparsity < 1)
    dtm <- removeSparseTerms(dtm, sparsity)

  if(min.occ > 1)
    dtm <- dtm[, col_sums(dtm) >= min.occ]

  cs <- col_sums(dtm)
  cs.tot <- cs/tot

  sapply(rownames(dtm), simplify=FALSE, function(l) {
    # rownames(dtm) == l is used below because "" is a possible level
    i <- rownames(dtm) == l

    # Empty documents create errors
    if(rs[i] == 0)
      return(numeric(0))

    rp <- as.matrix(dtm[l,]/rs[l])[1,]
    cp <- as.matrix(dtm[l,])[1,]/cs
    sup <- rp > cs.tot

    counts <- as.matrix(dtm[i,])[1,]

    # As this is a discrete distribution, we need to subtract one
    # to include the value when switching sides
    p.val <- phyper(ifelse(sup, counts - 1, counts), rs[l], tot - rs[l], cs)
    t.val <- qnorm(p.val)

    p.val[sup] <- 1 - p.val[sup]

    keep <- which(p.val <= p)

    if(length(keep) == 0) return(numeric(0))

    ord <- head(intersect(order(p.val), keep), n.max)
    ret <- cbind(term.clus=rp[ord] * 100, clus.term=cp[ord] * 100,
                 p.global=cs[ord]/tot * 100, n.int=counts[ord], n.global=cs[ord],
                 t.value=t.val[ord], p.value=round(p.val[ord], 4))
    colnames(ret) <- c(.gettext("% Term/Level"), .gettext("% Level/Term"), .gettext("Global %"),
                       .gettext("Level"), .gettext("Global"),
                       .gettext("t value"), .gettext("Prob."))

    ret <- ret[order(ret[, 6], decreasing=TRUE),]
    n <- which(ret[,6] < 0)[1]
    if(!is.na(n)) {
      ret <- rbind(ret[1:(n-1),, drop=FALSE], NA, ret[n:nrow(ret),, drop=FALSE])
      rownames(ret)[n] <- stri_dup("-", max(nchar(rownames(ret), type="width")))
    }
    ret
  })
}

frequentTerms <- function(dtm, variable=NULL, n=25) {
  if(length(variable) == 1 && is.na(variable)) {
    counts <- sort(col_sums(dtm), decreasing=TRUE)[1:n]
    mat <- cbind(counts, counts/sum(dtm) * 100)
    colnames(mat) <- c("Global freq.", "Global %")
    return(mat)
  }

  #if(!is.null(variable) && length(unique(variable)) < 2)
   # stop("Please provide a variable with at least two levels.")

  if(!is.null(variable))
    dtm <- rollup(dtm, 1, variable)

  rs <- row_sums(dtm)
  tot <- sum(rs)

  cs <- col_sums(dtm)
  cs.tot <- cs/tot

  sapply(rownames(dtm), simplify=FALSE, function(l) {
    # rownames(dtm) == l is used below because "" is a possible level
    i <- rownames(dtm) == l

    # Empty documents create errors
    if(rs[i] == 0)
      return(numeric(0))

    rp <- as.matrix(dtm[l,]/rs[l])[1,]
    cp <- as.matrix(dtm[l,])[1,]/cs
    sup <- rp > cs.tot

    counts <- as.matrix(dtm[i,])[1,]

    # As this is a discrete distribution, we need to subtract one
    # to include the value when switching sides
    p.val <- phyper(ifelse(sup, counts - 1, counts), rs[l], tot - rs[l], cs)
    t.val <- qnorm(p.val)

    p.val[sup] <- 1 - p.val[sup]

    ord <- head(order(counts, decreasing=TRUE), n)
    ret <- cbind(term.clus=rp[ord] * 100, clus.term=cp[ord] * 100,
                 p.global=cs[ord]/tot * 100, n.int=counts[ord], n.global=cs[ord],
                 t.value=t.val[ord], p.value=round(p.val[ord], 4))
    colnames(ret) <- c("% Term/Level","% Level/Term","Global %",
                       "Level","Global",
                      "t value","Prob.")

    ret[order(ret[, 4], decreasing=TRUE),]
  })
}




extractMetadata <- function(corpus, date=TRUE) {
  if(date) {
    dates <- lapply(corpus, meta, "datetimestamp")
    dates <- sapply(dates, function(x) if(length(x) > 0) as.character(x) else NA)
    vars <- data.frame(origin=rep(NA, length(corpus)),
                       date=dates,
                       author=rep(NA, length(corpus)),
                       section=rep(NA, length(corpus)))
  }
  else {
    vars <- data.frame(origin=rep(NA, length(corpus)),
                       author=rep(NA, length(corpus)),
                       section=rep(NA, length(corpus)))
  }

  specialTags <- c("subject", "coverage", "company", "stocksymbol", "industry", "infocode", "infodesc")

  tags <- setdiff(unique(unlist(lapply(corpus, function(x) names(meta(x))))),
                  c("datetimestamp", "heading", "id", "language", specialTags))
  for(tag in tags) {
    var <- lapply(corpus, meta, tag)
    # paste() is here to prevent an error in case x contains more than one elemen
    # This typically happens with Rights
    var <- lapply(var, function(x) if(length(x) > 0) paste(x, collapse=" ") else NA)
    vars[[tag]] <- unlist(var)
  }

  # Keep in sync with importCorpusFromTwitter()
  colnames(vars)[colnames(vars) == "origin"] <- "Origin"
  colnames(vars)[colnames(vars) == "date"] <- "Date"
  colnames(vars)[colnames(vars) == "author"] <- "Author"
  colnames(vars)[colnames(vars) == "section"] <- "Section"
  colnames(vars)[colnames(vars) == "type"] <- "Type"
  colnames(vars)[colnames(vars) == "edition"] <- "Edition"
  colnames(vars)[colnames(vars) == "wordcount"] <- "Word.Count"
  colnames(vars)[colnames(vars) == "pages"] <- "Pages"
  colnames(vars)[colnames(vars) == "publisher"] <- "Publisher"
  colnames(vars)[colnames(vars) == "rights"] <- "Rights"

  # Drop variables with only NAs, which can appear with sources that do not support them
  vars <- vars[sapply(vars, function(x) sum(!is.na(x))) > 0]


  # Tags that contain several values and have to be represented using dummies
  meta <- sapply(corpus, function(x) meta(x)[specialTags])
  # Tags missing from all documents
  meta <- meta[!is.na(rownames(meta)),]
  # Tags missing from some documents
  meta[] <- sapply(meta, function(x) if(is.null(x)) NA else x)

  for(tag in rownames(meta)) {
    var <- meta[tag,]
    levs <- unique(unlist(var))
    levs <- levs[!is.na(levs)]

    if(length(levs) == 0)
      next

    # We remove the identifier before ":" and abbreviate the names since they can get out of control
    for(lev in levs)
      vars[[make.names(substr(gsub("^[[:alnum:]]+ : ", "", lev), 1, 20))]] <- sapply(var, function(x) lev %in% x)
  }

  rownames(vars) <- names(corpus)

  vars
}



vocabularyTable <- function(dtm, wordsDtm, variable=NULL, unit=c("document", "global")) {
  unit <- match.arg(unit)

  if(is.null(variable) || unit == "document") {
    totaltPerDoc <- row_sums(dtm)
    uniquePerDoc <- row_sums(dtm > 0)
    hapaxPerDoc <- row_sums(dtm == 1)
  }


  totalwPerDoc <- row_sums(wordsDtm)


  # Per-document statistics
  if(is.null(variable)) {
    voc <- rbind(totaltPerDoc,
                 uniquePerDoc, uniquePerDoc/totaltPerDoc*100,
                 hapaxPerDoc, hapaxPerDoc/totaltPerDoc*100,
                 totalwPerDoc)

    uniquet <- sum(col_sums(dtm) > 0)
    hapaxt <- sum(col_sums(dtm) == 1)

    voc <- cbind(voc, c(mean(totaltPerDoc),
                        mean(uniquePerDoc), mean(uniquePerDoc/totaltPerDoc, na.rm=TRUE)*100,
                        mean(hapaxPerDoc), mean(hapaxPerDoc/totaltPerDoc, na.rm=TRUE)*100,
                        mean(totalwPerDoc)),
                 c(sum(totaltPerDoc),
                   sum(uniquet), sum(uniquet)/sum(totaltPerDoc)*100,
                   sum(hapaxt), sum(hapaxt)/sum(totaltPerDoc)*100,
                   sum(totalwPerDoc)
                   ))

    colnames(voc)[c(ncol(voc)-1, ncol(voc))] <- c("Corpus mean", "Corpus total")
    lab <- ""
  }
  # Per-category statistics
  else if(unit == "document") {
    totalt <- tapply(totaltPerDoc, variable, mean)
    unique <- tapply(uniquePerDoc, variable, mean)
    hapax <- tapply(hapaxPerDoc, variable, mean)
    totalw <- tapply(totalwPerDoc, variable, mean)

    voc <- rbind(totalt, unique, unique/totalt*100,
                 hapax, hapax/totalt*100,
                 totalw)

    voc <- cbind(voc, c(mean(totalt),
                        mean(uniquePerDoc), mean(uniquePerDoc/totaltPerDoc, na.rm=TRUE)*100,
                        mean(hapaxPerDoc), mean(hapaxPerDoc/totaltPerDoc, na.rm=TRUE)*100,
                        mean(totalw)))

    colnames(voc)[ncol(voc)] <- "Corpus mean"
    lab <- "Per document mean:"
  }
  else {
    totalt <- row_sums(rollup(dtm, 1, variable, sum))
    unique <- row_sums(rollup(dtm, 1, variable, sum) > 0)
    hapax <- row_sums(rollup(dtm, 1, variable, sum) == 1)
    totalw <- tapply(totalwPerDoc, variable, sum)
    uniquet <- sum(col_sums(dtm) > 0)
    hapaxt <- sum(col_sums(dtm) == 1)

    voc <- rbind(totalt, unique, unique/totalt*100,
                 hapax, hapax/totalt*100,
                 totalw)

    voc <- cbind(voc, c(sum(totalt), uniquet, uniquet/sum(totalt)*100,
                        hapaxt, hapaxt/sum(totalt)*100,
                        sum(totalw)))
    colnames(voc)[ncol(voc)] <- "Corpus total"
    lab <- "Per category total:"
  }


  voc <- as.table(voc)
  rownames(voc) <- c("Number of terms",
                     "Number of unique terms",
                    "Percent of unique terms",
                     "Number of hapax legomena",
                     "Percent of hapax legomena",
                     "Number of words")


  names(dimnames(voc)) <- c(lab, "")

  voc
}


splitTexts <- function (corpus, chunksize, preserveMetadata=TRUE)
{
  chunks <- list(length(corpus))
  origins <- list(length(corpus))
  seqnum <- list(length(names))

  for (k in seq_along(corpus)) {
    paragraphs <- unlist(strsplit(content(corpus[[k]]), "\r\n|\n|\r"))
    chunks_k <- tapply(paragraphs,
                       rep(seq_along(paragraphs),
                           each=chunksize, length.out=length(paragraphs)), c)

    # Skeep empty chunks
    keep <- nchar(gsub("[\n[:space:][:punct:]]+", "", sapply(chunks_k, paste, collapse=""))) > 0

    chunks[[k]] <- chunks_k[keep]
    origins[[k]] <- rep(k, sum(keep))
    seqnum[[k]] <- seq(sum(keep))
  }

  # Merge only the per-document lists of chunks at the end to reduce the number of copies
  chunks <- do.call(c, chunks)
  origins <- do.call(c, origins)
  seqnum <- do.call(c, seqnum)

  ids <- names(corpus)
  newCorpus <- VCorpus(VectorSource(chunks))
  # Note: does not work for SimpleCorpus
  names(newCorpus) <- paste(ids[origins], seqnum)

  # Copy meta data from old documents
  if(preserveMetadata) {
    newCorpus$dmeta <- meta(corpus)[origins,, drop=FALSE]

    for(i in seq_along(corpus)) {
      attrs <- meta(corpus[[i]])

      for(j in which(origins == i)) {
        doc <- newCorpus[[j]]
        doc$meta <- attrs
        # Note: does not work for SimpleCorpus
        meta(doc, "id") <- paste(ids[i], seqnum[j])
        meta(doc, "document") <- ids[i]
        newCorpus[[j]] <- doc
      }
    }
  }

  meta(newCorpus, "Doc ID") <- ids[origins]
  meta(newCorpus, "Doc N") <- origins

  newCorpus
}
