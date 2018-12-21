#' import_corpus
#'
#' Import a corpus from a file.
#'
#' @param paths Path to one of more files, or to a directory (if `format="txt"`) to import.
#' @param format File format: can be `"csv"`, `"txt"`, `"factiva"`, `"europresse"`,
#'   `"lexisnexis"` or `"alceste"`.
#' @param language The language name or code (preferably as IETF language tags,
#'   see [`language`][NLP::language]) to be used in particular for stopwords and stemming.
#'
#' @param textcolumn When `format="csv"`, the column containing the text, either
#'  as a string or as a position
#' @param encoding The character encoding of the file, or `NULL` to attempt automatic detection.
#'
#' @return A `Corpus` object.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' import_corpus(file, "factiva", language="en")
#'
#' @import NLP
#' @import graphics
#' @import slam
#' @import tm
#' @import stats
#' @import utils
#' @import stringi
#' @import FactoMineR
#' @import explor
#' 
#' @export
import_corpus <- function(paths, format, language, textcolumn=1, encoding=NULL) {
  format <- match.arg(format, c("txt", "csv", "factiva", "europresse", "lexisnexis", "alceste"))

  if(is.null(encoding)) {
      if(format == "txt")
          files <- list.files(paths, full.names=TRUE)
      else
          files <- paths

      encs <- table(sapply(files,
                           function(f) stri_enc_detect(readBin(f, "raw", 50000))[[1]]$Encoding[1]))
      encoding <- names(encs)[order(encs, decreasing=TRUE)][1]

      if(is.null(encoding))
          ""
      else
          encoding
  }

  if(format == "txt") {
    corpus <- VCorpus(DirSource(paths, encoding=encoding),
                      readerControl=list(language=language))
  }
  else if(format == "csv") {
    # Try to guess the separator from the most common character of ; and ,
    # This should work in all cases where text is not too long
    excerpt <- readLines(con <- file(paths, encoding=encoding), 50)
    close(con)
    n1 <- sum(sapply(gregexpr(",", excerpt), length))
    n2 <- sum(sapply(gregexpr(";", excerpt), length))

    if(n1 > n2)
      corpusDataset <- read.csv(paths, fileEncoding=encoding, stringsAsFactors=FALSE)
    else
      corpusDataset <- read.csv2(paths, fileEncoding=encoding, stringsAsFactors=FALSE)

    corpus <- VCorpus(DataframeSource(data.frame(doc_id=rownames(corpusDataset),
                                                 text=corpusDataset[[textcolumn]])),
                      readerControl=list(language))

    for(i in names(corpusDataset)[-textcolumn])
      meta(corpus,i) <- corpusDataset[[i]]
  }
  else if(format == "factiva") {
      corpus <- import_files(tm.plugin.factiva::FactivaSource, paths, language, encoding)
  }
  else if(format == "europresse") {
      corpus <- import_files(tm.plugin.europresse::EuropresseSource, paths, language, encoding)
      
  }
  else if(format == "lexisnexis") {
      corpus <- import_files(tm.plugin.lexisnexis::LexisNexisSource, paths, language, encoding)
  }
  else if(format == "alceste") {
      corpus <- import_files(tm.plugin.alceste::AlcesteSource, paths, language, encoding)
  }

  # make.names ensures that names are unique and
  # that they will not be modified by corpus_ca (to work around a bug in explor)
  names(corpus) <- make.names(names(corpus))
  meta(corpus, type="corpus", tag="language") <- language

  corpus
}

import_files <- function(f, paths, language, encoding) {
    if(length(paths) == 1) {
        corpus <- VCorpus(f(paths, encoding=encoding), readerControl=list(language=language))
    }
    else {
        corpusList <- lapply(paths, function(path)
                             VCorpus(f(path, encoding=encoding),
                                     readerControl=list(language=language)))
        corpus <- do.call(c, c(corpusList, list(recursive=TRUE)))
    }
    corpus <- set_corpus_variables(corpus, extract_metadata(corpus))
    corpus
}

#' split_documents
#'
#' Split documents in a corpus into documents of one of more paragraphs.
#'
#' @param corpus A `Corpus` object.
#' @param chunksize The number of paragraphs each new document should contain at most.
#' @param preserveMetadata Whether to preserve the meta-data of original documents.
#'
#' @return A `Corpus` object with split documents.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' split_documents(corpus, 3)
#'
#' @export
split_documents <- function(corpus, chunksize, preserveMetadata=TRUE)
{
  chunks <- list(length(corpus))
  origins <- list(length(corpus))
  seqnum <- list(length(names))

  for (k in seq_along(corpus)) {
    paragraphs <- unlist(strsplit(content(corpus[[k]]), "\r\n|\n|\r"))
    chunks_k <- tapply(paragraphs,
                       rep(seq_along(paragraphs),
                           each=chunksize, length.out=length(paragraphs)), c)

    # Skip empty chunks
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
  names(newCorpus) <- paste(ids[origins], seqnum, sep=".")

  # Copy meta data from old documents
  if(preserveMetadata) {
    newCorpus$dmeta <- meta(corpus)[origins,, drop=FALSE]

    for(i in seq_along(corpus)) {
      attrs <- meta(corpus[[i]])

      for(j in which(origins == i)) {
        doc <- newCorpus[[j]]
        doc$meta <- attrs
        # Note: does not work for SimpleCorpus
        meta(doc, "id") <- paste(ids[i], seqnum[j], sep=".")
        meta(doc, "document") <- ids[i]
        newCorpus[[j]] <- doc
      }
    }
  }

  meta(newCorpus, "doc_id") <- ids[origins]
  meta(newCorpus, "doc_n") <- origins
  meta(newCorpus, type="corpus", tag="split") <- TRUE
  meta(newCorpus, type="corpus", tag="language") <- meta(corpus, type="corpus", tag="language")

  newCorpus
}

#' set_corpus_variables
#'
#' Set corpus meta-data variables from a data frame.
#'
#' @param corpus A `Corpus` object.
#' @param dset A `data.frame` containing meta-data variables, with one row per document in `corpus.
#'
#' @return A `Corpus` object with meta-data added.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' dset <- data.frame(x=1:length(corpus))
#' corpus <- set_corpus_variables(corpus, dset)
#'
#' @export
set_corpus_variables <- function(corpus, dset) {
  # If corpus was split, we need to replicate variables
  split <- isTRUE(meta(corpus, type="corpus", tag="split"))

  len <- if (split) length(unique(meta(corpus, "doc_n")[[1]])) else length(corpus)

  # Remove dropped and empty variables
  for(var in colnames(meta(corpus))[!colnames(meta(corpus)) %in%
                                    c(colnames(dset), "doc_n", "doc_id", "cluster",
                                      sapply(dset, function(x) all(is.na(x) | x == "")))])
    meta(corpus, var) <- NULL

  # Add new variables
  indices <- which(sapply(dset, function(x) !all(is.na(x) | x == "", na.rm=TRUE)))

  # Drop empty levels, which notably happen when changing values manually
  if(length(indices) > 0) {
    if(split) {
      for(i in indices)
        meta(corpus, colnames(dset)[i]) <- droplevels(factor(dset[meta(corpus, "doc_n")[[1]], i]))
    }
    else {
      for(i in indices)
        meta(corpus, colnames(dset)[i]) <- droplevels(factor(dset[[i]]))
    }
  }

  # Update names only if they changed
  oldDocNames <- if(split) unique(meta(corpus, "doc_id")[[1]]) else names(corpus)
  corpusNames <- names(corpus)
  if(!identical(oldDocNames, row.names(dset))) {
    if(split) {
      # Note: does not work for SimpleCorpus
      names(corpus) <- make.unique(make.names(row.names(dset)[meta(corpus, "doc_n")[[1]]]))
      meta(corpus, "Doc ID") <- row.names(dset)[meta(corpus, "doc_n")[[1]]]
    }
    else {
      # Note: does not work for SimpleCorpus
      names(corpus) <- make.unique(make.names(row.names(dset)))
    }
  }
  return(corpus)
}

extract_metadata <- function(corpus, date=TRUE) {
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
  colnames(vars)[colnames(vars) == "origin"] <- .gettext("Origin")
  colnames(vars)[colnames(vars) == "date"] <- .gettext("Date")
  colnames(vars)[colnames(vars) == "author"] <- .gettext("Author")
  colnames(vars)[colnames(vars) == "section"] <- .gettext("Section")
  colnames(vars)[colnames(vars) == "type"] <- .gettext("Type")
  colnames(vars)[colnames(vars) == "edition"] <- .gettext("Edition")
  colnames(vars)[colnames(vars) == "wordcount"] <- .gettext("Word.Count")
  colnames(vars)[colnames(vars) == "pages"] <- .gettext("Pages")
  colnames(vars)[colnames(vars) == "publisher"] <- .gettext("Publisher")
  colnames(vars)[colnames(vars) == "rights"] <- .gettext("Rights")

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

#' concordances
#'
#' Print documents which contain one or more terms and return
#' a sub-corpus with these documents.
#'
#' Occurrences of the specified terms are highlighted. If stemming
#' or other transformations have been applied to original words using
#' [`combine_terms`], all original words which have been transformed
#' to the specified terms are highlighted.
#'
#' @param corpus A `Corpus` object.
#' @param dtm A `DocumentTermMatrix` object corresponding to `corpus`.
#' @param terms One of more terms appearing in `dtm`.
#' @param all Whether only documents containing all terms should be printed.
#'   By default, documents need to contain at least one of the terms.
#'
#' @return `Corpus` object (invisibly).
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' concordances(corpus, dtm, "oil")
#' concordances(corpus, dtm, c("oil", "opec"))
#' concordances(corpus, dtm, c("oil", "opec"), all=TRUE)
#'
#' # Also works when terms have been combined
#' dict <- dictionary(dtm)
#' dtm2 <- combine_terms(dtm, dict)
#' concordances(corpus, dtm2, "product")
#'
#' @export
concordances <- function(corpus, dtm, terms, all=FALSE) {
  if(!isTRUE(all.equal(names(corpus), rownames(dtm))))
    stop(.gettext("`dtm` must have one row per document in `corpus`, with the same names and in the same order."))

  if(!all(terms %in% colnames(dtm)))
    stop(.gettext(sprintf("term \"%s\" is not in `dtm`", setdiff(terms, colnames(dtm))[1])))

  if(all)
    corpus <- corpus[row_sums(dtm[, terms] == 0) == 0]
  else
    corpus <- corpus[row_sums(dtm[, terms]) > 0]

  dict <- attr(dtm, "dict")
  if(is.null(dict))
    words <- terms
  else
    words <- rownames(dict)[dict[[.gettext("Term")]] %in% terms]

  # Used mostly to benefit from detection of terminal support
  color <- crayon::red(crayon::bold("X"))
  for(i in seq_along(corpus)) {
    cat(crayon::bold(names(corpus)[i]), "\n")
    cat(gsub(sprintf("\\b(%s)\\b",
                     paste(words, collapse="|")),
             sub("X", "\\\\1", color),
             as.character(corpus[[i]]),
             ignore.case=TRUE))
    cat("\n")
  }
  invisible(corpus)
}

#' subset_corpus
#'
#' Select documents containing (or not containing) one or more terms.
#'
#' @param corpus A `Corpus` object.
#' @param dtm A `DocumentTermMatrix` object corresponding to `corpus`.
#' @param terms One of more terms appearing in `dtm`.
#' @param exclude Whether documents containing the terms should be excluded rather than retained.
#' @param all Whether only documents containing all terms should be retained or excluded.
#'   By default, documents need to contain at least one of the terms.
#'
#' @return `Corpus` object.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' subset_corpus(corpus, dtm, "barrel")
#' subset_corpus(corpus, dtm, c("barrel", "opec"))
#' subset_corpus(corpus, dtm, c("barrel", "opec"), exclude=TRUE)
#' subset_corpus(corpus, dtm, c("barrel", "opec"), all=TRUE)
#'
#' @export
subset_corpus <- function(corpus, dtm, terms, exclude=FALSE, all=FALSE) {
    if(exclude) {
        if(all)  {
            corpus[row_sums(dtm[, terms]) == 0]
        }
        else {
            corpus[row_sums(dtm[, terms] > 0) == 0]
        }
    }
    else {
        if(all)  {
            corpus[row_sums(dtm[, terms] == 0) == 0]
        }
        else {
            corpus[row_sums(dtm[, terms]) > 0]
        }
    }
}