#' lexical_summary
#'
#' Build a lexical summary table, optionally over a variable.
#'
#' _Words_ are defined as the forms of two or more characters present in the texts
#' before stemming and stopword removal. On the contrary, unique _terms_ are extracted
#' from `dtm`, which means they do not include words that were removed from it, and that
#' words different in the original text might become identical terms if stemming was performed.
#' Please note that percentages for terms and words are computed with regard
#' respectively to the total number of terms and of words, so the denominators are not the
#' same for all measures.
#'
#' When `variable` is not `NULL`, `unit` defines two different ways of
#' aggregating per-document statistics into per-category measures:
#' - "document": values computed for each document are simply averaged for each category.
#' - "global": values are computed for each category taken as a whole: word counts are summed
#' for each category, and ratios and averages are calculated for this level only, from
#' the summed counts.
#'
#' This distinction does not make sense when `variable=NULL`: in this case, "level"
#' in the above explanation corresponds to "document", and two columns are provided about
#' the whole corpus.
#' - "Corpus mean" is simply the average value of measures over all documents
#' - "Corpus total" is the sum of the number of terms, the percentage of terms (ratio of
#' the summed numbers of terms) and the average word length in the corpus when taken as a
#' single document.
#'
#' @param dtm A `DocumentTermMatrix` containing the terms to summarize,
#'   which may have been stemmed.
#' @param corpus A `Corpus` object containing the original texts from which
#'   `dtm` was constructed.
#' @param variable An optional vector with one element per document indicating to which
#'    category it belongs. If `NULL, per-document measures are returned.
#' @param unit When `variable` is not `NULL`, defines the way measures are aggregated
#'   (see below).
#'
#' @return A `table` object with the following information for each document or
#'   each category of documents in the corpus:
#'  - total number of terms
#'  - number and percent of unique terms (i.e. appearing at least once)
#'   number and percent of hapax legomena (i.e. terms appearing once and only once)
#'  - total number of words
#'  - number and percent of long words (defined as at least seven characters)
#'  - number and percent of very long words (defined as at least ten characters)
#'  - average word length
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' lexical_summary(dtm, corpus)
#'
#' @export
lexical_summary <- function(dtm, corpus, variable=NULL, unit=c("document", "global")) {
    # Order of arguments is reversed compared with most functions
    if(!inherits(dtm, "DocumentTermMatrix"))
        stop("`dtm` must be a DocumentTermMatrix")
    
    unit <- match.arg(unit)
    
    if(is.null(variable) || unit == "document") {
        totaltPerDoc <- row_sums(dtm)
        uniquePerDoc <- row_sums(dtm > 0)
        hapaxPerDoc <- row_sums(dtm == 1)
    }

    if(!isTRUE(all.equal(names(corpus), rownames(dtm))))
        stop(.gettext("`dtm` must have one row per document in `corpus`, with the same names and in the same order."))
        
    wordsDtm <- build_dtm(corpus)
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
        
        colnames(voc)[c(ncol(voc)-1, ncol(voc))] <- c(.gettext("Corpus mean"), .gettext("Corpus total"))
        lab <- ""
    }
    # Per-category statistics
    else if(unit == "document") {
        var <- meta(corpus)[[variable]]
        totalt <- tapply(totaltPerDoc, var, mean)
        unique <- tapply(uniquePerDoc, var, mean)
        hapax <- tapply(hapaxPerDoc, var, mean)
        totalw <- tapply(totalwPerDoc, var, mean)
        
        voc <- rbind(totalt, unique, unique/totalt*100,
                     hapax, hapax/totalt*100,
                     totalw)
        
        voc <- cbind(voc, c(mean(totalt),
                            mean(uniquePerDoc), mean(uniquePerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(hapaxPerDoc), mean(hapaxPerDoc/totaltPerDoc, na.rm=TRUE)*100,
                            mean(totalw)))
        
        colnames(voc)[ncol(voc)] <- .gettext("Corpus mean")
        lab <- .gettext("Per document mean:")
    }
    else {
        var <- meta(corpus)[[variable]]
        totalt <- row_sums(rollup(dtm, 1, var, sum))
        unique <- row_sums(rollup(dtm, 1, var, sum) > 0)
        hapax <- row_sums(rollup(dtm, 1, var, sum) == 1)
                          totalw <- tapply(totalwPerDoc, var, sum)
                          uniquet <- sum(col_sums(dtm) > 0)
                          hapaxt <- sum(col_sums(dtm) == 1)
                          
                          voc <- rbind(totalt, unique, unique/totalt*100,
                                       hapax, hapax/totalt*100,
                                       totalw)
                          
                          voc <- cbind(voc, c(sum(totalt), uniquet, uniquet/sum(totalt)*100,
                                              hapaxt, hapaxt/sum(totalt)*100,
                                              sum(totalw)))
                          colnames(voc)[ncol(voc)] <- .gettext("Corpus total")
                          lab <- .gettext("Per category total:")
    }
    
    voc <- as.table(voc)
    rownames(voc) <- c(.gettext("Number of terms"),
                       .gettext("Number of unique terms"),
                       .gettext("Percent of unique terms"),
                       .gettext("Number of hapax legomena"),
                       .gettext("Percent of hapax legomena"),
                       .gettext("Number of words"))
    
    
    names(dimnames(voc)) <- c(lab, "")
    
    voc
}