\name{corpusClustDlg}
\alias{corpusClustDlg}
\title{Hierarchical clustering of a tm corpus}
\description{Hierarchical clustering of the documents of a tm corpus.}

\details{This dialog allows creating a tree of the documents present in a \pkg{tm} corpus
         either based on its document-term matrix, or on selected dimensions of a previously
         run correspondence analysis (if no correspondence analysis has been performed, the
         relevant widgets are not available). With both methods, the dendrogram starts with
         all separate documents at the bottom, and progressively merges them into clusters
         until reaching a single group at the top.

         Technically, Ward's minimum variance method is used with a Chi-squared distance: see
         \code{\link{hclust}} for details about the clustering process.

         The first slider allows skipping less significant terms to use less memory with large
         corpora. The second allows choosing what dimensions of the correspondence analysis
         should be used, which helps removing noise to concentrate on identified caracteristics
         of the corpus.

         Since the clustering by itself only returns a tree, cutting it at a given size is
         needed to create classes of documents: this is offered automatically after the dendrogram
         has been computed, and can be achieved as many times as needed thanks to the Text
         Mining->Hierarchical clustering->Create clusters... dialog.
        }

\seealso{\code{\link{hclust}}, \code{\link{dist}}, \code{\link{corpusCaDlg}}, \code{\link[tm]{removeSparseTerms}},
         \code{\link[tm]{DocumentTermMatrix}}, \code{\link{createClustersDlg}} }
