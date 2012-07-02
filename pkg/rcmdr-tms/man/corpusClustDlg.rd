\name{corpusClustDlg}
\alias{corpusClustDlg}
\title{Hierarchical clustering of a tm corpus}
\description{Hierarchical clustering of the documents of a tm corpus.}

\details{This dialog allows creating a tree of the documents present in a \pkg{tm} corpus
         based on its document-term matrix. This tree starts with all separate documents
         at the bottom, and progressively merges them into clusters until reaching a single
         group at the top.

         Technically, it uses Ward's minimum variance method with an euclidean distance as
         computed by \code{\link{dist}}. See \code{\link{hclust}} for details about the
         clustering process.

         The slider ('sparsity') allows skipping less significant terms to use less memory
         with large corpora.

         Since the clustering by itself only returns a tree, cutting it at a given size is
         needed to create classes of documents: this can be achieved thanks to the Text
         Mining->Hierarchical clustering->Create classes... dialog.
        }

\seealso{\code{\link{hclust}}, \code{\link{dist}}, \code{\link{removeSparseTerms}},
         \code{\link{DocumentTermMatrix}}, \code{\link{createClassesDlg}} }
