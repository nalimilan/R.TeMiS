\name{createClustersDlg}
\alias{createClustersDlg}
\title{Cut hierarchical clustering tree into clusters}
\description{Cut a hierarchical clustering tree into clusters of documents.}

\details{This dialog allows grouping the documents present in a \pkg{tm} corpus
         according to a previously computed hierarchical clustering tree (see
         \code{\link{corpusClustDlg}}). It adds a new meta-data variable to the corpus,
         each number corresponding to a cluster; this variable is also added to the corpusMetaData
         data set. If clusters were created before, they are simply replaced.

         The slider allows choosing the height at which the tree will be cut. This value must be
         chosen according to the wanted number of clusters, which in turn depends on the aspect
         of the tree. Since plotting the full (uncut) tree doesn't give a readable result, it is
         advised to cut the tree at a low height first, and then raise the value after examining the
         partial plot (note that height is reported on the vertical axis).

         This dialog can only be used after having created a tree, which is done via the Text
         Mining->Hierarchical clustering->Create dendrogram... dialog.
        }

\seealso{\code{\link{corpusClustDlg}}, \code{\link{cutree}}, \code{\link{hclust}}, \code{\link{dendrogram}} }
