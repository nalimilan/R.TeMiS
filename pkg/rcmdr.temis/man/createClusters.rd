\name{createClustersDlg}
\alias{createClustersDlg}
\alias{showCorpusClustering}
\title{Cut hierarchical clustering tree into clusters}
\description{Cut a hierarchical clustering tree into clusters of documents.}
\details{This dialog allows grouping the documents present in a \pkg{tm} corpus
         according to a previously computed hierarchical clustering tree (see
         \code{\link{corpusClustDlg}}). It adds a new meta-data variable to the corpus,
         each number corresponding to a cluster; this variable is also added to the corpusMetaData
         data set. If clusters were already created before, they are simply replaced.

         Clusters will be created by starting from the top of the dendrogram, and going through
         the merge points with the highest position until the requested number of branches is reached.

         A window opens to summarize created clusters, providing information about specific documents
         and terms for each cluster. Specific terms are those with the highest Chi-squared statistic
         in the document-term matrix collapsed so that the rows contain for each term the sum of its
         occurrences in the cluster's documents. Thus, the Chi-squared value reflects the deviation of the
         term from what would be expected if vocabulary was homogeneous accross all clusters. Terms that are
         negatively associated with the cluster are signalled by a negative Chi-squared value: this is just a
         printing convention, as by definition Chi-squared values can only be positive. Two statistics are also
         provided: the \dQuote{Prevalence} of the term in the cluster, i.e. the ratio of the term's occurrences
         on the total number of terms appearing in the cluster; the \dQuote{Distribution} of occurrences of the
         term in the cluster, i.e. the share of occurrences that appear in the considered cluster on the total
         number of occurrences of the term.

         Specific documents are selected using a different criterion than terms: documents with the smaller
         Chi-squared distance to the average vocabulary of the cluster are shown. This is a euclidean distance,
         but weighted by the inverse of the prevalence of each term in the whole corpus, and controlling for
         the documents' different lengths.

         This dialog can only be used after having created a tree, which is done via the Text
         Mining->Hierarchical clustering->Create dendrogram... dialog.
        }

\seealso{\code{\link{corpusClustDlg}}, \code{\link{cutree}}, \code{\link{hclust}}, \code{\link{dendrogram}} }
