\name{importCorpusFromDir}
\alias{importCorpusFromDir}
\title{Import a text corpus from directory}
\description{Import a text corpus from a directory and process it.}
\details{This dialog allows creating a \pkg{tm} corpus, creating one document
         for each .txt file found in the specified directory. The documents
         are named according to the name of the file they were loaded from.

         A stub data set called \code{corpusMetaData} is also created, with one row
         for each document in the corpus: it can be used to enter meta-data about the
         corpus. This can also be done by importing an existing data set via the
         Data->Load data set or Data->Import data menus. Whatever way you choose use,
         the Text mining->Set corpus meta-data command after that to set the corpus's
         meta-data that will be used by later analyses (see \code{\link{setCorpusMetadata}}).

         See \code{\link{processCorpusDlg}} for documentation about the processing step that
         is run on the corpus after loading it.
}
\references{Ingo Feinerer, Kurt Hornik, and David Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. Available at \url{http://www.jstatsoft.org/v25/i05}.\cr\cr
            Ingo Feinerer. An introduction to text mining in R. R News, 8(2):19-22, October 2008. Available at \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-2.pdf}}

\seealso{\code{\link{processCorpusDlg}}, \code{\link{Corpus}}, \code{\link{setCorpusMetadata}} }
