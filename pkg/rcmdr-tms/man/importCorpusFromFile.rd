\name{importCorpusFromFile}
\alias{importCorpusFromFile}
\title{Import a text corpus from file}
\description{Import a text corpus from a file and process it.}
\details{This dialog allows creating a \pkg{tm} corpus, creating one document
         for each row in the chosen file. At the moment, only CSV files are
         supported (see \code{\link{read.csv}}).

         The first column of the file must contain the actual text of each document.
         Other columns are stored in the \code{corpusMetaData} data set, and set as
         the corpus's meta-data which is used in further analyses (see
         {\link{setCorpusMetadata}}). If you need to change this meta-data, you can
         simply edit this data set, or import an existing data set via the
         Data->Load data set or Data->Import data menus. Whatever way you choose, use
         the Text mining->Set corpus meta-data command after that to set the corpus's
         meta-data that will be used by later analyses (see \code{\link{setCorpusMetadata}}).

         See \code{\link{processCorpusDlg}} for documentation about the processing step that
         is run on the corpus after loading it.
}
\references{Ingo Feinerer, Kurt Hornik, and David Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. Available at \url{http://www.jstatsoft.org/v25/i05}.\cr\cr
            Ingo Feinerer. An introduction to text mining in R. R News, 8(2):19-22, October 2008. Available at \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-2.pdf}}

\seealso{\code{\link{processCorpusDlg}}, \code{\link{Corpus}}, \code{\link{setCorpusMetadata}} }
