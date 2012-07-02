\name{importCorpusDlg}
\alias{importCorpusDlg}
\alias{importCorpusFromDir}
\alias{importCorpusFromFile}
\alias{importCorpusFromFactiva}
\title{Import a corpus and process it}
\description{Import a corpus, process it and extract a document-term matrix.}
\details{This dialog allows creating a \pkg{tm} corpus from various sources. Once the
         documents have been loaded, they are processed according to the chosen settings,
         and a document-term matrix is extracted.

         The first source, \dQuote{Directory containing plain text files}, creates one
         document for each .txt file found in the specified directory. The documents
         are named according to the name of the file they were loaded from. When choosing
         the directoty where the .txt files can be found, please note that files are not
         listed in the file browser, only directories, but they will be loaded nevertheless.

         The second source, \dQuote{Spreadsheet file}, creates one document for each row
         of a file containg tabular data, typically an Excel (.xls) or Open Document
         Spreadsheet (.ods), CSV (.csv) or TSV (.tsv) file. The first column is taken as
         the contents of the document, while the remaining columns are added as variables
         describing each document.

         The third source, \dQuote{Factiva XML file}, loads articles exported from
         the Dow Jones Factiva website in the \acronym{XML} format. Various meta-data
         describing the articles are automatically extracted. If the corpus is split into
         several .xml files, you can put them in the same directory and select them by holding
         the Ctrl key to concatenate them into a single corpus. Please note that some
         articles from Factiva are known to contain invalid character that trigger an error
         when loading. If this problem happens to you, please try to identify the problematic
         article, for example by removing half of the documents and retrying, until only one
         document is left in the corpus; then, report the problem to the Factiva Customer
         Service, or ask for help to the maintainers of the present package.

         For all sources, a data set called \code{corpusVariables} is created, with one row
         for each document in the corpus: it contains meta-data that could be extracted from
         the source, if any, and can be used to enter further meta-data about the corpus.
         This can also be done by importing an existing data set via the
         Data->Load data set or Data->Import data menus. Whatever way you choose, use the
         Text mining->Set corpus meta-data command after that to set or update the corpus's
         meta-data that will be used by later analyses (see \code{\link{setCorpusVariables}}).

         The dialog also provides a few processing options that will most likely be
         all run in order to get a meaningful set of terms from a text corpus.
         Among them, stopwords removal and stemming require you to select the
         language used in the corpus: at the moment supported languages are
         Danish (\sQuote{da}), Dutch (\sQuote{nl}), English (\sQuote{en}), Finnish (\sQuote{fi}),
         French (\sQuote{fr}), German (\sQuote{de}), Hungarian (\sQuote{hu}), Italian (\sQuote{it}),
         Norwegian (\sQuote{no}), Portuguese (\sQuote{pt}), Russian (\sQuote{ru}), Spanish (\sQuote{es}),
         and Swedish (\sQuote{sv}) - to specifify via their ISO 639 two-letter code.

         Once the corpus has been imported, its document-term matrix is extracted.
}
\references{Ingo Feinerer, Kurt Hornik, and David Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. Available at \url{http://www.jstatsoft.org/v25/i05}.\cr\cr
            Ingo Feinerer. An introduction to text mining in R. R News, 8(2):19-22, October 2008. Available at \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-2.pdf}}

\seealso{\code{\link{Corpus}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{setCorpusVariables}}, \code{\link{tolower}}, \code{\link{removePunctuation}},
         \code{\link{removeNumbers}}, \code{\link{stopwords}},  \code{\link{stemDocument}},
         \code{\link{tm_map}} }
