\name{importCorpusDlg}
\alias{importCorpusDlg}
\alias{importCorpusFromDir}
\alias{importCorpusFromFile}
\alias{importCorpusFromFactiva}
\alias{importCorpusFromLexisNexis}
\alias{importCorpusFromEuropresse}
\alias{importCorpusFromAlceste}
\alias{importCorpusFromTwitter}
\alias{editDictionary}
\alias{splitTexts}
\alias{extractMetadata}
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
         Spreadsheet (.ods), comma-separated values (.csv) or tab-separated values (.tsv, .txt,
         .dat) file. One column must be specified as containing the text of the document, while the
         remaining columns are added as variables describing each document. For the CSV format,
         \dQuote{,} or \dQuote{;} is used as separator, whichever is the most frequent in the
         50 first lines of the file.

         The third, fourth and fifth sources, \dQuote{Factiva XML or HTML file(s)},
         \dQuote{LexisNexis HTML file(s)} and \dQuote{Europresse HTML file(s)}, load articles
         exported from the corresponding website in the \acronym{XML} or \acronym{HTML} formats
         (for Factiva, the former is recommended if you can choose it). Various meta-data variables
         describing the articles are automatically extracted. If the corpus is split into several .xml
         or .html files, you  can put them in the same directory and select them by holding the Ctrl
         key to concatenate them into a single corpus. Please note that some articles from Factiva
         are known to contain invalid character that trigger an error when loading. If this problem
         happens to you, please try to identify the problematic article, for example by removing half
         of the documents and retrying, until only one document is left in the corpus; then, report
         the problem to the Factiva Customer Service, or ask for help to the maintainers of the
         present package.

         The sixth source, \dQuote{Alceste file(s)}, loads texts and variables from a single file
         in the Alceste format, which uses asterisks to separate texts and code variables.

         The seventh source, \dQuote{Twitter search}, retrieves most recent tweets matching the search
         query and written in the specified language, up to the chosen maximum number of messages.
         Please note that you need to register a custom application and fill in the needed information
         to authenticate with the Twitter API (see \code{vignette("twitteR")} about OAuth authentication
         and \url{https://apps.twitter.com} to register a new application).
         Due to limitations imposed by Twitter, only tweets published up to 6 or 9 days ago can be
         downloaded, and up to a maximum number of 1500 tweets. Search queries can notably include
         one or more terms that must be present together for a tweet to match the query, and/or of
         hashtags  starting with \dQuote{#}; see \url{https://dev.twitter.com/docs/using-search} if
         you need more complex search strings. User names, hashtags, URLs and \dQuote{RT} (re-tweet)
         mentions are automatically removed from the corpus when computing the document-term matrix
         as they generally disturb the analysis. If the option to remove user names and hashtags is
         disabled, they will be included as standard text, i.e. \dQuote{#} and \dQuote{@} will be
         removed if the punctuation removal processing option has been enabled. The \dQuote{Exclude
         retweets} option works by identifying tweets that contain \dQuote{RT} as a separate expression;
         this operation can also be carried out manually later by using the \dQuote{Retweet} corpus
         variable that is created automatically at import time.

         The original texts can optionally be split into smaller chunks, which will then be
         considered as the real unit (called \sQuote{documents}) for all analyses. In order
         to get meaningful chunks, texts are only splitted into paragraphs. These are defined
         by the import filter: when importing a directory of text files, a new paragraph
         starts with a line break; when importing a Factiva files, paragraphs are defined
         by the content provider itself, so may vary in size (heading is always a separate
         paragraph); splitting has no effect when importing from a spreadsheet file. A corpus
         variable called \dQuote{Document} is created, which identifies the original text
         the chunk comes from.

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
         language used in the corpus. If you tick \dQuote{Edit stemming manually},
         enabled processing steps will be applied to the terms before presenting you with
         a list of all words originally found in the corpus, together with their stemmed forms.
         Terms with an empty stemmed form will be excluded from the document-term matrix;
         the \dQuote{Stopword} column is only presented as an indication, it is not taken into
         account when deciding whether to keep a term.

         By default, the program tries to detect the encoding used by plain text (usually .txt)
         and comma/tab-separated values files (.csv, .tsv, .dat...). If importation fails or
         the imported texts contain strange characters, specify the encoding manually (a tooltip
         gives suggestions based on the selected language).

         Once the corpus has been imported, its document-term matrix is extracted.
}
\references{Ingo Feinerer, Kurt Hornik, and David Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. Available at \url{http://www.jstatsoft.org/v25/i05}.\cr\cr
            Ingo Feinerer. An introduction to text mining in R. R News, 8(2):19-22, October 2008. Available at \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-2.pdf}}

\seealso{\code{\link{Corpus}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{setCorpusVariables}}, \code{\link{tolower}}, \code{\link{removePunctuation}},
         \code{\link{removeNumbers}}, \code{\link{stopwords}},  \code{\link{stemDocument}},
         \code{\link{tm_map}} }
