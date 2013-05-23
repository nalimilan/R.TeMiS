stemDocumentSnowballC <- function(x, language = "porter") UseMethod("stemDocumentSnowballC", x)
stemDocumentSnowballC.character <- function (x, language = "porter")
    SnowballC::wordStem(x, language = language)

stemDocumentSnowballC.PlainTextDocument <- function(x, language = Language(x))
{
    s <- unlist(lapply(x, function(x) paste(stemDocumentSnowballC.character(unlist(strsplit(x, "[[:blank:]]")), language), collapse = " ")))
    Content(x) <- if (is.character(s))
        s
    else ""
    x
}

