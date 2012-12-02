stemDocumentRstem <- function(x, language = "english") UseMethod("stemDocumentRstem", x)
stemDocumentRstem.character <- function (x, language = "english")
Rstem::wordStem(x, language = language)

stemDocumentRstem.PlainTextDocument <- function(x, language = tm:::map_IETF_Snowball(Language(x)))
{
    s <- unlist(lapply(x, function(x) paste(stemDocumentRstem.character(unlist(strsplit(x, "[[:blank:]]")), language), collapse = " ")))
    Content(x) <- if (is.character(s))
        s
    else ""
    x
}
