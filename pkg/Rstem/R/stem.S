getTestedLanguages =
function()
{
  .TestedLanguages
}

wordStem =
  #
  # Do the stemming.
  #
  #
  # Does this handle upper case? Yes.
  #
function(words, language = character(), warnTested = FALSE)
{
   # Check the language is okay.
 if(length(language)) {
   if(length(language) == 1) {
     langs = getStemLanguages()
     idx = pmatch(language, langs)
     if(is.na(idx))
       stop("No such language: ", language, ".  See the documentation for wordStem() and getStemLanguages().")

     language = langs[idx]

     if(warnTested && !(language %in% getTestedLanguages()))
       warning("Currently, ", language, " is not tested. It may work, but you will need support for UTF characters.")
   } else {

      if(!is.character(language)) {
        if(!is.list(language)  && !all(sapply(language, function(x) inherits(x, "NativeSymbol"))))
          stop("Require two native symbols or names of native symbols")
      } else {
        language = lapply(language, function(x) getNativeSymbolInfo(x)$address)
      }
   }
 } 
   
 lens = nchar(words, "bytes")

   # Need to synchronize this with the C code, so ask it..
 lim = .Call("S_getMaxWordLength", PACKAGE = "Rstem")
 if(any(lens > lim))
   stop("There is a limit of ",  lim, "characters on the number of characters in a word being stemmed")
 
 .Call("S_stemWords", words, lens, language, PACKAGE="Rstem")
}  


getStemLanguages =
  #
  # Get the names of the supported languages from the C code.
  #
function()
{
 .Call("S_getLanguages", PACKAGE = "Rstem")
}  
