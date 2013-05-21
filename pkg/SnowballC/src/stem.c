#include <stdio.h>
#include <stdlib.h> /* for malloc, free */
#include <string.h> 
#include <ctype.h>  /* for isupper, tolower */

#include <R.h>
#include <Rdefines.h>

#include "libstemmer.h"


static const sb_symbol *
stemString(struct sb_stemmer * stemmer, const char * str)
{
  const sb_symbol * stemmed = sb_stemmer_stem(stemmer, (sb_symbol *)str, strlen(str));

  if (stemmed == NULL)
      error("out of memory");

  return(stemmed);
}

SEXP
R_stemWords(SEXP words, SEXP language)
{
    unsigned int i, n;
    const sb_symbol * s;
    struct sb_stemmer * stemmer;
    SEXP result;

    stemmer = sb_stemmer_new(CHAR(STRING_ELT(language, 0)), NULL);
    if (stemmer == 0) {
        error("language `%s' is not available for stemming\n", CHAR(STRING_ELT(language, 0)));
    }

    /* Iterate over the given words and stem each of them in turn, putting the result into the answer vector. */
    n = GET_LENGTH(words);
    result = PROTECT(NEW_CHARACTER(n));

    for(i = 0; i < n; i++) {
        s = stemString(stemmer, translateCharUTF8(STRING_ELT(words, i)));
        SET_STRING_ELT(result, i, COPY_TO_USER_STRING((char *) s));
    }

    sb_stemmer_delete(stemmer);

    UNPROTECT(1);

    return(result);
}

SEXP
R_getStemLanguages()
{
    unsigned int i, n;
    const char ** list = sb_stemmer_list();
    SEXP result;

    for(n = 0; list[n] != NULL; n++);

    result = PROTECT(NEW_CHARACTER(n));

    for(i = 0; i < n; i++) {
        SET_STRING_ELT(result, i, COPY_TO_USER_STRING(list[i]));
    }

    UNPROTECT(1);

    return(result);
}
