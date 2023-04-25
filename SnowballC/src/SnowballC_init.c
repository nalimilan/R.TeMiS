#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP R_getStemLanguages(void);
extern SEXP R_stemWords(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_getStemLanguages", (DL_FUNC) &R_getStemLanguages, 0},
    {"R_stemWords",        (DL_FUNC) &R_stemWords,        2},
    {NULL, NULL, 0}
};

void R_init_SnowballC(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

