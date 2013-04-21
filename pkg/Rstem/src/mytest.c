#include "api.h"

#include <Rdefines.h>
#include <R_ext/Rdynload.h>

typedef struct SN_env SN_env;

typedef struct SN_env * (*CreateEnvFun)(void);
typedef void (*CloseEnvFun)(struct SN_env*);
typedef int (*StemFun)(struct SN_env *);

/* Structure for holding a specific language, given by its name and the create and close environment routines. */
typedef struct {
	char *language; 
	CreateEnvFun create;
	CloseEnvFun close;
	StemFun stem;
} StemLanguage;


/* Machine generated code to declare the different create and close environment routines
   and create an array of StemLanguage objects to list them for english, swedish, etc. */
#include "Languages.h"


static int getLanguageRoutine(SEXP language, CreateEnvFun *create, CloseEnvFun *close, StemFun *fun);
StemLanguage *findLanguage(const char *langName);


#define RSTEM_MAX_WORD_LENGTH 255

/*
 Do the stemming.
*/
SEXP
S_stemWords(SEXP words, SEXP lens, SEXP language)
{
	int i, n;
	struct SN_env *env;
	SEXP ans;

	symbol buf[RSTEM_MAX_WORD_LENGTH];

   	    /* Get the create and close routines for the appropriate language */
	CreateEnvFun create = &english_create_env;
	CloseEnvFun close = &english_close_env;
	StemFun stem = &english_stem;

  	    /* If the caller specified a language,  use that and do the appropriate lookups. */
        if(GET_LENGTH(language)) {
		getLanguageRoutine(language, &create, &close, &stem);
	}

        
	/* Create the environment. */
	env =  (*create)();

        if(!env) {
         PROBLEM "Cannot create a Snowball environment"
	 ERROR;
	}


	/* Iterate over the given words and stem each of them in turn, putting the result into the answer vector. */
	n = GET_LENGTH(words);
	PROTECT(ans = NEW_CHARACTER(n));
	for(i = 0; i < n; i++) {
                /* Do we need to do this? We will when we have Unicode, etc. */
		memcpy(buf, CHAR(STRING_ELT(words, i)), INTEGER_DATA(lens)[i]);
		SN_set_current(env, INTEGER_DATA(lens)[i], buf);

		(*stem)(env);
                /* Put into the answer vector. */
		memcpy(buf, env->p, env->l);
		buf[env->l] = '\0';
		SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(buf));
	}

         /* Clean up the Snowball environment, releasing the resources. */
	(*close)(env);
	UNPROTECT(1);

	return(ans);
}


/*
  Try to match the given name to the names of the languages for which we compiled
  stemming support.  If we find a match, return the entry. Otherwise, return NULL. 
 */
StemLanguage *
findLanguage(const char *langName)
{
	int i;

	for(i = 0; i < sizeof(languages)/sizeof(languages[0]); i++) {
		if(strcmp(langName, languages[i].language) == 0)
			return(&languages[i]);
	}
	return(NULL);
}

/* Return a character vector giving the names of the languages for which we compiled stemming support
   into the package*/
SEXP
S_getLanguages()
{
	SEXP ans;
	int i, n;

        n = sizeof(languages)/sizeof(languages[0]);
	PROTECT(ans = NEW_CHARACTER(n));
	for(i = 0; i < n; i++) {
		SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(languages[i].language));
	}
	UNPROTECT(1);
	return(ans);
}


/*
  Return the maximum word limit that we currently support.
 */
SEXP
S_getMaxWordLength()
{
	SEXP ans;
	ans = NEW_INTEGER(1);
	INTEGER_DATA(ans)[0] = RSTEM_MAX_WORD_LENGTH;
	return(ans);
}



/*
  Lookup and resolve the create and close routines for the given   language .
  If this is one of the builtin languages, we get the entry in the array
  of StemLanguage objects.  If not, we resolve the symbols dynamically.

  This should handle NativeSymbolInfo objects.
 */
static int
getLanguageRoutine(SEXP language, CreateEnvFun *create, CloseEnvFun *close, StemFun *stem)
{

	StemLanguage *lang;
	if(TYPEOF(language) == STRSXP) {
  	    lang = findLanguage(CHAR(STRING_ELT(language, 0)));
	    if(!lang) {
		    *create = (CreateEnvFun) R_FindSymbol(CHAR(STRING_ELT(language, 0)), "Rstem", NULL);
		    *close = (CloseEnvFun) R_FindSymbol(CHAR(STRING_ELT(language, 1)), "Rstem", NULL);
		    *stem = (StemFun) R_FindSymbol(CHAR(STRING_ELT(language, 2)), "Rstem", NULL);
/*		    fprintf(stderr, "Got dynamic symbols for language support\n");fflush(stderr); */
	    } else {
   	        *close = lang->close;
		*create = lang->create;
		*stem = lang->stem;
/*		fprintf(stderr, "Using builtin language %s", CHAR(STRING_ELT(language, 0))); fflush(stderr); */
	    }
	} else if(TYPEOF(language) == VECSXP) {
		SEXP el;
		if(TYPEOF(VECTOR_ELT(language, 0)) != EXTPTRSXP || TYPEOF(VECTOR_ELT(language, 1)) != EXTPTRSXP) {
			PROBLEM "both elements must be the addresses of native routines. See the function getNativeSymbolInfo()"
			ERROR;
		}
		el = VECTOR_ELT(language, 0);
		*create = (CreateEnvFun) R_ExternalPtrAddr(el);
		el = VECTOR_ELT(language, 1);
		*close = (CloseEnvFun) R_ExternalPtrAddr(el);
		el = VECTOR_ELT(language, 2);
		*stem = (StemFun) R_ExternalPtrAddr(el);

	}

	if(*create == NULL || *close == NULL || *stem == NULL) {
	    PROBLEM  "Couldn't find dynamic symbol(s) for stemming"
	    ERROR;
	}

	return(1);
}


/* These two routines are here to provide an example of how to use the dynamic lookup
   to get the the create and close routines for the Snowball environment.
   See the example for the wordStem function in S.
*/
SN_env*
testDynCreate()
{
	return(english_create_env());
}

void
testDynClose(SN_env *env)
{
	english_close_env(env);
}

int
testDynStem(SN_env *env)
{
	return(english_stem(env));
}



/*
 Initialization material for the package.
*/

static const R_CallMethodDef CallEntries[]  = {
	{"S_stemWords", (DL_FUNC) &S_stemWords, 3},
	{"S_getLanguages", (DL_FUNC) &S_getLanguages, 0},
	{"S_getMaxWordLength", (DL_FUNC) &S_getMaxWordLength, 0},
	{NULL, NULL, 0}
};


void R_init_Rstem(DllInfo *dll)
{
	/* Allow us to find symbols that we don't call directly from R but pass to other routines as
           pointers, e.g. testDynCreate and testDynClose. */
    R_useDynamicSymbols(dll, TRUE);	
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
}



