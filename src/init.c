
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _sentometrics_compute_df(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_sentometrics_compute_df", (DL_FUNC) &_sentometrics_compute_df, 3},
    {NULL, NULL, 0}
};

void R_init_sentometrics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

