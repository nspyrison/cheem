// This file was automatically generated by 'Kmisc::registerFunctions()'

#include <R.h>
#include <Rinternals.h>

#include <R_ext/Rdynload.h>

SEXP _cheem_predict_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP is_leafSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP valueSEXP);
SEXP _cheem_new_covers(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP is_leafSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP);
SEXP _cheem_treeshap_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP is_leafSEXP, SEXP valueSEXP, SEXP coverSEXP, SEXP verboseSEXP);
SEXP _cheem_treeshap_interactions_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP is_leafSEXP, SEXP valueSEXP, SEXP coverSEXP, SEXP verboseSEXP);
void R_init_cheem(DllInfo *dll);

R_CallMethodDef callMethods[]  = {
  {"_cheem_predict_cpp", (DL_FUNC) &_cheem_predict_cpp, 11},
  {"_cheem_new_covers", (DL_FUNC) &_cheem_new_covers, 10},
  {"_cheem_treeshap_cpp", (DL_FUNC) &_cheem_treeshap_cpp, 13},
  {"_cheem_treeshap_interactions_cpp", (DL_FUNC) &_cheem_treeshap_interactions_cpp, 13},
  {"R_init_cheem", (DL_FUNC) &R_init_cheem, 1},
  {NULL, NULL, 0}
};

void R_init_cheem(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

