// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// predict_cpp
NumericVector predict_cpp(DataFrame x, DataFrame is_na, IntegerVector roots, IntegerVector yes, IntegerVector no, IntegerVector missing, LogicalVector is_leaf, IntegerVector feature, NumericVector split, IntegerVector decision_type, NumericVector value);
RcppExport SEXP _cheem_predict_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP is_leafSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type is_na(is_naSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type no(noSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type missing(missingSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type is_leaf(is_leafSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type split(splitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type decision_type(decision_typeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_cpp(x, is_na, roots, yes, no, missing, is_leaf, feature, split, decision_type, value));
    return rcpp_result_gen;
END_RCPP
}
// new_covers
IntegerVector new_covers(DataFrame x, DataFrame is_na, IntegerVector roots, IntegerVector yes, IntegerVector no, IntegerVector missing, LogicalVector is_leaf, IntegerVector feature, NumericVector split, IntegerVector decision_type);
RcppExport SEXP _cheem_new_covers(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP is_leafSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type is_na(is_naSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type no(noSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type missing(missingSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type is_leaf(is_leafSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type split(splitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type decision_type(decision_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(new_covers(x, is_na, roots, yes, no, missing, is_leaf, feature, split, decision_type));
    return rcpp_result_gen;
END_RCPP
}
// treeshap_cpp
NumericVector treeshap_cpp(DataFrame x, DataFrame is_na, IntegerVector roots, IntegerVector yes, IntegerVector no, IntegerVector missing, IntegerVector feature, NumericVector split, IntegerVector decision_type, LogicalVector is_leaf, NumericVector value, NumericVector cover, bool verbose);
RcppExport SEXP _cheem_treeshap_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP is_leafSEXP, SEXP valueSEXP, SEXP coverSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type is_na(is_naSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type no(noSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type missing(missingSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type split(splitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type decision_type(decision_typeSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type is_leaf(is_leafSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type value(valueSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cover(coverSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(treeshap_cpp(x, is_na, roots, yes, no, missing, feature, split, decision_type, is_leaf, value, cover, verbose));
    return rcpp_result_gen;
END_RCPP
}
// treeshap_interactions_cpp
List treeshap_interactions_cpp(DataFrame x, DataFrame is_na, IntegerVector roots, IntegerVector yes, IntegerVector no, IntegerVector missing, IntegerVector feature, NumericVector split, IntegerVector decision_type, LogicalVector is_leaf, NumericVector value, NumericVector cover, bool verbose);
RcppExport SEXP _cheem_treeshap_interactions_cpp(SEXP xSEXP, SEXP is_naSEXP, SEXP rootsSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP missingSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP decision_typeSEXP, SEXP is_leafSEXP, SEXP valueSEXP, SEXP coverSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type is_na(is_naSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type roots(rootsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type no(noSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type missing(missingSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type split(splitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type decision_type(decision_typeSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type is_leaf(is_leafSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type value(valueSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cover(coverSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(treeshap_interactions_cpp(x, is_na, roots, yes, no, missing, feature, split, decision_type, is_leaf, value, cover, verbose));
    return rcpp_result_gen;
END_RCPP
}
