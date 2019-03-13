// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// parseEntry
Rcpp::List parseEntry(Rcpp::CharacterVector entryText);
RcppExport SEXP _exforParser_parseEntry(SEXP entryTextSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type entryText(entryTextSEXP);
    rcpp_result_gen = Rcpp::wrap(parseEntry(entryText));
    return rcpp_result_gen;
END_RCPP
}
// convToJSON
Rcpp::CharacterVector convToJSON(Rcpp::List myList);
RcppExport SEXP _exforParser_convToJSON(SEXP myListSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type myList(myListSEXP);
    rcpp_result_gen = Rcpp::wrap(convToJSON(myList));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_exforParser_parseEntry", (DL_FUNC) &_exforParser_parseEntry, 1},
    {"_exforParser_convToJSON", (DL_FUNC) &_exforParser_convToJSON, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_exforParser(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
