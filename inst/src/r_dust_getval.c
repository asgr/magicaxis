#include <R.h>
#include <Rinternals.h>
#include "interface.h"
#include "subs_fits.h"
#include "subs_lambert.h"
#include "subs_memory.h"

SEXP r_dust_getval_call(SEXP pFileNSEXP,
                        SEXP pFileSSEXP,
                        SEXP pGallSEXP,
                        SEXP pGalbSEXP,
                        SEXP pQInterpSEXP,
                        SEXP pQNoloopSEXP,
                        SEXP pQVerboseSEXP)
{
  R_xlen_t iGal;
  R_xlen_t nGal;
  int qInterp;
  int qNoloop;
  int qVerbose;
  float *pGall;
  float *pGalb;
  float *pMapval;
  SEXP pOutputSEXP;

  if (TYPEOF(pFileNSEXP) != STRSXP || XLENGTH(pFileNSEXP) != 1) {
    error("north map path must be a length-1 character vector");
  }
  if (TYPEOF(pFileSSEXP) != STRSXP || XLENGTH(pFileSSEXP) != 1) {
    error("south map path must be a length-1 character vector");
  }
  if (!isReal(pGallSEXP) || !isReal(pGalbSEXP)) {
    error("coordinates must be numeric");
  }

  nGal = XLENGTH(pGallSEXP);
  if (XLENGTH(pGalbSEXP) != nGal) {
    error("gall and galb must have the same length");
  }

  qInterp = asLogical(pQInterpSEXP);
  qNoloop = asLogical(pQNoloopSEXP);
  qVerbose = asLogical(pQVerboseSEXP);

  pGall = (float *) R_alloc((size_t)nGal, sizeof(float));
  pGalb = (float *) R_alloc((size_t)nGal, sizeof(float));

  for (iGal = 0; iGal < nGal; iGal++) {
    pGall[iGal] = (float) REAL(pGallSEXP)[iGal];
    pGalb[iGal] = (float) REAL(pGalbSEXP)[iGal];
  }

  pMapval = lambert_getval(
    (char *) CHAR(STRING_ELT(pFileNSEXP, 0)),
    (char *) CHAR(STRING_ELT(pFileSSEXP, 0)),
    (long) nGal,
    pGall,
    pGalb,
    qInterp,
    qNoloop,
    qVerbose
  );

  PROTECT(pOutputSEXP = allocVector(REALSXP, nGal));
  for (iGal = 0; iGal < nGal; iGal++) {
    REAL(pOutputSEXP)[iGal] = (double) pMapval[iGal];
  }

  ccfree_((void **)&pMapval);

  UNPROTECT(1);
  return pOutputSEXP;
}
