
#ifndef __INCsubs_predict_h
#define __INCsubs_predict_h

void DECLARE(fort_predict_thermal)
  (void  *  pNGal,
   float *  pGall,
   float *  pGalb,
   float *  pNu,
   char  *  pIPath,
   char  *  pResName,
   char  *  pUnitsName,
   void  *  pModelNum,
   void  *  pQInterp,
   void  *  pQNoloop,
   void  *  pQVerbose,
   float *  pOutput);
void DECLARE(fort_predict_sync)
  (void  *  pNGal,
   float *  pGall,
   float *  pGalb,
   float *  pNu,
   char  *  pIPath,
   char  *  pUnitsName,
   void  *  pQInterp,
   void  *  pQNoloop,
   void  *  pQVerbose,
   float *  pOutput);
float * predict_thermal
  (long     nGal,
   float *  pGall,
   float *  pGalb,
   float *  pNu,
   char  *  pIPath,
   char  *  pResName,
   char  *  pUnitsName,
   int      modelNum,
   int      qInterp,
   int      qNoloop,
   int      qVerbose);
float * predict_sync
  (long     nGal,
   float *  pGall,
   float *  pGalb,
   float *  pNu,
   char  *  pIPath,
   char  *  pUnitsName,
   int      qInterp,
   int      qNoloop,
   int      qVerbose);
float kfactor
  (float    alpha,
   float    temp);
float planck
  (float    temp,
   float    nu);
float fac_flux2temp
  (float    nu);
float planckcorr
  (float    nu);

#endif /* __INCsubs_predict_h */
