#include "EarlyStopping.h"
#include <R.h> // for error 
#include <R_ext/Rdynload.h>

void LMSquareLossInterations_interface( const int *n_train, 
                                        const int *n_features,
                                        const int *max_iterations,
                                        const int *step_size,
                                        const double *feature_ptr, // n_train x n_features
                                        const double *label_ptr, // n_train x 1
                                        double *weight_ptr ){
  
int status = LMSquareLossInterations(*n_train,*n_features, 
                                *max_iterations, *step_size, feature_ptr, label_ptr, 
                                 weight_ptr);
  
if( status != 0 ){
    if( status == INVALID_MAX_ITERATIONS ){
      error("Invalid max iterations number");
    }
    else if( status == INVALID_STEP_SIZE ){
      error("Invalid step size number");
    } 
  }
}

<<<<<<< HEAD
void LMLogisticLossIterations_interface( const int *n_train, 
                                        const int *n_features,
                                        const int *max_iterations,
                                        const int *step_size,
                                        const double *feature_ptr, // n_train x n_features
                                        const double *label_ptr, // n_train x 1
                                        double *weight_ptr ){
  
int status = LMLogisticLossIterations(*n_train,*n_features, 
                                       *max_iterations, *step_size, feature_ptr, label_ptr, 
                                       weight_ptr);
  
  if( status != 0 ){
    if( status == INVALID_MAX_ITERATIONS ){
      error("Invalid max iterations number");
    }
    else if( status == INVALID_STEP_SIZE ){
      error("Invalid step size number");
    } 
=======
void LMLogisticLoss_interface( const int *n_train, 
                               const int *n_features,
                               const double *feature_ptr,
                               const double *label_ptr,
                               const double *weight_ptr,
                               double *output_ptr ){
  
int status = LMLogisticLoss(*n_train,
                            *n_features, 
                            feature_ptr,
                            label_ptr,
                            weight_ptr, 
                            output_ptr);
if( status != 0 ){
  error("Unknown error in LMLogisticLoss");
>>>>>>> 231c0bf329e8af94d14888a0762c3758c030d6bb
  }
}

R_CMethodDef cMethods[] = {
  {"LMSquareLossInterations_interface", (DL_FUNC) &LMSquareLossInterations_interface, 7 },
<<<<<<< HEAD
  {"LMLogisticLossIterations_interface", (DL_FUNC) &LMLogisticLossIterations_interface, 7 },
=======
  {"LMLogisticLoss_interface", (DL_FUNC) &LMLogisticLoss_interface, 6 },
>>>>>>> 231c0bf329e8af94d14888a0762c3758c030d6bb
  {NULL, NULL, 0}
};

extern "C" {
  void R_init_linearModels(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}