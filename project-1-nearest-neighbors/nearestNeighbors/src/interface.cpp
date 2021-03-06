#include "nn.h"
#include <R.h> // for error 
#include <R_ext/Rdynload.h> // for registration


void NN1toKmaxPredict_interface( int *n_train_observations_ptr, 
                                 int *n_test_observations_ptr, 
                                 int *n_features_ptr, 
                                 int *max_neighbors_ptr, 
                                 double *train_in_ptr, 
                                 double *train_out_ptr, 
                                 double *test_in_ptr, 
                                 double *predictions_out_ptr){
  
int status = NN1toKmaxPredict(*n_train_observations_ptr, *n_test_observations_ptr, *n_features_ptr, 
                                *max_neighbors_ptr, train_in_ptr, train_out_ptr, 
                                test_in_ptr, predictions_out_ptr);
  
if( status != 0 ){
  if( status == INVALID_NUM_TRAIN_OBSERVATIONS ){
    error("Invalid Number of Training Observations");
  }
  else if( status == INVALID_NUM_TEST_OBSERVATIONS ){
    error("Invalid Number of Test Observations");
  }
  else if( status == INVALID_NUM_FEATURES ){
    error("Invalid Number of Features");
  }
  else{
    error("Invalid Max Number of Neighbors");
  }
} 


R_CMethodDef cMethods[] = {
  {"NN1toKmaxPredict_interface", (DL_FUNC) &NN1toKmaxPredict_interface, 8 },
  {NULL, NULL, 0}
};


extern "C" {
  void R_init_nearestNeighbors(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}