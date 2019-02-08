#include "nn.h"
#include <R.h> // for error
#include <R_ext/Rdynload.h> // for registration


void NN1toKmaxPredict_interface( int *n_train_observations, int *n_test_observations, int *n_features, 
                                     int *max_neighbors, double *train_in_ptr, double *train_out_ptr, 
                                     double *test_in_ptr, double *predictions_out_ptr){
  
int status = NN1toKmaxPredict(*n_train_observations, *n_test_observations, *n_features, 
                                *max_neighbors, train_in_ptr, train_out_ptr, 
                                test_in_ptr, predictions_out_ptr);
  
if(status != NO_ERR){
  if(status == INVALID_NUM_TRAIN_OBSERVATIONS ){
    error("Invalid Number of Training Observations");
  }
  else if(status == INVALID_NUM_TEST_OBSERVATIONS){
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
  {"NN1toKmaxPredict_interface", (DL_FUNC) &NN1toKmaxPredict_interface, 5 }
}
}

