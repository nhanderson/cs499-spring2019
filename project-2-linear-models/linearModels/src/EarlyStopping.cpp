
#include "EarlyStopping.h"
#include <Eigen/Dense>

double LMSquareLossInterations( const int n_train, 
                             const int n_test,
                             const int n_features,
                             const int max_iterations,
                             const int step_size,
                             const double *feature_ptr, // n_train x n_features
                             const double *label_ptr, // n_train x 1
                             double *weight_ptr )// n_features x max_iterations
{
  // Error Checking
  if( max_iterations <= 1 ){
    return INVALID_MAX_ITERATIONS;
  }
  if( step_size < 1 ){
    return INVALID_STEP_SIZE;
  }
  
  //scaling input
  
  Eigen::Map< Eigen::MatrixXd > feature_mat((double*) feature_ptr, n_train, n_features); // feature matrix (n_train x f_features)
  Eigen::Map< Eigen::VectorXd > label_vec((double*) label_ptr, n_train); // label vector (n_train x 1)
  // Eigen::Map< Eigen::MatrixXd > test_in_mat((double*) test_in_ptr, n_test_observations, n_features); // matrix test data input
  
  double total = 0.0;
  for (int i = 0; i < max_iterations; i++ ){
    total += label_vec(i) - feature_mat(i);
  }
  
  double meanloss = total/max_iterations;
  
}
