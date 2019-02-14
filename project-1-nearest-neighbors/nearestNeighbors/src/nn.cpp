#include "nn.h"
#include <Eigen/Dense>
#include <stdlib.h> // for abs

int NN1toKmaxPredict( const int n_train_observations, 
                      const int n_test_observations, 
                      const int n_features, 
                      const int max_neighbors, 
                      const double *train_in_ptr,   //n_observations x n_features 
                      const double *train_out_ptr,  //n_observations
                      const double *test_in_ptr,    //n_features
                      double *predictions_out_ptr ) //max_neighbors
{
  // Error Checking
  if( max_neighbors < 0 ){
    return INVALID_MAX_NUMBER_NEIGHBORS;
  }
  if( n_train_observations < 0 ){
    return INVALID_NUM_TRAIN_OBSERVATIONS;
  }
  if( n_test_observations < 0 ){
    return INVALID_NUM_TEST_OBSERVATIONS;
  } 
  if( n_features < 0 ){
    return INVALID_NUM_FEATURES;
  }
 
  // Use Eigen to map 
  Eigen::Map< Eigen::MatrixXd > train_in_mat((double*) train_in_ptr, n_train_observations, n_features); // matrix training data inputs
  Eigen::Map< Eigen::VectorXd > test_in_vec((double*) test_in_ptr, n_features); // vector test data inputs
  Eigen::Map< Eigen::MatrixXd > test_in_mat((double*)test_in_ptr, n_test_observations, n_features); // matrix test data input
  Eigen::Map< Eigen::VectorXd > train_out_vec((double*)train_out_ptr, n_train_observations); // vector training data outputs
  Eigen::VectorXd dist_vec(n_features);
  Eigen::VectorXi sorted_index_vec(n_train_observations);
  
  for(int i=0; i<n_train_observations; i++){
    dist_vec = abs(train_in_mat.row(i).transpose().array()-test_in_vec.array()) + abs(test_in_mat.row(i).transpose().array()-train_out_vec.array()); // |test x-train x| + |test y-train y|
    dist_vec(i) = dist_vec.norm(); 
    sorted_index_vec(i) = i;
  }
  
  std::sort(sorted_index_vec.data(), 
            sorted_index_vec.data() + sorted_index_vec.size(), 
            [&dist_vec](int left, int right){ return dist_vec(left) < dist_vec(right);});
  
  double total_observations = 0.0;
  for( int k = 0; k < max_neighbors; k++ ){
    int row = sorted_index_vec(k);
    int neighbors = k+1;
    total_observations += train_out_ptr[row];
    predictions_out_ptr[k] = total_observations/neighbors; 
  }
  
  // Eigen::VectorXd distance_vec(n_train_observations);

  return 0;
}