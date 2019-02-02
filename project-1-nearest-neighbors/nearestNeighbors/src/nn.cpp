#include "nn.h"
#include <Eigen>

int NN1toKmaxPredict( int n_train_observations, int n_test_observations, int n_features, 
                      int max_neighbors, double *train_in_ptr, double *train_out_ptr, 
                      double *test_in_ptr, double *predictions_out_ptr)
{
  // Error Checking
  
  Eigen::VectorXd distance_vec(n_train_observations);
  // Use Eigen to map 
  Eigen::Map< Eigen::MatrixXd > train_in_mat(train_in_ptr, n_train_observations, n_features);
  Eigen::Map< Eigen::VectorXd > test_in_vec(test_in_ptr, n_features);
  Eigen::VectorXd diff_vec(ncol);
  for(int i=0; i<n_train_observations; i++){
    diff_vec = train_input_mat.row(i).transpose()-test_in_ptr;
    distance_vec(i) = diff_vec
  }
}