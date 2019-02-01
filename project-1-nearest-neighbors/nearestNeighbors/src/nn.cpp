#include "nn.h"
#include <Eigen>

int NN1toKmaxPredict( int n_train_observations, int n_test_observations, int n_features, 
                      int max_neighbors, double *train_in_ptr, double *train_out_ptr, 
                      double *test_in_ptr, double *predictions_out_ptr)
{
  // Error Checking
  
  // Use Eigen to map  
  Eigen::Map< Eigen::MatrixXd > train_in_mat(train_in_ptr, n_train_observations, n_features);
  Eigen::Map< Eigen::VectorXd > test_in_vec(test_in_ptr, n_features);
  //
}