#include "nn.h"
#include <Eigen>
#include <stdlib.h> // for abs

int NN1toKmaxPredict( int n_train_observations, int n_test_observations, int n_features, 
                      int max_neighbors, double *train_in_ptr, double *train_out_ptr, 
                      double *test_in_ptr, double *predictions_out_ptr)
{
  // Error Checking
  
  Eigen::VectorXd distance_vec(n_train_observations);
  // Use Eigen to map 
  Eigen::Map< Eigen::MatrixXd > train_in_mat(train_in_ptr, n_train_observations, n_features); // matrix training data inputs
  Eigen::Map< Eigen::VectorXd > train_out_vec(train_out_ptr, n_train_observations); // vector training data outputs
  Eigen::Map< Eigen::MatrixXd > test_in_mat(test_in_ptr, n_test_observations, n_features); // matrix test data input
  Eigen::Map< Eigen::VectorXd > test_in_vec(test_in_ptr, n_features); // not sure what this is for
  Eigen::VectorXd diff_vec(n_features);
  Eigen::VectorXi sorted_index_vec(n_train_observations)
  for(int i=0; i<n_train_observations; i++){
    diff_vec = abs(train_in_mat.row(i).transpose()-test_in_vec) + abs(test_in_mat.row(i).transpose()-train_out_vec); // |test x-train x| + |test y-train y|
    distance_vec(i) = diff_vec.norm(); 
    sorted_index_vec(i) = i;
  }
  std::sort(sorted_index_vec.data(), sored_index_vec.data() + sorted_index_vec.size(), 
            [&distance_vec](int left, int right){ return distance_vec(left) < distance_vec(right)});
  double total_observations = 0.0;
  for( int k = 0; k < max_neighbors; k++ ){
    int row = sorted_index_vec(k);
    int neighbors = k+1;
    total_observations += train_out_ptr[row];
    preditions_out_ptr[k] = total_observations/neighbors; 
  }
  return 0;
}