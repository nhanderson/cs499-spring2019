int NN1toKmaxPredict( const int n_train_observations, 
                      const int n_test_observations, 
                      const int n_features, 
                      int max_neighbors,
                      const double *train_in_ptr, 
                      const double *train_out_ptr, 
                      const double *test_in_ptr, 
                      double *predictions_out_ptr);

#define INVALID_NUM_TRAIN_OBSERVATIONS 1
#define INVALID_NUM_TEST_OBSERVATIONS 2
#define INVALID_NUM_FEATURES 3
#define INVALID_MAX_NUMBER_NEIGHBORS 4
