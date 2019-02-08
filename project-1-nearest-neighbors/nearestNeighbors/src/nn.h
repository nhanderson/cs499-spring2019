int NN1toKmaxPredict( int n_train_observations, int n_test_observations, int n_features, 
                      int max_neighbors, double *train_in_ptr, double *train_out_ptr, 
                      double *test_in_ptr, double *predictions_out_ptr);

#define INVALID_NUM_TRAIN_OBSERVATIONS 1
#define INVALID_NUM_TEST_OBSERVATIONS 2
#define INVALID_NUM_FEATURES 3
#define INVALID_MAX_NUMBER_NEIGHBORS 4
