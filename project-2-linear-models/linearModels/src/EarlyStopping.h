double LMSquareLossInterations( const int n_train, 
                             const int n_features,
                             const int max_iterations,
                             const int step_size,
                             const double *train_input_ptr,
                             const double *train_output_ptr, 
                             double *predictions_output_ptr );

double LMLogisticLossIterations( const int n_train, 
                                 const int n_features,
                                 const int max_iterations,
                                 const int step_size,
                                 const double *feature_ptr, // n_train x n_features
                                 const double *label_ptr, // n_train x 1
                                 double *weight_ptr );

#define INVALID_MAX_ITERATIONS 1
#define INVALID_STEP_SIZE 2