int LMSquareLossInterations( const int n_train, 
                             const int n_features,
                             const int max_iterations,
                             const int step_size,
                             const double *train_input_ptr,
                             const double *train_output_ptr, 
                             double *predictions_output_ptr );

int LMLogisticLoss( const int n_train, 
                    const int n_features,
                    const double *feature_ptr,
                    const double *label_ptr,
                    const double *weight_ptr,
                    double *output_ptr );

#define INVALID_MAX_ITERATIONS 1
#define INVALID_STEP_SIZE 2