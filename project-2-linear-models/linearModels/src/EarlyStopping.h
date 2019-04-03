double LMSquareLossInterations( const int n_train, 
                             const int n_features,
                             const int max_iterations,
                             const int step_size,
                             const double *train_input_ptr,
                             const double *train_output_ptr, 
                             double *predictions_output_ptr );

<<<<<<< HEAD
double LMLogisticLossIterations( const int n_train, 
                                 const int n_features,
                                 const int max_iterations,
                                 const int step_size,
                                 const double *feature_ptr, // n_train x n_features
                                 const double *label_ptr, // n_train x 1
                                 double *weight_ptr );
=======
int LMLogisticLoss( const int n_train, 
                    const int n_features,
                    const double *feature_ptr,
                    const double *label_ptr,
                    const double *weight_ptr,
                    double *output_ptr );
>>>>>>> 231c0bf329e8af94d14888a0762c3758c030d6bb

#define INVALID_MAX_ITERATIONS 1
#define INVALID_STEP_SIZE 2