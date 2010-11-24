












CVAPI(void)  cvCalcOpticalFlowLK( const CvArr* prev, const CvArr* curr,
                                  CvSize win_size, CvArr* velx, CvArr* vely );


CVAPI(void)  cvCalcOpticalFlowBM( const CvArr* prev, const CvArr* curr,
                                  CvSize block_size, CvSize shift_size,
                                  CvSize max_range, int use_previous,
                                  CvArr* velx, CvArr* vely );


CVAPI(void)  cvCalcOpticalFlowHS( const CvArr* prev, const CvArr* curr,
                                  int use_previous, CvArr* velx, CvArr* vely,
                                  double lambda, CvTermCriteria criteria );



CVAPI(void)  cvCalcOpticalFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
                                     CvArr*  prev_pyr, CvArr*  curr_pyr,
                                     const CvPoint2D32f* prev_features,
                                     CvPoint2D32f* curr_features,
                                     int       count,
                                     CvSize    win_size,
                                     int       level,
                                     char*     status,
                                     float*    track_error,
                                     CvTermCriteria criteria,
                                     int       flags );



CVAPI(void)  cvCalcAffineFlowPyrLK( const CvArr*  prev, const CvArr*  curr,
                                    CvArr*  prev_pyr, CvArr*  curr_pyr,
                                    const CvPoint2D32f* prev_features,
                                    CvPoint2D32f* curr_features,
                                    float* matrices, int  count,
                                    CvSize win_size, int  level,
                                    char* status, float* track_error,
                                    CvTermCriteria criteria, int flags );


CVAPI(int)  cvEstimateRigidTransform( const CvArr* A, const CvArr* B,
                                      CvMat* M, int full_affine );


CVAPI(void) cvCalcOpticalFlowFarneback( const CvArr* prev, const CvArr* next,
                                        CvArr* flow, double pyr_scale, int levels,
                                        int winsize, int iterations, int poly_n,
                                        double poly_sigma, int flags );






CVAPI(void)    cvUpdateMotionHistory( const CvArr* silhouette, CvArr* mhi,
                                      double timestamp, double duration );


CVAPI(void)    cvCalcMotionGradient( const CvArr* mhi, CvArr* mask, CvArr* orientation,
                                     double delta1, double delta2,
                                     int aperture_size CV_DEFAULT(3));


CVAPI(double)  cvCalcGlobalOrientation( const CvArr* orientation, const CvArr* mask,
                                        const CvArr* mhi, double timestamp,
                                        double duration );


CVAPI(CvSeq*)  cvSegmentMotion( const CvArr* mhi, CvArr* seg_mask,
                                CvMemStorage* storage,
                                double timestamp, double seg_thresh );




CVAPI(int)  cvCamShift( const CvArr* prob_image, CvRect  window,
                        CvTermCriteria criteria, CvConnectedComp* comp,
                        CvBox2D* box CV_DEFAULT(NULL) );


CVAPI(int)  cvMeanShift( const CvArr* prob_image, CvRect  window,
                         CvTermCriteria criteria, CvConnectedComp* comp );


 


CVAPI(CvKalman*) cvCreateKalman( int dynam_params, int measure_params,
                                 int control_params CV_DEFAULT(0));


CVAPI(void)  cvReleaseKalman( CvKalman** kalman);


CVAPI(const CvMat*)  cvKalmanPredict( CvKalman* kalman,
                                      const CvMat* control CV_DEFAULT(NULL));


CVAPI(const CvMat*)  cvKalmanCorrect( CvKalman* kalman, const CvMat* measurement );