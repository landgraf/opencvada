







 


CVAPI(CvStereoBMState*) cvCreateStereoBMState(int preset CV_DEFAULT(CV_STEREO_BM_BASIC),
                                              int numberOfDisparities CV_DEFAULT(0));

CVAPI(void) cvReleaseStereoBMState( CvStereoBMState** state );

CVAPI(void) cvFindStereoCorrespondenceBM( const CvArr* left, const CvArr* right,
                                          CvArr* disparity, CvStereoBMState* state );
    
CVAPI(CvRect) cvGetValidDisparityROI( CvRect roi1, CvRect roi2, int minDisparity,
                                      int numberOfDisparities, int SADWindowSize );
    
CVAPI(void) cvValidateDisparity( CvArr* disparity, const CvArr* cost,
                                 int minDisparity, int numberOfDisparities,
                                 int disp12MaxDiff CV_DEFAULT(1) );  



 

CVAPI(CvStereoGCState*) cvCreateStereoGCState( int numberOfDisparities, int maxIters );
CVAPI(void) cvReleaseStereoGCState( CvStereoGCState** state );

CVAPI(void) cvFindStereoCorrespondenceGC( const CvArr* left, const CvArr* right,
                                          CvArr* disparityLeft, CvArr* disparityRight,
                                          CvStereoGCState* state,
                                          int useDisparityGuess CV_DEFAULT(0) );


CVAPI(void)  cvReprojectImageTo3D( const CvArr* disparityImage,
                                   CvArr* _3dImage, const CvMat* Q,
                                   int handleMissingValues CV_DEFAULT(0) );