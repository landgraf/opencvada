
















CVAPI(void) cvReleaseBGStatModel( CvBGStatModel** bg_model );


CVAPI(int) cvUpdateBGStatModel( IplImage* current_frame, CvBGStatModel*  bg_model,
                                double learningRate CV_DEFAULT(-1));






CVAPI(void) cvRefineForegroundMaskBySegm( CvSeq* segments, CvBGStatModel*  bg_model );


CVAPI(int)  cvChangeDetection( IplImage*  prev_frame,
                               IplImage*  curr_frame,
                               IplImage*  change_mask );

















 

 

 

 


 


CVAPI(CvBGStatModel*) cvCreateFGDStatModel( IplImage* first_frame,
                    CvFGDStatModelParams* parameters CV_DEFAULT(NULL));










 

 

 


 



CVAPI(CvBGStatModel*) cvCreateGaussianBGModel( IplImage* first_frame,
                CvGaussBGStatModelParams* parameters CV_DEFAULT(NULL));


 

 

CVAPI(CvBGCodeBookModel*) cvCreateBGCodeBookModel();
CVAPI(void) cvReleaseBGCodeBookModel( CvBGCodeBookModel** model );

CVAPI(void) cvBGCodeBookUpdate( CvBGCodeBookModel* model, const CvArr* image,
                                CvRect roi CV_DEFAULT(cvRect(0,0,0,0)),
                                const CvArr* mask CV_DEFAULT(0) );

CVAPI(int) cvBGCodeBookDiff( const CvBGCodeBookModel* model, const CvArr* image,
                             CvArr* fgmask, CvRect roi CV_DEFAULT(cvRect(0,0,0,0)) );

CVAPI(void) cvBGCodeBookClearStale( CvBGCodeBookModel* model, int staleThresh,
                                    CvRect roi CV_DEFAULT(cvRect(0,0,0,0)),
                                    const CvArr* mask CV_DEFAULT(0) );

CVAPI(CvSeq*) cvSegmentFGMask( CvArr *fgmask, int poly1Hull0 CV_DEFAULT(1),
                               float perimScale CV_DEFAULT(4.f),
                               CvMemStorage* storage CV_DEFAULT(0),
                               CvPoint offset CV_DEFAULT(cvPoint(0,0)));