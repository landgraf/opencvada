





CVAPI(CvSeq*) cvSegmentImage( const CvArr* srcarr, CvArr* dstarr,
                                    double canny_threshold,
                                    double ffill_threshold,
                                    CvMemStorage* storage );



 int (CV_CDECL * CvCallback)(int index, void* buffer, void* user_data);



CVAPI(void)  cvCalcCovarMatrixEx( int nObjects, void* input, int ioFlags,
                                  int ioBufSize, uchar* buffer, void* userData,
                                  IplImage* avg, float* covarMatrix );


CVAPI(void)  cvCalcEigenObjects( int nObjects, void* input, void* output,
                                 int ioFlags, int ioBufSize, void* userData,
                                 CvTermCriteria* calcLimit, IplImage* avg,
                                 float* eigVals );


CVAPI(double)  cvCalcDecompCoeff( IplImage* obj, IplImage* eigObj, IplImage* avg );


CVAPI(void)  cvEigenDecomposite( IplImage* obj, int nEigObjs, void* eigInput,
                                 int ioFlags, void* userData, IplImage* avg,
                                 float* coeffs );


CVAPI(void)  cvEigenProjection( void* eigInput, int nEigObjs, int ioFlags,
                                void* userData, float* coeffs, IplImage* avg,
                                IplImage* proj );

 

 






CVAPI(CvEHMM*)  cvCreate2DHMM( int* stateNumber, int* numMix, int obsSize );


CVAPI(void)  cvRelease2DHMM( CvEHMM** hmm );



CVAPI(CvImgObsInfo*)  cvCreateObsInfo( CvSize numObs, int obsSize );


CVAPI(void)  cvReleaseObsInfo( CvImgObsInfo** obs_info );



CVAPI(void)  cvImgToObs_DCT( const CvArr* arr, float* obs, CvSize dctSize,
                             CvSize obsSize, CvSize delta );



CVAPI(void)  cvUniformImgSegm( CvImgObsInfo* obs_info, CvEHMM* ehmm );


CVAPI(void)  cvInitMixSegm( CvImgObsInfo** obs_info_array,
                            int num_img, CvEHMM* hmm );


CVAPI(void)  cvEstimateHMMStateParams( CvImgObsInfo** obs_info_array,
                                       int num_img, CvEHMM* hmm );


CVAPI(void)  cvEstimateTransProb( CvImgObsInfo** obs_info_array,
                                  int num_img, CvEHMM* hmm );


CVAPI(void)  cvEstimateObsProb( CvImgObsInfo* obs_info,
                                CvEHMM* hmm );


CVAPI(float)  cvEViterbi( CvImgObsInfo* obs_info, CvEHMM* hmm );



CVAPI(void)  cvMixSegmL2( CvImgObsInfo** obs_info_array,
                          int num_img, CvEHMM* hmm );




CVAPI(void)  cvCreateHandMask( CvSeq* hand_points,
                                   IplImage *img_mask, CvRect *roi);


CVAPI(void)  cvFindHandRegion (CvPoint3D32f* points, int count,
                                CvSeq* indexs,
                                float* line, CvSize2D32f size, int flag,
                                CvPoint3D32f* center,
                                CvMemStorage* storage, CvSeq **numbers);


CVAPI(void)  cvFindHandRegionA( CvPoint3D32f* points, int count,
                                CvSeq* indexs,
                                float* line, CvSize2D32f size, int jc,
                                CvPoint3D32f* center,
                                CvMemStorage* storage, CvSeq **numbers);


CVAPI(void)  cvCalcImageHomography( float* line, CvPoint3D32f* center,
                                    float* intrinsic, float* homography );




CVAPI(void)  icvDrawMosaic( CvSubdiv2D* subdiv, IplImage* src, IplImage* dst );



CVAPI(int)   icvSubdiv2DCheck( CvSubdiv2D* subdiv );


CV_INLINE double icvSqDist2D32f( CvPoint2D32f pt1, CvPoint2D32f pt2 )
;









 

 

 



CVAPI(void)  cvCalcPGH( const CvSeq* contour, CvHistogram* hist );



CVAPI(CvSeq*) cvFindDominantPoints( CvSeq* contour, CvMemStorage* storage,
                                   int method CV_DEFAULT(CV_DOMINANT_IPAN),
                                   double parameter1 CV_DEFAULT(0),
                                   double parameter2 CV_DEFAULT(0),
                                   double parameter3 CV_DEFAULT(0),
                                   double parameter4 CV_DEFAULT(0));






 



















CVAPI(void) 
cvFindStereoCorrespondence( 
                   const  CvArr* leftImage, const  CvArr* rightImage,
                   int     mode,
                   CvArr*  dispImage,
                   int     maxDisparity,                                
                   double  param1 CV_DEFAULT(CV_UNDEF_SC_PARAM), 
                   double  param2 CV_DEFAULT(CV_UNDEF_SC_PARAM), 
                   double  param3 CV_DEFAULT(CV_UNDEF_SC_PARAM), 
                   double  param4 CV_DEFAULT(CV_UNDEF_SC_PARAM), 
                   double  param5 CV_DEFAULT(CV_UNDEF_SC_PARAM) );






 


 

 


 


CVAPI(int) icvConvertWarpCoordinates(double coeffs[3][3],
                                CvPoint2D32f* cameraPoint,
                                CvPoint2D32f* warpPoint,
                                int direction);

CVAPI(int) icvGetSymPoint3D(  CvPoint3D64f pointCorner,
                            CvPoint3D64f point1,
                            CvPoint3D64f point2,
                            CvPoint3D64f *pointSym2);

CVAPI(void) icvGetPieceLength3D(CvPoint3D64f point1,CvPoint3D64f point2,double* dist);

CVAPI(int) icvCompute3DPoint(    double alpha,double betta,
                            CvStereoLineCoeff* coeffs,
                            CvPoint3D64f* point);

CVAPI(int) icvCreateConvertMatrVect( CvMatr64d     rotMatr1,
                                CvMatr64d     transVect1,
                                CvMatr64d     rotMatr2,
                                CvMatr64d     transVect2,
                                CvMatr64d     convRotMatr,
                                CvMatr64d     convTransVect);

CVAPI(int) icvConvertPointSystem(CvPoint3D64f  M2,
                            CvPoint3D64f* M1,
                            CvMatr64d     rotMatr,
                            CvMatr64d     transVect
                            );

CVAPI(int) icvComputeCoeffForStereo(  CvStereoCamera* stereoCamera);

CVAPI(int) icvGetCrossPieceVector(CvPoint2D32f p1_start,CvPoint2D32f p1_end,CvPoint2D32f v2_start,CvPoint2D32f v2_end,CvPoint2D32f *cross);
CVAPI(int) icvGetCrossLineDirect(CvPoint2D32f p1,CvPoint2D32f p2,float a,float b,float c,CvPoint2D32f* cross);
CVAPI(float) icvDefinePointPosition(CvPoint2D32f point1,CvPoint2D32f point2,CvPoint2D32f point);
CVAPI(int) icvStereoCalibration( int numImages,
                            int* nums,
                            CvSize imageSize,
                            CvPoint2D32f* imagePoints1,
                            CvPoint2D32f* imagePoints2,
                            CvPoint3D32f* objectPoints,
                            CvStereoCamera* stereoparams
                           );


CVAPI(int) icvComputeRestStereoParams(CvStereoCamera *stereoparams);

CVAPI(void) cvComputePerspectiveMap( const double coeffs[3][3], CvArr* rectMapX, CvArr* rectMapY );

CVAPI(int) icvComCoeffForLine(   CvPoint2D64f point1,
                            CvPoint2D64f point2,
                            CvPoint2D64f point3,
                            CvPoint2D64f point4,
                            CvMatr64d    camMatr1,
                            CvMatr64d    rotMatr1,
                            CvMatr64d    transVect1,
                            CvMatr64d    camMatr2,
                            CvMatr64d    rotMatr2,
                            CvMatr64d    transVect2,
                            CvStereoLineCoeff*    coeffs,
                            int* needSwapCameras);

CVAPI(int) icvGetDirectionForPoint(  CvPoint2D64f point,
                                CvMatr64d camMatr,
                                CvPoint3D64f* direct);

CVAPI(int) icvGetCrossLines(CvPoint3D64f point11,CvPoint3D64f point12,
                       CvPoint3D64f point21,CvPoint3D64f point22,
                       CvPoint3D64f* midPoint);

CVAPI(int) icvComputeStereoLineCoeffs(   CvPoint3D64f pointA,
                                    CvPoint3D64f pointB,
                                    CvPoint3D64f pointCam1,
                                    double gamma,
                                    CvStereoLineCoeff*    coeffs);



CVAPI(int) icvGetAngleLine( CvPoint2D64f startPoint, CvSize imageSize,CvPoint2D64f *point1,CvPoint2D64f *point2);

CVAPI(void) icvGetCoefForPiece(   CvPoint2D64f p_start,CvPoint2D64f p_end,
                        double *a,double *b,double *c,
                        int* result);



CVAPI(void) icvComputeeInfiniteProject1(CvMatr64d    rotMatr,
                                     CvMatr64d    camMatr1,
                                     CvMatr64d    camMatr2,
                                     CvPoint2D32f point1,
                                     CvPoint2D32f *point2);

CVAPI(void) icvComputeeInfiniteProject2(CvMatr64d    rotMatr,
                                     CvMatr64d    camMatr1,
                                     CvMatr64d    camMatr2,
                                     CvPoint2D32f* point1,
                                     CvPoint2D32f point2);

CVAPI(void) icvGetCrossDirectDirect(  CvVect64d direct1,CvVect64d direct2,
                            CvPoint2D64f *cross,int* result);

CVAPI(void) icvGetCrossPieceDirect(   CvPoint2D64f p_start,CvPoint2D64f p_end,
                            double a,double b,double c,
                            CvPoint2D64f *cross,int* result);

CVAPI(void) icvGetCrossPiecePiece( CvPoint2D64f p1_start,CvPoint2D64f p1_end,
                            CvPoint2D64f p2_start,CvPoint2D64f p2_end,
                            CvPoint2D64f* cross,
                            int* result);
                            
CVAPI(void) icvGetPieceLength(CvPoint2D64f point1,CvPoint2D64f point2,double* dist);

CVAPI(void) icvGetCrossRectDirect(    CvSize imageSize,
                            double a,double b,double c,
                            CvPoint2D64f *start,CvPoint2D64f *end,
                            int* result);

CVAPI(void) icvProjectPointToImage(   CvPoint3D64f point,
                            CvMatr64d camMatr,CvMatr64d rotMatr,CvVect64d transVect,
                            CvPoint2D64f* projPoint);

CVAPI(void) icvGetQuadsTransform( CvSize        imageSize,
                        CvMatr64d     camMatr1,
                        CvMatr64d     rotMatr1,
                        CvVect64d     transVect1,
                        CvMatr64d     camMatr2,
                        CvMatr64d     rotMatr2,
                        CvVect64d     transVect2,
                        CvSize*       warpSize,
                        double quad1[4][2],
                        double quad2[4][2],
                        CvMatr64d     fundMatr,
                        CvPoint3D64f* epipole1,
                        CvPoint3D64f* epipole2
                        );

CVAPI(void) icvGetQuadsTransformStruct(  CvStereoCamera* stereoCamera);

CVAPI(void) icvComputeStereoParamsForCameras(CvStereoCamera* stereoCamera);

CVAPI(void) icvGetCutPiece(   CvVect64d areaLineCoef1,CvVect64d areaLineCoef2,
                    CvPoint2D64f epipole,
                    CvSize imageSize,
                    CvPoint2D64f* point11,CvPoint2D64f* point12,
                    CvPoint2D64f* point21,CvPoint2D64f* point22,
                    int* result);

CVAPI(void) icvGetMiddleAnglePoint(   CvPoint2D64f basePoint,
                            CvPoint2D64f point1,CvPoint2D64f point2,
                            CvPoint2D64f* midPoint);

CVAPI(void) icvGetNormalDirect(CvVect64d direct,CvPoint2D64f point,CvVect64d normDirect);

CVAPI(double) icvGetVect(CvPoint2D64f basePoint,CvPoint2D64f point1,CvPoint2D64f point2);

CVAPI(void) icvProjectPointToDirect(  CvPoint2D64f point,CvVect64d lineCoeff,
                            CvPoint2D64f* projectPoint);

CVAPI(void) icvGetDistanceFromPointToDirect( CvPoint2D64f point,CvVect64d lineCoef,double*dist);

CVAPI(IplImage*) icvCreateIsometricImage( IplImage* src, IplImage* dst,
                              int desired_depth, int desired_num_channels );

CVAPI(void) cvDeInterlace( const CvArr* frame, CvArr* fieldEven, CvArr* fieldOdd );







 


CVAPI(CvContourTree*)  cvCreateContourTree( const CvSeq* contour,
                                            CvMemStorage* storage,
                                            double threshold );


CVAPI(CvSeq*)  cvContourFromContourTree( const CvContourTree* tree,
                                         CvMemStorage* storage,
                                         CvTermCriteria criteria );




CVAPI(double)  cvMatchContourTrees( const CvContourTree* tree1,
                                    const CvContourTree* tree2,
                                    int method, double threshold );




CvSeq* cvCalcContoursCorrespondence( const CvSeq* contour1,
                                     const CvSeq* contour2, 
                                     CvMemStorage* storage);


CvSeq* cvMorphContours( const CvSeq* contour1, const CvSeq* contour2,
                        CvSeq* corr, double alpha,
                        CvMemStorage* storage );





CVAPI(void)  cvSnakeImage( const IplImage* image, CvPoint* points,
                           int  length, float* alpha,
                           float* beta, float* gamma,
                           int coeff_usage, CvSize  win,
                           CvTermCriteria criteria, int calc_gradient CV_DEFAULT(1));







 

CVAPI(CvFaceTracker*) cvInitFaceTracker(CvFaceTracker* pFaceTracking, const IplImage* imgGray,
                                                CvRect* pRects, int nRects);
CVAPI(int) cvTrackFace( CvFaceTracker* pFaceTracker, IplImage* imgGray,
                              CvRect* pRects, int nRects,
                              CvPoint* ptRotate, double* dbAngleRotate);
CVAPI(void) cvReleaseFaceTracker(CvFaceTracker** ppFaceTracker);


 

CvSeq * cvFindFace(IplImage * Image,CvMemStorage* storage);
CvSeq * cvPostBoostingFindFace(IplImage * Image,CvMemStorage* storage);

 

CV_INLINE Cv3dTracker2dTrackedObject cv3dTracker2dTrackedObject(int id, CvPoint2D32f p)
;

 

CV_INLINE Cv3dTrackerTrackedObject cv3dTrackerTrackedObject(int id, CvPoint3D32f p)
;

 

 

CVAPI(CvBool) cv3dTrackerCalibrateCameras(int num_cameras,
                     const Cv3dTrackerCameraIntrinsics camera_intrinsics[], 
                     CvSize etalon_size,
                     float square_size,
                     IplImage *samples[],                                   
                     Cv3dTrackerCameraInfo camera_info[]);                  

CVAPI(int)  cv3dTrackerLocateObjects(int num_cameras, int num_objects,
                   const Cv3dTrackerCameraInfo camera_info[],        
                   const Cv3dTracker2dTrackedObject tracking_info[], 
                   Cv3dTrackerTrackedObject tracked_objects[]);      





 



 


 


 


 


CVAPI(int)  cvVoronoiDiagramFromContour(CvSeq* ContourSeq,
                                           CvVoronoiDiagram2D** VoronoiDiagram,
                                           CvMemStorage* VoronoiStorage,
                                           CvLeeParameters contour_type CV_DEFAULT(CV_LEE_INT),
                                           int contour_orientation CV_DEFAULT(-1),
                                           int attempt_number CV_DEFAULT(10));


CVAPI(int)  cvVoronoiDiagramFromImage(IplImage* pImage,
                                         CvSeq** ContourSeq,
                                         CvVoronoiDiagram2D** VoronoiDiagram,
                                         CvMemStorage* VoronoiStorage,
                                         CvLeeParameters regularization_method CV_DEFAULT(CV_LEE_NON),
                                         float approx_precision CV_DEFAULT(CV_LEE_AUTO));


CVAPI(void) cvReleaseVoronoiStorage(CvVoronoiDiagram2D* VoronoiDiagram,
                                          CvMemStorage** pVoronoiStorage);





 



CVAPI(CvGraph*) cvLinearContorModelFromVoronoiDiagram(CvVoronoiDiagram2D* VoronoiDiagram,
                                                         float maxWidth);


CVAPI(int) cvReleaseLinearContorModelStorage(CvGraph** Graph);




CVAPI(void) cvInitPerspectiveTransform( CvSize size, const CvPoint2D32f vertex[4], double matrix[3][3],
                                              CvArr* rectMap );





 


CVAPI(void)  cvMakeScanlines( const CvMatrix3* matrix, CvSize  img_size,
                              int*  scanlines1, int*  scanlines2,
                              int*  lengths1, int*  lengths2,
                              int*  line_count );


CVAPI(void)  cvPreWarpImage( int       line_count,
                             IplImage* img,
                             uchar*    dst,
                             int*      dst_nums,
                             int*      scanlines);


CVAPI(void)  cvFindRuns( int    line_count,
                         uchar* prewarp1,
                         uchar* prewarp2,
                         int*   line_lengths1,
                         int*   line_lengths2,
                         int*   runs1,
                         int*   runs2,
                         int*   num_runs1,
                         int*   num_runs2);


CVAPI(void)  cvDynamicCorrespondMulti( int  line_count,
                                       int* first,
                                       int* first_runs,
                                       int* second,
                                       int* second_runs,
                                       int* first_corr,
                                       int* second_corr);


CVAPI(void)  cvMakeAlphaScanlines( int*  scanlines1,
                                   int*  scanlines2,
                                   int*  scanlinesA,
                                   int*  lengths,
                                   int   line_count,
                                   float alpha);


CVAPI(void)  cvMorphEpilinesMulti( int    line_count,
                                   uchar* first_pix,
                                   int*   first_num,
                                   uchar* second_pix,
                                   int*   second_num,
                                   uchar* dst_pix,
                                   int*   dst_num,
                                   float  alpha,
                                   int*   first,
                                   int*   first_runs,
                                   int*   second,
                                   int*   second_runs,
                                   int*   first_corr,
                                   int*   second_corr);


CVAPI(void)  cvPostWarpImage( int       line_count,
                              uchar*    src,
                              int*      src_nums,
                              IplImage* img,
                              int*      scanlines);


CVAPI(void)  cvDeleteMoire( IplImage*  img );


 
                               

CVAPI(CvConDensation*)  cvCreateConDensation( int dynam_params,
                                             int measure_params,
                                             int sample_count );


CVAPI(void)  cvReleaseConDensation( CvConDensation** condens );


CVAPI(void)  cvConDensUpdateByTime( CvConDensation* condens);


CVAPI(void)  cvConDensInitSampleSet( CvConDensation* condens, CvMat* lower_bound, CvMat* upper_bound );                               

CV_INLINE int iplWidth( const IplImage* img )
;

CV_INLINE int iplHeight( const IplImage* img )
;