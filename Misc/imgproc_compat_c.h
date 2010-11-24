







        
        













 CvMat cvMatArray( int rows, int cols, int type,
                            int count, void* data CV_DEFAULT(0))
;




 double cvMean( const CvArr* image, const CvArr* mask CV_DEFAULT(0))
;


 double  cvSumPixels( const CvArr* image )
;

 void  cvMean_StdDev( const CvArr* image, double* mean, double* sdv,
                               const CvArr* mask CV_DEFAULT(0))
;


 void cvmPerspectiveProject( const CvMat* mat, const CvArr* src, CvArr* dst )
;


 void cvFillImage( CvArr* mat, double color )
;







  void  cvRandSetRange( CvRandState* state, double param1,
                                 double param2, int index CV_DEFAULT(-1))
;


  void  cvRandInit( CvRandState* state, double param1,
                             double param2, int seed,
                             int disttype CV_DEFAULT(CV_RAND_UNI))
;



 void cvRand( CvRandState* state, CvArr* arr )
;


 void cvbRand( CvRandState* state, float* dst, int len )
;


 void  cvbCartToPolar( const float* y, const float* x,
                                float* magnitude, float* angle, int len )
;


 void  cvbFastArctan( const float* y, const float* x,
                               float* angle, int len )
;


  void  cvbSqrt( const float* x, float* y, int len )
;


  void  cvbInvSqrt( const float* x, float* y, int len )
;


  void  cvbReciprocal( const float* x, float* y, int len )
;


  void  cvbFastExp( const float* x, double* y, int len )
;


  void  cvbFastLog( const double* x, float* y, int len )
;


  CvRect  cvContourBoundingRect( void* point_set, int update CV_DEFAULT(0))
;


 double cvPseudoInverse( const CvArr* src, CvArr* dst )
;











 void cvConvexHull( CvPoint* points, int num_points,
                             CvRect* CV_UNREFERENCED(bound_rect),
                             int orientation, int* hull, int* hullsize )
;








 void cvMinAreaRect( CvPoint* points, int n,
                              int CV_UNREFERENCED(left), int CV_UNREFERENCED(bottom),
                              int CV_UNREFERENCED(right), int CV_UNREFERENCED(top),
                              CvPoint2D32f* anchor,
                              CvPoint2D32f* vect1,
                              CvPoint2D32f* vect2 )
;





  void  cvFitLine3D( CvPoint3D32f* points, int count, int dist,
                              void *param, float reps, float aeps, float* line )
;


  void  cvFitLine2D( CvPoint2D32f* points, int count, int dist,
                              void *param, float reps, float aeps, float* line )
;


  void cvFitEllipse( const CvPoint2D32f* points, int count, CvBox2D* box )
;


  void  cvProject3D( CvPoint3D32f* points3D, int count,
                              CvPoint2D32f* points2D,
                              int xIndx CV_DEFAULT(0),
                              int yIndx CV_DEFAULT(1))
;









  int  cvHoughLines( CvArr* image, double rho,
                              double theta, int threshold,
                              float* lines, int linesNumber )
;


  int  cvHoughLinesP( CvArr* image, double rho,
                               double theta, int threshold,
                               int lineLength, int lineGap,
                               int* lines, int linesNumber )
;


  int  cvHoughLinesSDiv( CvArr* image, double rho, int srn,
                                  double theta, int stn, int threshold,
                                  float* lines, int linesNumber )
;




  float  cvCalcEMD( const float* signature1, int size1,
                             const float* signature2, int size2,
                             int dims, int dist_type CV_DEFAULT(CV_DIST_L2),
                             CvDistanceFunction dist_func CV_DEFAULT(0),
                             float* lower_bound CV_DEFAULT(0),
                             void* user_param CV_DEFAULT(0))
;


  void  cvKMeans( int num_clusters, float** samples,
                           int num_samples, int vec_size,
                           CvTermCriteria termcrit, int* cluster_idx )
;


 void  cvStartScanGraph( CvGraph* graph, CvGraphScanner* scanner,
                                  CvGraphVtx* vtx CV_DEFAULT(NULL),
                                  int mask CV_DEFAULT(CV_GRAPH_ALL_ITEMS))
;


  void  cvEndScanGraph( CvGraphScanner* scanner )
;



  void  cvLineAA( CvArr* img, CvPoint pt1, CvPoint pt2,
                           double color, int scale CV_DEFAULT(0))
;

  void  cvCircleAA( CvArr* img, CvPoint center, int radius,
                             double color, int scale CV_DEFAULT(0) )
;

  void  cvEllipseAA( CvArr* img, CvPoint center, CvSize axes,
                              double angle, double start_angle,
                              double end_angle, double color,
                              int scale CV_DEFAULT(0) )
;

  void  cvPolyLineAA( CvArr* img, CvPoint** pts, int* npts, int contours,
                               int is_closed, double color, int scale CV_DEFAULT(0) )
;






























































































 void cvUnDistortOnce( const CvArr* src, CvArr* dst,
                                const float* intrinsic_matrix,
                                const float* distortion_coeffs,
                                int CV_UNREFERENCED(interpolate) )
;



 void cvUnDistortInit( const CvArr* CV_UNREFERENCED(src),
                                CvArr* undistortion_map,
                                const float* A, const float* k,
                                int CV_UNREFERENCED(interpolate) )
;

 void  cvUnDistort( const CvArr* src, CvArr* dst,
                             const CvArr* undistortion_map,
                             int CV_UNREFERENCED(interpolate) )
;