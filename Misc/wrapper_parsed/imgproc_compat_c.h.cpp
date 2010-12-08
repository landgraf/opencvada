 CvMat cvMatArray_wrap(int rows , int cols , int type , int count , void * data CV_DEFAULT 0 ){
	return cvMatArray(/*int*/rows , /*int*/cols , /*int*/type , /*int*/count , /*void*//***//*data*//*CV_DEFAULT*/0);
}
 double cvMean_wrap(const CvArr * image , const CvArr * mask CV_DEFAULT 0 ){
	return cvMean(/*const*//*CvArr*//***/image , /*const*//*CvArr*//***//*mask*//*CV_DEFAULT*/0);
}
 double cvSumPixels_wrap(const CvArr * image ){
	return cvSumPixels(/*const*//*CvArr*//***/image);
}
 void cvMean_StdDev_wrap(const CvArr * image , double * mean , double * sdv , const CvArr * mask CV_DEFAULT 0 ){
	cvMean_StdDev(/*const*//*CvArr*//***/image , /*double*//***/mean , /*double*//***/sdv , /*const*//*CvArr*//***//*mask*//*CV_DEFAULT*/0);
}
 void cvmPerspectiveProject_wrap(const CvMat * mat , const CvArr * src , CvArr * dst ){
	cvmPerspectiveProject(/*const*//*CvMat*//***/mat , /*const*//*CvArr*//***/src , /*CvArr*//***/dst);
}
 void cvFillImage_wrap(CvArr * mat , double color ){
	cvFillImage(/*CvArr*//***/mat , /*double*/color);
}
 void cvRandSetRange_wrap(CvRandState * state , double param1 , double param2 , int index CV_DEFAULT -1 ){
	cvRandSetRange(/*CvRandState*//***/state , /*double*/param1 , /*double*/param2 , /*int*//*index*//*CV_DEFAULT*/-1);
}
 void cvRandInit_wrap(CvRandState * state , double param1 , double param2 , int seed , int disttype CV_DEFAULT CV_RAND_UNI ){
	cvRandInit(/*CvRandState*//***/state , /*double*/param1 , /*double*/param2 , /*int*/seed , /*int*//*disttype*//*CV_DEFAULT*/CV_RAND_UNI);
}
 void cvRand_wrap(CvRandState * state , CvArr * arr ){
	cvRand(/*CvRandState*//***/state , /*CvArr*//***/arr);
}
 void cvbRand_wrap(CvRandState * state , float * dst , int len ){
	cvbRand(/*CvRandState*//***/state , /*float*//***/dst , /*int*/len);
}
 void cvbCartToPolar_wrap(const float * y , const float * x , float * magnitude , float * angle , int len ){
	cvbCartToPolar(/*const*//*float*//***/y , /*const*//*float*//***/x , /*float*//***/magnitude , /*float*//***/angle , /*int*/len);
}
 void cvbFastArctan_wrap(const float * y , const float * x , float * angle , int len ){
	cvbFastArctan(/*const*//*float*//***/y , /*const*//*float*//***/x , /*float*//***/angle , /*int*/len);
}
 void cvbSqrt_wrap(const float * x , float * y , int len ){
	cvbSqrt(/*const*//*float*//***/x , /*float*//***/y , /*int*/len);
}
 void cvbInvSqrt_wrap(const float * x , float * y , int len ){
	cvbInvSqrt(/*const*//*float*//***/x , /*float*//***/y , /*int*/len);
}
 void cvbReciprocal_wrap(const float * x , float * y , int len ){
	cvbReciprocal(/*const*//*float*//***/x , /*float*//***/y , /*int*/len);
}
 void cvbFastExp_wrap(const float * x , double * y , int len ){
	cvbFastExp(/*const*//*float*//***/x , /*double*//***/y , /*int*/len);
}
 void cvbFastLog_wrap(const double * x , float * y , int len ){
	cvbFastLog(/*const*//*double*//***/x , /*float*//***/y , /*int*/len);
}
 CvRect cvContourBoundingRect_wrap(void * point_set , int update CV_DEFAULT 0 ){
	return cvContourBoundingRect(/*void*//***/point_set , /*int*//*update*//*CV_DEFAULT*/0);
}
 double cvPseudoInverse_wrap(const CvArr * src , CvArr * dst ){
	return cvPseudoInverse(/*const*//*CvArr*//***/src , /*CvArr*//***/dst);
}
 void cvConvexHull_wrap(CvPoint * points , int num_points , CvRect * CV_UNREFERENCED bound_rect , int orientation , int * hull , int * hullsize ){
	cvConvexHull(/*CvPoint*//***/points , /*int*/num_points , /*CvRect*//***//*CV_UNREFERENCED*/bound_rect , /*int*/orientation , /*int*//***/hull , /*int*//***/hullsize);
}
 void cvMinAreaRect_wrap(CvPoint * points , int n , int CV_UNREFERENCED left , int CV_UNREFERENCED bottom , int CV_UNREFERENCED right , int CV_UNREFERENCED top , CvPoint2D32f * anchor , CvPoint2D32f * vect1 , CvPoint2D32f * vect2 ){
	cvMinAreaRect(/*CvPoint*//***/points , /*int*/n , /*int*//*CV_UNREFERENCED*/left , /*int*//*CV_UNREFERENCED*/bottom , /*int*//*CV_UNREFERENCED*/right , /*int*//*CV_UNREFERENCED*/top , /*CvPoint2D32f*//***/anchor , /*CvPoint2D32f*//***/vect1 , /*CvPoint2D32f*//***/vect2);
}
 void cvFitLine3D_wrap(CvPoint3D32f * points , int count , int dist , void * param , float reps , float aeps , float * line ){
	cvFitLine3D(/*CvPoint3D32f*//***/points , /*int*/count , /*int*/dist , /*void*//***/param , /*float*/reps , /*float*/aeps , /*float*//***/line);
}
 void cvFitLine2D_wrap(CvPoint2D32f * points , int count , int dist , void * param , float reps , float aeps , float * line ){
	cvFitLine2D(/*CvPoint2D32f*//***/points , /*int*/count , /*int*/dist , /*void*//***/param , /*float*/reps , /*float*/aeps , /*float*//***/line);
}
 void cvFitEllipse_wrap(const CvPoint2D32f * points , int count , CvBox2D * box ){
	cvFitEllipse(/*const*//*CvPoint2D32f*//***/points , /*int*/count , /*CvBox2D*//***/box);
}
 void cvProject3D_wrap(CvPoint3D32f * points3D , int count , CvPoint2D32f * points2D , int xIndx CV_DEFAULT 0 , int yIndx CV_DEFAULT 1 ){
	cvProject3D(/*CvPoint3D32f*//***/points3D , /*int*/count , /*CvPoint2D32f*//***/points2D , /*int*//*xIndx*//*CV_DEFAULT*/0 , /*int*//*yIndx*//*CV_DEFAULT*/1);
}
 int cvHoughLines_wrap(CvArr * image , double rho , double theta , int threshold , float * lines , int linesNumber ){
	return cvHoughLines(/*CvArr*//***/image , /*double*/rho , /*double*/theta , /*int*/threshold , /*float*//***/lines , /*int*/linesNumber);
}
 int cvHoughLinesP_wrap(CvArr * image , double rho , double theta , int threshold , int lineLength , int lineGap , int * lines , int linesNumber ){
	return cvHoughLinesP(/*CvArr*//***/image , /*double*/rho , /*double*/theta , /*int*/threshold , /*int*/lineLength , /*int*/lineGap , /*int*//***/lines , /*int*/linesNumber);
}
 int cvHoughLinesSDiv_wrap(CvArr * image , double rho , int srn , double theta , int stn , int threshold , float * lines , int linesNumber ){
	return cvHoughLinesSDiv(/*CvArr*//***/image , /*double*/rho , /*int*/srn , /*double*/theta , /*int*/stn , /*int*/threshold , /*float*//***/lines , /*int*/linesNumber);
}
 float cvCalcEMD_wrap(const float * signature1 , int size1 , const float * signature2 , int size2 , int dims , int dist_type CV_DEFAULT CV_DIST_L2 , CvDistanceFunction dist_func CV_DEFAULT 0 , float * lower_bound CV_DEFAULT 0 , void * user_param CV_DEFAULT 0 ){
	return cvCalcEMD(/*const*//*float*//***/signature1 , /*int*/size1 , /*const*//*float*//***/signature2 , /*int*/size2 , /*int*/dims , /*int*//*dist_type*//*CV_DEFAULT*/CV_DIST_L2 , /*CvDistanceFunction*//*dist_func*//*CV_DEFAULT*/0 , /*float*//***//*lower_bound*//*CV_DEFAULT*/0 , /*void*//***//*user_param*//*CV_DEFAULT*/0);
}
 void cvKMeans_wrap(int num_clusters , float * * samples , int num_samples , int vec_size , CvTermCriteria termcrit , int * cluster_idx ){
	cvKMeans(/*int*/num_clusters , /*float*//***//***/samples , /*int*/num_samples , /*int*/vec_size , /*CvTermCriteria*/termcrit , /*int*//***/cluster_idx);
}
 void cvStartScanGraph_wrap(CvGraph * graph , CvGraphScanner * scanner , CvGraphVtx * vtx CV_DEFAULT NULL , int mask CV_DEFAULT CV_GRAPH_ALL_ITEMS ){
	cvStartScanGraph(/*CvGraph*//***/graph , /*CvGraphScanner*//***/scanner , /*CvGraphVtx*//***//*vtx*//*CV_DEFAULT*/NULL , /*int*//*mask*//*CV_DEFAULT*/CV_GRAPH_ALL_ITEMS);
}
 void cvEndScanGraph_wrap(CvGraphScanner * scanner ){
	cvEndScanGraph(/*CvGraphScanner*//***/scanner);
}
 void cvLineAA_wrap(CvArr * img , CvPoint pt1 , CvPoint pt2 , double color , int scale CV_DEFAULT 0 ){
	cvLineAA(/*CvArr*//***/img , /*CvPoint*/pt1 , /*CvPoint*/pt2 , /*double*/color , /*int*//*scale*//*CV_DEFAULT*/0);
}
 void cvCircleAA_wrap(CvArr * img , CvPoint center , int radius , double color , int scale CV_DEFAULT 0 ){
	cvCircleAA(/*CvArr*//***/img , /*CvPoint*/center , /*int*/radius , /*double*/color , /*int*//*scale*//*CV_DEFAULT*/0);
}
 void cvEllipseAA_wrap(CvArr * img , CvPoint center , CvSize axes , double angle , double start_angle , double end_angle , double color , int scale CV_DEFAULT 0 ){
	cvEllipseAA(/*CvArr*//***/img , /*CvPoint*/center , /*CvSize*/axes , /*double*/angle , /*double*/start_angle , /*double*/end_angle , /*double*/color , /*int*//*scale*//*CV_DEFAULT*/0);
}
 void cvPolyLineAA_wrap(CvArr * img , CvPoint * * pts , int * npts , int contours , int is_closed , double color , int scale CV_DEFAULT 0 ){
	cvPolyLineAA(/*CvArr*//***/img , /*CvPoint*//***//***/pts , /*int*//***/npts , /*int*/contours , /*int*/is_closed , /*double*/color , /*int*//*scale*//*CV_DEFAULT*/0);
}
 void cvUnDistortOnce_wrap(const CvArr * src , CvArr * dst , const float * intrinsic_matrix , const float * distortion_coeffs , int CV_UNREFERENCED interpolate ){
	cvUnDistortOnce(/*const*//*CvArr*//***/src , /*CvArr*//***/dst , /*const*//*float*//***/intrinsic_matrix , /*const*//*float*//***/distortion_coeffs , /*int*//*CV_UNREFERENCED*/interpolate);
}
 void cvUnDistortInit_wrap(const CvArr * CV_UNREFERENCED src , CvArr * undistortion_map , const float * A , const float * k , int CV_UNREFERENCED interpolate ){
	cvUnDistortInit(/*const*//*CvArr*//***//*CV_UNREFERENCED*/src , /*CvArr*//***/undistortion_map , /*const*//*float*//***/A , /*const*//*float*//***/k , /*int*//*CV_UNREFERENCED*/interpolate);
}
 void cvUnDistort_wrap(const CvArr * src , CvArr * dst , const CvArr * undistortion_map , int CV_UNREFERENCED interpolate ){
	cvUnDistort(/*const*//*CvArr*//***/src , /*CvArr*//***/dst , /*const*//*CvArr*//***/undistortion_map , /*int*//*CV_UNREFERENCED*/interpolate);
}
