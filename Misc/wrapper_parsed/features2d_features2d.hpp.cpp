 CvSURFPoint cvSURFPoint_wrap(CvPoint2D32f pt , int laplacian , int size , float dir CV_DEFAULT 0 , float hessian CV_DEFAULT 0 ){
	return cvSURFPoint(/*CvPoint2D32f*/pt , /*int*/laplacian , /*int*/size , /*float*//*dir*//*CV_DEFAULT*/0 , /*float*//*hessian*//*CV_DEFAULT*/0);
}
 CvStarKeypoint cvStarKeypoint_wrap(CvPoint pt , int size , float response ){
	return cvStarKeypoint(/*CvPoint*/pt , /*int*/size , /*float*/response);
}
 CvStarDetectorParams cvStarDetectorParams_wrap(int maxSize CV_DEFAULT 45 , int responseThreshold CV_DEFAULT 30 , int lineThresholdProjected CV_DEFAULT 10 , int lineThresholdBinarized CV_DEFAULT 8 , int suppressNonmaxSize CV_DEFAULT 5 ){
	return cvStarDetectorParams(/*int*//*maxSize*//*CV_DEFAULT*/45 , /*int*//*responseThreshold*//*CV_DEFAULT*/30 , /*int*//*lineThresholdProjected*//*CV_DEFAULT*/10 , /*int*//*lineThresholdBinarized*//*CV_DEFAULT*/8 , /*int*//*suppressNonmaxSize*//*CV_DEFAULT*/5);
}
