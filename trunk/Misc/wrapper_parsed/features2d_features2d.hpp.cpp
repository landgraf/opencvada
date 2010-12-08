 CvSURFPoint cvSURFPoint_wrap(CvPoint2D32f pt , int laplacian , int size , float dir , float hessian ){
	return cvSURFPoint(/*CvPoint2D32f*/pt , /*int*/laplacian , /*int*/size , /*float*/dir , /*float*/hessian);
}
 CvStarKeypoint cvStarKeypoint_wrap(CvPoint pt , int size , float response ){
	return cvStarKeypoint(/*CvPoint*/pt , /*int*/size , /*float*/response);
}
 CvStarDetectorParams cvStarDetectorParams_wrap(int maxSize , int responseThreshold , int lineThresholdProjected , int lineThresholdBinarized , int suppressNonmaxSize ){
	return cvStarDetectorParams(/*int*/maxSize , /*int*/responseThreshold , /*int*/lineThresholdProjected , /*int*/lineThresholdBinarized , /*int*/suppressNonmaxSize);
}
