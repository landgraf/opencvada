#include "legacy_legacy.hpp.hpp"

double icvSqDist2D32f_wrap(CvPoint2D32f pt1 , CvPoint2D32f pt2 ){
	return icvSqDist2D32f(/*CvPoint2D32f*/pt1 , /*CvPoint2D32f*/pt2);
}
 Cv3dTracker2dTrackedObject cv3dTracker2dTrackedObject_wrap(int id , CvPoint2D32f p ){
	return cv3dTracker2dTrackedObject(/*int*/id , /*CvPoint2D32f*/p);
}
 Cv3dTrackerTrackedObject cv3dTrackerTrackedObject_wrap(int id , CvPoint3D32f p ){
	return cv3dTrackerTrackedObject(/*int*/id , /*CvPoint3D32f*/p);
}
 int iplWidth_wrap(const IplImage * img ){
	return iplWidth(/*const*//*IplImage*//***/img);
}
 int iplHeight_wrap(const IplImage * img ){
	return iplHeight(/*const*//*IplImage*//***/img);
}
