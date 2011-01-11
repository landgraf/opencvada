#include <opencv2/opencv.hpp>
#include <opencv2/legacy/legacy.hpp>
extern "C" {
  double icvSqDist2D32f_wrap(CvPoint2D32f pt1 , CvPoint2D32f pt2 );
  Cv3dTracker2dTrackedObject cv3dTracker2dTrackedObject_wrap(int id , CvPoint2D32f p );
  Cv3dTrackerTrackedObject cv3dTrackerTrackedObject_wrap(int id , CvPoint3D32f p );
  int iplWidth_wrap(const IplImage * img );
  int iplHeight_wrap(const IplImage * img );
}
