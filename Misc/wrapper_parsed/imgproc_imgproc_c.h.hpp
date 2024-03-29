#include <opencv2/opencv.hpp>
extern "C" {
  CvSubdiv2D * cvCreateSubdivDelaunay2D_wrap(CvRect rect , CvMemStorage * storage );
  CvSubdiv2DEdge cvSubdiv2DNextEdge_wrap(CvSubdiv2DEdge edge );
  CvSubdiv2DEdge cvSubdiv2DRotateEdge_wrap(CvSubdiv2DEdge edge , int rotate );
  CvSubdiv2DEdge cvSubdiv2DSymEdge_wrap(CvSubdiv2DEdge edge );
  CvSubdiv2DEdge cvSubdiv2DGetEdge_wrap(CvSubdiv2DEdge edge , CvNextEdgeType type );
  CvSubdiv2DPoint * cvSubdiv2DEdgeOrg_wrap(CvSubdiv2DEdge edge );
  CvSubdiv2DPoint * cvSubdiv2DEdgeDst_wrap(CvSubdiv2DEdge edge );
  double cvTriangleArea_wrap(CvPoint2D32f a , CvPoint2D32f b , CvPoint2D32f c );
  double cvContourPerimeter_wrap(const void * contour );
  void cvCalcHist_wrap(IplImage * * image , CvHistogram * hist , int accumulate , const CvArr * mask );
}
