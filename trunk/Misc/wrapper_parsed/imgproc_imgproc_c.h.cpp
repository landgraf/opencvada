CvSubdiv2D * cvCreateSubdivDelaunay2D (CvRect rect , CvMemStorage * storage ){
	return cvCreateSubdivDelaunay2D(/*CvRect*/rect , /*CvMemStorage*//***/storage);
}
CvSubdiv2DEdge cvSubdiv2DNextEdge (CvSubdiv2DEdge edge ){
	return cvSubdiv2DNextEdge(/*CvSubdiv2DEdge*/edge);
}
CvSubdiv2DEdge cvSubdiv2DRotateEdge (CvSubdiv2DEdge edge , int rotate ){
	return cvSubdiv2DRotateEdge(/*CvSubdiv2DEdge*/edge , /*int*/rotate);
}
CvSubdiv2DEdge cvSubdiv2DSymEdge (CvSubdiv2DEdge edge ){
	return cvSubdiv2DSymEdge(/*CvSubdiv2DEdge*/edge);
}
CvSubdiv2DEdge cvSubdiv2DGetEdge (CvSubdiv2DEdge edge , CvNextEdgeType type ){
	return cvSubdiv2DGetEdge(/*CvSubdiv2DEdge*/edge , /*CvNextEdgeType*/type);
}
CvSubdiv2DPoint * cvSubdiv2DEdgeOrg (CvSubdiv2DEdge edge ){
	return cvSubdiv2DEdgeOrg(/*CvSubdiv2DEdge*/edge);
}
CvSubdiv2DPoint * cvSubdiv2DEdgeDst (CvSubdiv2DEdge edge ){
	return cvSubdiv2DEdgeDst(/*CvSubdiv2DEdge*/edge);
}
double cvTriangleArea (CvPoint2D32f a , CvPoint2D32f b , CvPoint2D32f c ){
	return cvTriangleArea(/*CvPoint2D32f*/a , /*CvPoint2D32f*/b , /*CvPoint2D32f*/c);
}
double cvContourPerimeter (const void * contour ){
	return cvContourPerimeter(/*const*//*void*//***/contour);
}
void cvCalcHist (IplImage * * image , CvHistogram * hist , int accumulate CV_DEFAULT 0 , const CvArr * mask CV_DEFAULT NULL ){
	cvCalcHist(/*IplImage*//***//***/image , /*CvHistogram*//***/hist , /*int*//*accumulate*//*CV_DEFAULT*/0 , /*const*//*CvArr*//***//*mask*//*CV_DEFAULT*/NULL);
}
