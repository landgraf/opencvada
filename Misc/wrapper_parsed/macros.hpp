#include <opencv2/opencv.hpp>

extern "C" {
  void cvNextLinePoint(CvLineIterator * LineIterator);
  void cvNextSeqElem(int elem_size, CvSeqReader * reader);
  int find_obj_locatePlanarObject( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
				  const CvSeq* imageKeypoints, const CvSeq* imageDescriptors,
				  const CvPoint src_corners[4], CvPoint dst_corners[4] );
  void find_obj_findPairs( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
			const CvSeq* imageKeypoints, const CvSeq* imageDescriptors, int ** c_ptpairs, int * length );
  int find_obj_naiveNearestNeighbor( const float* vec, int laplacian,
				    const CvSeq* model_keypoints,
				    const CvSeq* model_descriptors );
  double find_obj_compareSURFDescriptors( const float* d1, const float* d2, double best, int length );
}
