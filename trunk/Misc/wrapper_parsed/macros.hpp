#include <opencv2/opencv.hpp>

extern "C" {
  void cvNextLinePoint(CvLineIterator * LineIterator);
  void cvNextSeqElem(int elem_size, CvSeqReader * reader);
}
