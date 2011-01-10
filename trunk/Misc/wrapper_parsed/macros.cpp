#include "macros.hpp"

void cvNextLinePoint(CvLineIterator * LineIterator){
  CV_NEXT_LINE_POINT(*LineIterator);
}

void cvNextSeqElem(int elem_size, CvSeqReader * reader) {
  CV_NEXT_SEQ_ELEM(elem_size, *reader);
}
