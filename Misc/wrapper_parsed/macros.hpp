#include <opencv2/opencv.hpp>
extern "C" {
  void cvNextLinePoint(CvLineIterator LineIterator);
  void cvNextSeqElem(int elem_size, CvSeqReader * reader);
  int Cv_Is_Haar_Classifier(CvHaarClassifierCascade * haar);
  void Cv_Next_Line_Point(CvLineIterator line_iterator);
  void * Cv_Mat_Elem_Ptr_Fast(CvMat mat, int row, int col, int pix_size);
  void Cv_Write_Seq_Elem_Var(void * elem_ptr, CvSeqWriter writer);
  void Cv_Prev_Seq_Elem(int elem_size, CvSeqReader reader);
  void Cv_Rev_Read_Seq_Elem(void * elem, CvSeqReader reader);
  void Cv_Read_Seq_Elem(void * elem, CvSeqReader reader);
  void Cv_Read_Chain_Point(CvPoint * _pt, CvChainPtReader reader);
  void Cv_Next_Graph_Edge( CvGraphEdge * edge, CvGraphVtx * vertex);
  void Cv_Read_Edge (CvPoint * pt1, CvPoint * pt2, CvChainPtReader * reader);
}
