#include "macros.hpp"

void cvNextLinePoint(CvLineIterator LineIterator){
  CV_NEXT_LINE_POINT(LineIterator);
}

void cvNextSeqElem(int elem_size, CvSeqReader * reader) {
  CV_NEXT_SEQ_ELEM(elem_size, *reader);
}

int Cv_Is_Haar_Classifier(CvHaarClassifierCascade * haar)
{
  if CV_IS_HAAR_CLASSIFIER(haar)
    return 1;
  else
    return 0;
}
void Cv_Next_Line_Point(CvLineIterator line_iterator)
{
  CV_NEXT_LINE_POINT(line_iterator);
}

void * Cv_Mat_Elem_Ptr_Fast(CvMat mat, int row, int col, int pix_size)
{
  return CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size );
}

void Cv_Write_Seq_Elem_Var(void * elem_ptr, CvSeqWriter writer)
{
  CV_WRITE_SEQ_ELEM_VAR( elem_ptr, writer );
}

void Cv_Prev_Seq_Elem(int elem_size, CvSeqReader reader)
{
  CV_PREV_SEQ_ELEM(elem_size,reader);
}

void Cv_Rev_Read_Seq_Elem(void * elem, CvSeqReader reader)
{
  CV_REV_READ_SEQ_ELEM(elem,reader);
}

void Cv_Read_Seq_Elem(void * elem, CvSeqReader reader)
{
  CV_READ_SEQ_ELEM(elem,reader);
}

void Cv_Read_Chain_Point(CvPoint * _pt, CvChainPtReader reader)
{
  CV_READ_CHAIN_POINT((*_pt),reader);
}

void Cv_Next_Graph_Edge( CvGraphEdge * edge, CvGraphVtx * vertex)
{
  CV_NEXT_GRAPH_EDGE( edge, vertex );
}

void Cv_Read_Edge (CvPoint * pt1, CvPoint * pt2, CvChainPtReader * reader)
{
  CV_READ_EDGE( (*pt1), (*pt2), (*reader) );
}

