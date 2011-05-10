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

int Cv_Is_Mat_Hdr(CvMat *mat) {
  return CV_IS_MAT_HDR(mat);
}

int Cv_Is_Mat(CvMat *mat) {
  return CV_IS_MAT(mat);
}

int Cv_Is_Mask_Arr(CvMat *mat) {
  return CV_IS_MASK_ARR(mat);
}

int Cv_Are_Types_Eq(CvMat *mat1, CvMat *mat2) {
  return CV_ARE_TYPES_EQ(mat1, mat2);
}

int Cv_Are_Cns_Eq(CvMat *mat1, CvMat *mat2) {
  return CV_ARE_CNS_EQ(mat1, mat2);
}

int Cv_Are_Depths_Eq(CvMat *mat1, CvMat *mat2) {
  return CV_ARE_DEPTHS_EQ(mat1, mat2);
}

int Cv_Are_Sizes_Eq(CvMat *mat1, CvMat *mat2) {
  return CV_ARE_SIZES_EQ(mat1, mat2);
}

int Cv_Is_Mat_Const(CvMat *mat) {
  return CV_IS_MAT_CONST(mat);
}

int Cv_Mat_Cn(int flags) {
  return CV_MAT_CN(flags);
}

void *Cv_Mat_Elem_Ptr_Fast(CvMat *mat, int row, int col, int pix_size) {
  return CV_MAT_ELEM_PTR_FAST(*mat, row, col, pix_size);
}

void *Cv_Mat_Elem_Ptr(CvMat *mat, int row, int col) {
  return CV_MAT_ELEM_PTR(*mat, row, col);
}

int Cv_Is_Mat_ND_Hdr(CvMatND *mat) {
  return CV_IS_MATND_HDR(mat);
}

int Cv_Is_Mat_ND(CvMatND *mat) {
  return CV_IS_MATND(mat);
}

int Cv_Is_Sparse_Mat_Hdr(CvSparseMat *mat) {
  return CV_IS_SPARSE_MAT_HDR(mat);
}

int Cv_Is_Sparse_Mat(CvSparseMat *mat) {
  return CV_IS_SPARSE_MAT(mat);
}

int Cv_Is_Sparse_Hist(CvHistogram *hist) {
  return CV_IS_SPARSE_HIST(hist);
}

int Cv_Maketype(int depth, int channels) {
  return CV_MAKETYPE(depth, channels);
}

int func_ptr_test(int (*func) (int, int)) {
  return func(30, 20);
}

int bounded_array_test(int arr[5]) {
  int max = arr[0];

  for (int i = 1; i < 10; i++) {
    if(arr[i] > max)
      max = arr[i];
  }
  return max;
}

int unbounded_array_test(int *arr, int length) {
  int max = arr[0];

  for(int i = 1; i < length; i++) {
    if (max < arr[i]) {
      max = arr[i];
      arr[i] = 0;
    }
  }
  return max;
}
