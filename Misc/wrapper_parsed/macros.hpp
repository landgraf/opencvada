#include <opencv2/opencv.hpp>
extern "C" {
  void cvNextLinePoint(CvLineIterator LineIterator);
  void cvNextSeqElem(int elem_size, CvSeqReader * reader);
  int Cv_Is_Haar_Classifier(CvHaarClassifierCascade * haar);
  void Cv_Next_Line_Point(CvLineIterator line_iterator);
  //void * Cv_Mat_Elem_Ptr_Fast(CvMat mat, int row, int col, int pix_size);
  void Cv_Write_Seq_Elem_Var(void * elem_ptr, CvSeqWriter writer);
  void Cv_Prev_Seq_Elem(int elem_size, CvSeqReader reader);
  void Cv_Rev_Read_Seq_Elem(void * elem, CvSeqReader reader);
  void Cv_Read_Seq_Elem(void * elem, CvSeqReader reader);
  void Cv_Read_Chain_Point(CvPoint * _pt, CvChainPtReader reader);
  void Cv_Next_Graph_Edge( CvGraphEdge * edge, CvGraphVtx * vertex);
  void Cv_Read_Edge (CvPoint * pt1, CvPoint * pt2, CvChainPtReader * reader);

  int Cv_Maketype(int depth, int channels);

  int Cv_Is_Mat_Hdr(CvMat *mat);
  int Cv_Is_Mat(CvMat *mat);
  int Cv_Is_Mask_Arr(CvMat *mat);
  int Cv_Are_Types_Eq(CvMat *mat1, CvMat *mat2);
  int Cv_Are_Cns_Eq(CvMat *mat1, CvMat *mat2);
  int Cv_Are_Depths_Eq(CvMat *mat1, CvMat *mat2);
  int Cv_Are_Sizes_Eq(CvMat *mat1, CvMat *mat2);
  int Cv_Is_Mat_Const(CvMat *mat);
  int Cv_Mat_Cn(int flags);
  void *Cv_Mat_Elem_Ptr_Fast(CvMat *mat, int row, int col, int pix_size);
  void *Cv_Mat_Elem_Ptr(CvMat *mat, int row, int col);

  int Cv_Is_Mat_ND_Hdr(CvMatND *mat);
  int Cv_Is_Mat_ND(CvMatND *mat);

  int Cv_Is_Sparse_Mat_Hdr(CvSparseMat *mat);
  int Cv_Is_Sparse_Mat(CvSparseMat *mat);
  int Cv_Is_Sparse_Hist(CvHistogram *hist);

  int func_ptr_test(int (*func) (int, int));
  int bounded_array_test(int arr[5]);
  int unbounded_array_test(int *arr, int length);

  typedef int (*func_ptr) (int, int);
  typedef struct {
    func_ptr func;
    int (*func_ptr2) (int, int);
  } func_struct;
}
