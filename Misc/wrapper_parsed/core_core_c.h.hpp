#include <opencv2/opencv.hpp>

extern "C" {

  void cvDecRefData_wrap(CvArr * arr );
  int cvIncRefData_wrap(CvArr * arr );
  CvMat * cvGetRow_wrap(const CvArr * arr , CvMat * submat , int row );
  CvMat * cvGetCol_wrap(const CvArr * arr , CvMat * submat , int col );
  void cvReleaseMatND_wrap(CvMatND * * mat );
  CvSparseNode * cvGetNextSparseNode_wrap(CvSparseMatIterator * mat_iterator );
  void cvSubS_wrap(const CvArr * src , CvScalar value , CvArr * dst , const CvArr * mask );
  CvSeq * cvCloneSeq_wrap(const CvSeq * seq , CvMemStorage * storage );
  CvSetElem * cvSetNew_wrap(CvSet * set_header );
  void cvSetRemoveByPtr_wrap(CvSet * set_header , void * elem );
  CvSetElem * cvGetSetElem_wrap(const CvSet * set_header , int index );
  void cvEllipseBox_wrap(CvArr * img , CvBox2D box , CvScalar color , int thickness , int line_type , int shift );
  CvFont cvFont_wrap(double scale , int thickness );
  int cvReadInt_wrap(const CvFileNode * node , int default_value );
  int cvReadIntByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , int default_value );
  double cvReadReal_wrap(const CvFileNode * node , double default_value );
  double cvReadRealByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , double default_value );
  const char * cvReadString_wrap(const CvFileNode * node , const char * default_value );
  const char * cvReadStringByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , const char * default_value );
  void * cvReadByName_wrap(CvFileStorage * fs , const CvFileNode * map , const char * name , CvAttrList * attributes );

  void *cvMatElem_wrap(CvMat *mat, int elemSize, int row, int col);

  CvPoint2D32f cvPoint2D32f_wrap(double x, double y);

  CvMat CvMat_wrap(int rows, int cols, int type, void *data);

  void *CvNodeVal_wrap(CvSparseNode *node, CvSparseMat *mat);
  int *CvNodeIdx_wrap(CvSparseNode *node, CvSparseMat *mat);

}
