#include "core_core_c.h.hpp"

void cvDecRefData_wrap(CvArr * arr ){
	cvDecRefData(/*CvArr*//***/arr);
}
 int cvIncRefData_wrap(CvArr * arr ){
	return cvIncRefData(/*CvArr*//***/arr);
}
 CvMat * cvGetRow_wrap(const CvArr * arr , CvMat * submat , int row ){
	return cvGetRow(/*const*//*CvArr*//***/arr , /*CvMat*//***/submat , /*int*/row);
}
 CvMat * cvGetCol_wrap(const CvArr * arr , CvMat * submat , int col ){
	return cvGetCol(/*const*//*CvArr*//***/arr , /*CvMat*//***/submat , /*int*/col);
}
 void cvReleaseMatND_wrap(CvMatND * * mat ){
	cvReleaseMatND(/*CvMatND*//***//***/mat);
}
 CvSparseNode * cvGetNextSparseNode_wrap(CvSparseMatIterator * mat_iterator ){
	return cvGetNextSparseNode(/*CvSparseMatIterator*//***/mat_iterator);
}
 void cvSubS_wrap(const CvArr * src , CvScalar value , CvArr * dst , const CvArr * mask ){
	cvSubS(/*const*//*CvArr*//***/src , /*CvScalar*/value , /*CvArr*//***/dst , /*const*//*CvArr*//***/mask);
}
 CvSeq * cvCloneSeq_wrap(const CvSeq * seq , CvMemStorage * storage ){
	return cvCloneSeq(/*const*//*CvSeq*//***/seq , /*CvMemStorage*//***/storage);
}
 CvSetElem * cvSetNew_wrap(CvSet * set_header ){
	return cvSetNew(/*CvSet*//***/set_header);
}
 void cvSetRemoveByPtr_wrap(CvSet * set_header , void * elem ){
	cvSetRemoveByPtr(/*CvSet*//***/set_header , /*void*//***/elem);
}
 CvSetElem * cvGetSetElem_wrap(const CvSet * set_header , int index ){
	return cvGetSetElem(/*const*//*CvSet*//***/set_header , /*int*/index);
}
 void cvEllipseBox_wrap(CvArr * img , CvBox2D box , CvScalar color , int thickness , int line_type , int shift ){
	cvEllipseBox(/*CvArr*//***/img , /*CvBox2D*/box , /*CvScalar*/color , /*int*/thickness , /*int*/line_type , /*int*/shift);
}
 CvFont cvFont_wrap(double scale , int thickness ){
	return cvFont(/*double*/scale , /*int*/thickness);
}
 int cvReadInt_wrap(const CvFileNode * node , int default_value ){
	return cvReadInt(/*const*//*CvFileNode*//***/node , /*int*/default_value);
}
 int cvReadIntByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , int default_value ){
	return cvReadIntByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*int*/default_value);
}
 double cvReadReal_wrap(const CvFileNode * node , double default_value ){
	return cvReadReal(/*const*//*CvFileNode*//***/node , /*double*/default_value);
}
 double cvReadRealByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , double default_value ){
	return cvReadRealByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*double*/default_value);
}
 const char * cvReadString_wrap(const CvFileNode * node , const char * default_value ){
	return cvReadString(/*const*//*CvFileNode*//***/node , /*const*//*char*//***/default_value);
}
 const char * cvReadStringByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , const char * default_value ){
	return cvReadStringByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*const*//*char*//***/default_value);
}
 void * cvReadByName_wrap(CvFileStorage * fs , const CvFileNode * map , const char * name , CvAttrList * attributes ){
	return cvReadByName(/*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*CvAttrList*//***/attributes);
}
