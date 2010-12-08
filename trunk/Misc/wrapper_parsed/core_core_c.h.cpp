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
 void cvSubS_wrap(const CvArr * src , CvScalar value , CvArr * dst , const CvArr * mask CV_DEFAULT NULL ){
	cvSubS(/*const*//*CvArr*//***/src , /*CvScalar*/value , /*CvArr*//***/dst , /*const*//*CvArr*//***//*mask*//*CV_DEFAULT*/NULL);
}
 CvSeq * cvCloneSeq_wrap(const CvSeq * seq , CvMemStorage * storage CV_DEFAULT NULL ){
	return cvCloneSeq(/*const*//*CvSeq*//***/seq , /*CvMemStorage*//***//*storage*//*CV_DEFAULT*/NULL);
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
 void cvEllipseBox_wrap(CvArr * img , CvBox2D box , CvScalar color , int thickness CV_DEFAULT 1 , int line_type CV_DEFAULT 8 , int shift CV_DEFAULT 0 ){
	cvEllipseBox(/*CvArr*//***/img , /*CvBox2D*/box , /*CvScalar*/color , /*int*//*thickness*//*CV_DEFAULT*/1 , /*int*//*line_type*//*CV_DEFAULT*/8 , /*int*//*shift*//*CV_DEFAULT*/0);
}
 CvFont cvFont_wrap(double scale , int thickness CV_DEFAULT 1 ){
	return cvFont(/*double*/scale , /*int*//*thickness*//*CV_DEFAULT*/1);
}
 int cvReadInt_wrap(const CvFileNode * node , int default_value CV_DEFAULT 0 ){
	return cvReadInt(/*const*//*CvFileNode*//***/node , /*int*//*default_value*//*CV_DEFAULT*/0);
}
 int cvReadIntByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , int default_value CV_DEFAULT 0 ){
	return cvReadIntByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*int*//*default_value*//*CV_DEFAULT*/0);
}
 double cvReadReal_wrap(const CvFileNode * node , double default_value CV_DEFAULT 0. ){
	return cvReadReal(/*const*//*CvFileNode*//***/node , /*double*//*default_value*//*CV_DEFAULT*/0.);
}
 double cvReadRealByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , double default_value CV_DEFAULT 0. ){
	return cvReadRealByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*double*//*default_value*//*CV_DEFAULT*/0.);
}
 const char * cvReadString_wrap(const CvFileNode * node , const char * default_value CV_DEFAULT NULL ){
	return cvReadString(/*const*//*CvFileNode*//***/node , /*const*//*char*//***//*default_value*//*CV_DEFAULT*/NULL);
}
 const char * cvReadStringByName_wrap(const CvFileStorage * fs , const CvFileNode * map , const char * name , const char * default_value CV_DEFAULT NULL ){
	return cvReadStringByName(/*const*//*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*const*//*char*//***//*default_value*//*CV_DEFAULT*/NULL);
}
 void * cvReadByName_wrap(CvFileStorage * fs , const CvFileNode * map , const char * name , CvAttrList * attributes CV_DEFAULT NULL ){
	return cvReadByName(/*CvFileStorage*//***/fs , /*const*//*CvFileNode*//***/map , /*const*//*char*//***/name , /*CvAttrList*//***//*attributes*//*CV_DEFAULT*/NULL);
}
