




typedef 

typedef 


 int (CV_CDECL *CvIsInstanceFunc)( const void* struct_ptr );
 void (CV_CDECL *CvReleaseFunc)( void** struct_dblptr );
 void* (CV_CDECL *CvReadFunc)( CvFileStorage* storage, CvFileNode* node );
 void (CV_CDECL *CvWriteFunc)( CvFileStorage* storage, const char* name,
                                      const void* struct_ptr, CvAttrList attributes );
 void* (CV_CDECL *CvCloneFunc)( const void* struct_ptr );