


   			













CVAPI(CvFont) cvFontQt(const char* nameFont, int pointSize CV_DEFAULT(-1), CvScalar color CV_DEFAULT(cvScalarAll(0)), int weight CV_DEFAULT(CV_FONT_NORMAL),  int style CV_DEFAULT(CV_STYLE_NORMAL), int spacing CV_DEFAULT(0));

CVAPI(void) cvAddText(const CvArr* img, const char* text, CvPoint org, CvFont *arg2);

CVAPI(void) cvDisplayOverlay(const char* name, const char* text, int delayms);
CVAPI(void) cvDisplayStatusBar(const char* name, const char* text, int delayms);

 void (CV_CDECL *CvOpenGLCallback)(void* userdata);
CVAPI(void) cvCreateOpenGLCallback( const char* window_name, CvOpenGLCallback callbackOpenGL, void* userdata CV_DEFAULT(NULL), double angle CV_DEFAULT(-1), double zmin CV_DEFAULT(-1), double zmax CV_DEFAULT(-1));

CVAPI(void) cvSaveWindowParameters(const char* name);
CVAPI(void) cvLoadWindowParameters(const char* name);
CVAPI(int) cvStartLoop(int (*pt2Func)(int argc, char *argv[]), int argc, char* argv[]);
CVAPI(void) cvStopLoop();

 void (CV_CDECL *CvButtonCallback)(int state, void* userdata);

CVAPI(int) cvCreateButton( const char* button_name CV_DEFAULT(NULL),CvButtonCallback on_change CV_DEFAULT(NULL), void* userdata CV_DEFAULT(NULL) , int button_type CV_DEFAULT(CV_PUSH_BUTTON), int initial_button_state CV_DEFAULT(0));




CVAPI(int) cvInitSystem( int argc, char** argv );

CVAPI(int) cvStartWindowThread();





CVAPI(int) cvNamedWindow( const char* name, int flags CV_DEFAULT(CV_WINDOW_AUTOSIZE) );


CVAPI(void) cvSetWindowProperty(const char* name, int prop_id, double prop_value);
CVAPI(double) cvGetWindowProperty(const char* name, int prop_id);


CVAPI(void) cvShowImage( const char* name, const CvArr* image );


CVAPI(void) cvResizeWindow( const char* name, int width, int height );
CVAPI(void) cvMoveWindow( const char* name, int x, int y );



CVAPI(void) cvDestroyWindow( const char* name );

CVAPI(void) cvDestroyAllWindows(void);


CVAPI(void*) cvGetWindowHandle( const char* name );


CVAPI(const char*) cvGetWindowName( void* window_handle );


 void (CV_CDECL *CvTrackbarCallback)(int pos);


CVAPI(int) cvCreateTrackbar( const char* trackbar_name, const char* window_name,
                             int* value, int count, CvTrackbarCallback on_change CV_DEFAULT(NULL));

 void (CV_CDECL *CvTrackbarCallback2)(int pos, void* userdata);

CVAPI(int) cvCreateTrackbar2( const char* trackbar_name, const char* window_name,
                              int* value, int count, CvTrackbarCallback2 on_change,
                              void* userdata CV_DEFAULT(0));


CVAPI(int) cvGetTrackbarPos( const char* trackbar_name, const char* window_name );
CVAPI(void) cvSetTrackbarPos( const char* trackbar_name, const char* window_name, int pos );





 void (CV_CDECL *CvMouseCallback )(int event, int x, int y, int flags, void* param);


CVAPI(void) cvSetMouseCallback( const char* window_name, CvMouseCallback on_mouse,
                                void* param CV_DEFAULT(NULL));




CVAPI(IplImage*) cvLoadImage( const char* filename, int iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));
CVAPI(CvMat*) cvLoadImageM( const char* filename, int iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));




CVAPI(int) cvSaveImage( const char* filename, const CvArr* image,
                        const int* params CV_DEFAULT(0) );


CVAPI(IplImage*) cvDecodeImage( const CvMat* buf, int iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));
CVAPI(CvMat*) cvDecodeImageM( const CvMat* buf, int iscolor CV_DEFAULT(CV_LOAD_IMAGE_COLOR));


CVAPI(CvMat*) cvEncodeImage( const char* ext, const CvArr* image,
                             const int* params CV_DEFAULT(0) );




CVAPI(void) cvConvertImage( const CvArr* src, CvArr* dst, int flags CV_DEFAULT(0));


CVAPI(int) cvWaitKey(int delay CV_DEFAULT(0));




 


CVAPI(CvCapture*) cvCreateCameraCapture( int index );


CVAPI(int) cvGrabFrame( CvCapture* capture );


CVAPI(IplImage*) cvRetrieveFrame( CvCapture* capture, int streamIdx CV_DEFAULT(0) );


CVAPI(IplImage*) cvQueryFrame( CvCapture* capture );


CVAPI(void) cvReleaseCapture( CvCapture** capture );




CVAPI(double) cvGetCaptureProperty( CvCapture* capture, int property_id );
CVAPI(int)    cvSetCaptureProperty( CvCapture* capture, int property_id, double value );


CVAPI(int)    cvGetCaptureDomain( CvCapture* capture);  


 





CVAPI(int) cvWriteFrame( CvVideoWriter* writer, const IplImage* image );


CVAPI(void) cvReleaseVideoWriter( CvVideoWriter** writer );





 int (CV_CDECL * CvWin32WindowCallback)(HWND, UINT, WPARAM, LPARAM, int*);
CVAPI(void) cvSetPreprocessFuncWin32( CvWin32WindowCallback on_preprocess );
CVAPI(void) cvSetPostprocessFuncWin32( CvWin32WindowCallback on_postprocess );