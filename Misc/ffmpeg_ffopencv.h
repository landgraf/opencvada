








CVAPI(CvCapture*) cvCreateFileCapture_FFMPEG( const char* filename );
CVAPI(void) cvReleaseCapture_FFMPEG(CvCapture** capture);
CVAPI(int) cvSetCaptureProperty_FFMPEG(CvCapture* capture, int prop_id, double value);
CVAPI(double) cvGetCaptureProperty_FFMPEG(CvCapture* capture, int prop_id);
CVAPI(int) cvGrabFrame_FFMPEG(CvCapture* capture);
CVAPI(IplImage*) cvRetrieveFrame_FFMPEG(CvCapture* capture, int);
CVAPI(CvVideoWriter*) cvCreateVideoWriter_FFMPEG( const char * filename, int fourcc,
		double fps, CvSize frameSize, int is_color );
CVAPI(void) cvReleaseVideoWriter_FFMPEG( CvVideoWriter** writer );
CVAPI(int) cvWriteFrame_FFMPEG( CvVideoWriter* writer, const IplImage* image );