










 

 

 

 

 


CVAPI(CvHaarClassifierCascade*) cvLoadHaarClassifierCascade(
                    const char* directory, CvSize orig_window_size);

CVAPI(void) cvReleaseHaarClassifierCascade( CvHaarClassifierCascade** cascade );


CVAPI(CvSeq*) cvHaarDetectObjects( const CvArr* image,
                     CvHaarClassifierCascade* cascade,
                     CvMemStorage* storage, double scale_factor CV_DEFAULT(1.1),
                     int min_neighbors CV_DEFAULT(3), int flags CV_DEFAULT(0),
                     CvSize min_size CV_DEFAULT(cvSize(0,0)), CvSize max_size CV_DEFAULT(cvSize(0,0)));


CVAPI(void) cvSetImagesForHaarClassifierCascade( CvHaarClassifierCascade* cascade,
                                                const CvArr* sum, const CvArr* sqsum,
                                                const CvArr* tilted_sum, double scale );


CVAPI(int) cvRunHaarClassifierCascade( const CvHaarClassifierCascade* cascade,
                                       CvPoint pt, int start_stage CV_DEFAULT(0));








 


















 









 





 





CVAPI(CvLatentSvmDetector*) cvLoadLatentSvmDetector(const char* filename);


CVAPI(void) cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);


CVAPI(CvSeq*) cvLatentSvmDetectObjects(IplImage* image, 
								CvLatentSvmDetector* detector, 
								CvMemStorage* storage, 
								float overlap_threshold CV_DEFAULT(0.5f));