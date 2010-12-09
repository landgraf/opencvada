void delete_BackgroundSubtractor (BackgroundSubtractor * _this );
BackgroundSubtractorMOG * new_BackgroundSubtractorMOG ();
BackgroundSubtractorMOG * new_BackgroundSubtractorMOG (int history , int nmixtures , double backgroundRatio , double noiseSigma );
void delete_BackgroundSubtractorMOG (BackgroundSubtractorMOG * _this );
void initialize (BackgroundSubtractorMOG * _this , Size frameSize , int frameType );
