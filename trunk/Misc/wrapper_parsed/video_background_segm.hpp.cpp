void delete_BackgroundSubtractor (BackgroundSubtractor * _this ){
	delete _this;
}
BackgroundSubtractorMOG * new_BackgroundSubtractorMOG (){
	return new BackgroundSubtractorMOG();
}
BackgroundSubtractorMOG * new_BackgroundSubtractorMOG (int history , int nmixtures , double backgroundRatio , double noiseSigma ){
	return new BackgroundSubtractorMOG(/*int*/history , /*int*/nmixtures , /*double*/backgroundRatio , /*double*/noiseSigma);
}
void delete_BackgroundSubtractorMOG (BackgroundSubtractorMOG * _this ){
	delete _this;
}
void initialize (BackgroundSubtractorMOG * _this , Size frameSize , int frameType ){
	_this->initialize(/*BackgroundSubtractorMOG*//***//*removed _this*//*Size*/frameSize , /*int*/frameType);
}
