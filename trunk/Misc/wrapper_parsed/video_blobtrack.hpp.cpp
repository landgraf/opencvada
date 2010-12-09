CvVSModule * new_CvVSModule (){
	return new CvVSModule();
}
void delete_CvVSModule (CvVSModule * _this ){
	delete _this;
}
const char * GetParamName (CvVSModule * _this , int index ){
	return _this->GetParamName(/*CvVSModule*//***//*removed _this*//*int*/index);
}
const char * GetParamComment (CvVSModule * _this , const char * name ){
	return _this->GetParamComment(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name);
}
double GetParam (CvVSModule * _this , const char * name ){
	return _this->GetParam(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name);
}
const char * GetParamStr (CvVSModule * _this , const char * name ){
	return _this->GetParamStr(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name);
}
void SetParam (CvVSModule * _this , const char * name , double val ){
	_this->SetParam(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name , /*double*/val);
}
void SetParamStr (CvVSModule * _this , const char * name , const char * str ){
	_this->SetParamStr(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name , /*const*//*char*//***/str);
}
void TransferParamsFromChild (CvVSModule * _this , CvVSModule * pM , const char * prefix ){
	_this->TransferParamsFromChild(/*CvVSModule*//***//*removed _this*//*CvVSModule*//***/pM , /*const*//*char*//***/prefix);
}
void TransferParamsToChild (CvVSModule * _this , CvVSModule * pM , char * prefix ){
	_this->TransferParamsToChild(/*CvVSModule*//***//*removed _this*//*CvVSModule*//***/pM , /*char*//***/prefix);
}
void ParamUpdate (CvVSModule * _this ){
	_this->ParamUpdate(/*CvVSModule*//***//*removed _this*/);
}
const char * GetTypeName (CvVSModule * _this ){
	return _this->GetTypeName(/*CvVSModule*//***//*removed _this*/);
}
int IsModuleTypeName (CvVSModule * _this , const char * name ){
	return _this->IsModuleTypeName(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name);
}
char * GetModuleName (CvVSModule * _this ){
	return _this->GetModuleName(/*CvVSModule*//***//*removed _this*/);
}
int IsModuleName (CvVSModule * _this , const char * name ){
	return _this->IsModuleName(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/name);
}
void SetNickName (CvVSModule * _this , const char * pStr ){
	_this->SetNickName(/*CvVSModule*//***//*removed _this*//*const*//*char*//***/pStr);
}
const char * GetNickName (CvVSModule * _this ){
	return _this->GetNickName(/*CvVSModule*//***//*removed _this*/);
}
void SaveState (CvVSModule * _this , CvFileStorage * ){
	_this->SaveState(/*CvVSModule*//***//*removed _this*//*CvFileStorage*/*);
}
void LoadState (CvVSModule * _this , CvFileStorage * , CvFileNode * ){
	_this->LoadState(/*CvVSModule*//***//*removed _this*//*CvFileStorage*//***/, /*CvFileNode*/*);
}
void Release (CvVSModule * _this ){
	_this->Release(/*CvVSModule*//***//*removed _this*/);
}
CvFGDetector * new_CvFGDetector (){
	return new CvFGDetector();
}
IplImage * GetMask (CvFGDetector * _this ){
	return _this->GetMask(/*CvFGDetector*//***//*removed _this*/);
}
void Process (CvFGDetector * _this , IplImage * pImg ){
	_this->Process(/*CvFGDetector*//***//*removed _this*//*IplImage*//***/pImg);
}
void Release (CvFGDetector * _this ){
	_this->Release(/*CvFGDetector*//***//*removed _this*/);
}
CvBlobSeq * new_CvBlobSeq (int BlobSize ){
	return new CvBlobSeq(/*int*/BlobSize);
}
void delete_CvBlobSeq (CvBlobSeq * _this ){
	delete _this;
}
CvBlob * GetBlob (CvBlobSeq * _this , int BlobIndex ){
	return _this->GetBlob(/*CvBlobSeq*//***//*removed _this*//*int*/BlobIndex);
}
CvBlob * GetBlobByID (CvBlobSeq * _this , int BlobID ){
	return _this->GetBlobByID(/*CvBlobSeq*//***//*removed _this*//*int*/BlobID);
}
void DelBlob (CvBlobSeq * _this , int BlobIndex ){
	_this->DelBlob(/*CvBlobSeq*//***//*removed _this*//*int*/BlobIndex);
}
void DelBlobByID (CvBlobSeq * _this , int BlobID ){
	_this->DelBlobByID(/*CvBlobSeq*//***//*removed _this*//*int*/BlobID);
}
void Clear (CvBlobSeq * _this ){
	_this->Clear(/*CvBlobSeq*//***//*removed _this*/);
}
void AddBlob (CvBlobSeq * _this , CvBlob * pB ){
	_this->AddBlob(/*CvBlobSeq*//***//*removed _this*//*CvBlob*//***/pB);
}
int GetBlobNum (CvBlobSeq * _this ){
	return _this->GetBlobNum(/*CvBlobSeq*//***//*removed _this*/);
}
void Write (CvBlobSeq * _this , CvFileStorage * fs , const char * name ){
	_this->Write(/*CvBlobSeq*//***//*removed _this*//*CvFileStorage*//***/fs , /*const*//*char*//***/name);
}
void Load (CvBlobSeq * _this , CvFileStorage * fs , CvFileNode * node ){
	_this->Load(/*CvBlobSeq*//***//*removed _this*//*CvFileStorage*//***/fs , /*CvFileNode*//***/node);
}
void AddFormat (CvBlobSeq * _this , const char * str ){
	_this->AddFormat(/*CvBlobSeq*//***//*removed _this*//*const*//*char*//***/str);
}
CvBlobTrackSeq * new_CvBlobTrackSeq (int TrackSize ){
	return new CvBlobTrackSeq(/*int*/TrackSize);
}
void delete_CvBlobTrackSeq (CvBlobTrackSeq * _this ){
	delete _this;
}
CvBlobTrack * GetBlobTrack (CvBlobTrackSeq * _this , int TrackIndex ){
	return _this->GetBlobTrack(/*CvBlobTrackSeq*//***//*removed _this*//*int*/TrackIndex);
}
CvBlobTrack * GetBlobTrackByID (CvBlobTrackSeq * _this , int TrackID ){
	return _this->GetBlobTrackByID(/*CvBlobTrackSeq*//***//*removed _this*//*int*/TrackID);
}
void DelBlobTrack (CvBlobTrackSeq * _this , int TrackIndex ){
	_this->DelBlobTrack(/*CvBlobTrackSeq*//***//*removed _this*//*int*/TrackIndex);
}
void DelBlobTrackByID (CvBlobTrackSeq * _this , int TrackID ){
	_this->DelBlobTrackByID(/*CvBlobTrackSeq*//***//*removed _this*//*int*/TrackID);
}
void Clear (CvBlobTrackSeq * _this ){
	_this->Clear(/*CvBlobTrackSeq*//***//*removed _this*/);
}
void AddBlobTrack (CvBlobTrackSeq * _this , int TrackID , int StartFrame ){
	_this->AddBlobTrack(/*CvBlobTrackSeq*//***//*removed _this*//*int*/TrackID , /*int*/StartFrame);
}
int GetBlobTrackNum (CvBlobTrackSeq * _this ){
	return _this->GetBlobTrackNum(/*CvBlobTrackSeq*//***//*removed _this*/);
}
CvBlobDetector * new_CvBlobDetector (){
	return new CvBlobDetector();
}
int DetectNewBlob (CvBlobDetector * _this , IplImage * pImg , IplImage * pImgFG , CvBlobSeq * pNewBlobList , CvBlobSeq * pOldBlobList ){
	return _this->DetectNewBlob(/*CvBlobDetector*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pImgFG , /*CvBlobSeq*//***/pNewBlobList , /*CvBlobSeq*//***/pOldBlobList);
}
void Release (CvBlobDetector * _this ){
	_this->Release(/*CvBlobDetector*//***//*removed _this*/);
}
CvObjectDetector * new_CvObjectDetector (const char * ){
	return new CvObjectDetector(/*const*//*char*/*);
}
void delete_CvObjectDetector (CvObjectDetector * _this ){
	delete _this;
}
bool Load (CvObjectDetector * _this , const char * ){
	return _this->Load(/*CvObjectDetector*//***//*removed _this*//*const*//*char*/*);
}
CvSize GetMinWindowSize (CvObjectDetector * _this ){
	return _this->GetMinWindowSize(/*CvObjectDetector*//***//*removed _this*/);
}
int GetMaxBorderSize (CvObjectDetector * _this ){
	return _this->GetMaxBorderSize(/*CvObjectDetector*//***//*removed _this*/);
}
void Detect (CvObjectDetector * _this , const CvArr * , CvBlobSeq * ){
	_this->Detect(/*CvObjectDetector*//***//*removed _this*//*const*//*CvArr*//***/, /*CvBlobSeq*/*);
}
CvImageDrawer * new_CvImageDrawer (){
	return new CvImageDrawer();
}
void delete_CvImageDrawer (CvImageDrawer * _this ){
	delete _this;
}
void SetShapes (CvImageDrawer * _this , const CvDrawShape * shapes , int num ){
	_this->SetShapes(/*CvImageDrawer*//***//*removed _this*//*const*//*CvDrawShape*//***/shapes , /*int*/num);
}
IplImage * Draw (CvImageDrawer * _this , const CvArr * src , CvBlobSeq * blob_seq , const CvSeq * roi_seq ){
	return _this->Draw(/*CvImageDrawer*//***//*removed _this*//*const*//*CvArr*//***/src , /*CvBlobSeq*//***/blob_seq , /*const*//*CvSeq*//***/roi_seq);
}
IplImage * GetImage (CvImageDrawer * _this ){
	return _this->GetImage(/*CvImageDrawer*//***//*removed _this*/);
}
CvBlobTrackGen * new_CvBlobTrackGen (){
	return new CvBlobTrackGen();
}
void SetFileName (CvBlobTrackGen * _this , char * pFileName ){
	_this->SetFileName(/*CvBlobTrackGen*//***//*removed _this*//*char*//***/pFileName);
}
void AddBlob (CvBlobTrackGen * _this , CvBlob * pBlob ){
	_this->AddBlob(/*CvBlobTrackGen*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Process (CvBlobTrackGen * _this , IplImage * pImg , IplImage * pFG ){
	_this->Process(/*CvBlobTrackGen*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pFG);
}
void Release (CvBlobTrackGen * _this ){
	_this->Release(/*CvBlobTrackGen*//***//*removed _this*/);
}
CvBlobTracker * new_CvBlobTracker (){
	return new CvBlobTracker();
}
CvBlob * AddBlob (CvBlobTracker * _this , CvBlob * pBlob , IplImage * pImg , IplImage * pImgFG ){
	return _this->AddBlob(/*CvBlobTracker*//***//*removed _this*//*CvBlob*//***/pBlob , /*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
int GetBlobNum (CvBlobTracker * _this ){
	return _this->GetBlobNum(/*CvBlobTracker*//***//*removed _this*/);
}
CvBlob * GetBlob (CvBlobTracker * _this , int BlobIndex ){
	return _this->GetBlob(/*CvBlobTracker*//***//*removed _this*//*int*/BlobIndex);
}
void DelBlob (CvBlobTracker * _this , int BlobIndex ){
	_this->DelBlob(/*CvBlobTracker*//***//*removed _this*//*int*/BlobIndex);
}
void Process (CvBlobTracker * _this , IplImage * pImg , IplImage * pImgFG ){
	_this->Process(/*CvBlobTracker*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
void Release (CvBlobTracker * _this ){
	_this->Release(/*CvBlobTracker*//***//*removed _this*/);
}
void ProcessBlob (CvBlobTracker * _this , int BlobIndex , CvBlob * pBlob , IplImage * , IplImage * ){
	_this->ProcessBlob(/*CvBlobTracker*//***//*removed _this*//*int*/BlobIndex , /*CvBlob*//***/pBlob , /*IplImage*//***/, /*IplImage*/*);
}
double GetConfidence (CvBlobTracker * _this , int , CvBlob * , IplImage * , IplImage * ){
	return _this->GetConfidence(/*CvBlobTracker*//***//*removed _this*//*int*/, /*CvBlob*//***/, /*IplImage*//***/, /*IplImage*/*);
}
double GetConfidenceList (CvBlobTracker * _this , CvBlobSeq * pBlobList , IplImage * pImg , IplImage * pImgFG ){
	return _this->GetConfidenceList(/*CvBlobTracker*//***//*removed _this*//*CvBlobSeq*//***/pBlobList , /*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
void UpdateBlob (CvBlobTracker * _this , int , CvBlob * , IplImage * , IplImage * ){
	_this->UpdateBlob(/*CvBlobTracker*//***//*removed _this*//*int*/, /*CvBlob*//***/, /*IplImage*//***/, /*IplImage*/*);
}
void Update (CvBlobTracker * _this , IplImage * pImg , IplImage * pImgFG ){
	_this->Update(/*CvBlobTracker*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
int GetBlobIndexByID (CvBlobTracker * _this , int BlobID ){
	return _this->GetBlobIndexByID(/*CvBlobTracker*//***//*removed _this*//*int*/BlobID);
}
CvBlob * GetBlobByID (CvBlobTracker * _this , int BlobID ){
	return _this->GetBlobByID(/*CvBlobTracker*//***//*removed _this*//*int*/BlobID);
}
void DelBlobByID (CvBlobTracker * _this , int BlobID ){
	_this->DelBlobByID(/*CvBlobTracker*//***//*removed _this*//*int*/BlobID);
}
void SetBlob (CvBlobTracker * _this , int , CvBlob * ){
	_this->SetBlob(/*CvBlobTracker*//***//*removed _this*//*int*/, /*CvBlob*/*);
}
void SetBlobByID (CvBlobTracker * _this , int BlobID , CvBlob * pBlob ){
	_this->SetBlobByID(/*CvBlobTracker*//***//*removed _this*//*int*/BlobID , /*CvBlob*//***/pBlob);
}
int GetBlobHypNum (CvBlobTracker * _this , int ){
	return _this->GetBlobHypNum(/*CvBlobTracker*//***//*removed _this*/int);
}
CvBlob * GetBlobHyp (CvBlobTracker * _this , int BlobIndex , int ){
	return _this->GetBlobHyp(/*CvBlobTracker*//***//*removed _this*//*int*/BlobIndex , int);
}
void SetBlobHyp (CvBlobTracker * _this , int , CvBlob * ){
	_this->SetBlobHyp(/*CvBlobTracker*//***//*removed _this*//*int*/, /*CvBlob*/*);
}
void Init (CvBlobTrackerOne * _this , CvBlob * pBlobInit , IplImage * pImg , IplImage * pImgFG ){
	_this->Init(/*CvBlobTrackerOne*//***//*removed _this*//*CvBlob*//***/pBlobInit , /*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
CvBlob * Process (CvBlobTrackerOne * _this , CvBlob * pBlobPrev , IplImage * pImg , IplImage * pImgFG ){
	return _this->Process(/*CvBlobTrackerOne*//***//*removed _this*//*CvBlob*//***/pBlobPrev , /*IplImage*//***/pImg , /*IplImage*//***/pImgFG);
}
void Release (CvBlobTrackerOne * _this ){
	_this->Release(/*CvBlobTrackerOne*//***//*removed _this*/);
}
void SkipProcess (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * ){
	_this->SkipProcess(/*CvBlobTrackerOne*//***//*removed _this*//*CvBlob*//***/, /*IplImage*//***/, /*IplImage*/*);
}
void Update (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * ){
	_this->Update(/*CvBlobTrackerOne*//***//*removed _this*//*CvBlob*//***/, /*IplImage*//***/, /*IplImage*/*);
}
void SetCollision (CvBlobTrackerOne * _this , int ){
	_this->SetCollision(/*CvBlobTrackerOne*//***//*removed _this*/int);
}
double GetConfidence (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * , IplImage * ){
	return _this->GetConfidence(/*CvBlobTrackerOne*//***//*removed _this*//*CvBlob*//***/, /*IplImage*//***/, /*IplImage*//***/, /*IplImage*/*);
}
CvBlobTrackPostProc * new_CvBlobTrackPostProc (){
	return new CvBlobTrackPostProc();
}
void AddBlob (CvBlobTrackPostProc * _this , CvBlob * pBlob ){
	_this->AddBlob(/*CvBlobTrackPostProc*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Process (CvBlobTrackPostProc * _this ){
	_this->Process(/*CvBlobTrackPostProc*//***//*removed _this*/);
}
int GetBlobNum (CvBlobTrackPostProc * _this ){
	return _this->GetBlobNum(/*CvBlobTrackPostProc*//***//*removed _this*/);
}
CvBlob * GetBlob (CvBlobTrackPostProc * _this , int index ){
	return _this->GetBlob(/*CvBlobTrackPostProc*//***//*removed _this*//*int*/index);
}
void Release (CvBlobTrackPostProc * _this ){
	_this->Release(/*CvBlobTrackPostProc*//***//*removed _this*/);
}
CvBlob * GetBlobByID (CvBlobTrackPostProc * _this , int BlobID ){
	return _this->GetBlobByID(/*CvBlobTrackPostProc*//***//*removed _this*//*int*/BlobID);
}
CvBlobTrackPostProcOne * new_CvBlobTrackPostProcOne (){
	return new CvBlobTrackPostProcOne();
}
CvBlob * Process (CvBlobTrackPostProcOne * _this , CvBlob * pBlob ){
	return _this->Process(/*CvBlobTrackPostProcOne*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Release (CvBlobTrackPostProcOne * _this ){
	_this->Release(/*CvBlobTrackPostProcOne*//***//*removed _this*/);
}
CvBlobTrackPredictor * new_CvBlobTrackPredictor (){
	return new CvBlobTrackPredictor();
}
CvBlob * Predict (CvBlobTrackPredictor * _this ){
	return _this->Predict(/*CvBlobTrackPredictor*//***//*removed _this*/);
}
void Update (CvBlobTrackPredictor * _this , CvBlob * pBlob ){
	_this->Update(/*CvBlobTrackPredictor*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Release (CvBlobTrackPredictor * _this ){
	_this->Release(/*CvBlobTrackPredictor*//***//*removed _this*/);
}
CvBlobTrackAnalysis * new_CvBlobTrackAnalysis (){
	return new CvBlobTrackAnalysis();
}
void AddBlob (CvBlobTrackAnalysis * _this , CvBlob * pBlob ){
	_this->AddBlob(/*CvBlobTrackAnalysis*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Process (CvBlobTrackAnalysis * _this , IplImage * pImg , IplImage * pFG ){
	_this->Process(/*CvBlobTrackAnalysis*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pFG);
}
float GetState (CvBlobTrackAnalysis * _this , int BlobID ){
	return _this->GetState(/*CvBlobTrackAnalysis*//***//*removed _this*//*int*/BlobID);
}
const char * GetStateDesc (CvBlobTrackAnalysis * _this , int ){
	return _this->GetStateDesc(/*CvBlobTrackAnalysis*//***//*removed _this*/int);
}
void SetFileName (CvBlobTrackAnalysis * _this , char * ){
	_this->SetFileName(/*CvBlobTrackAnalysis*//***//*removed _this*//*char*/*);
}
void Release (CvBlobTrackAnalysis * _this ){
	_this->Release(/*CvBlobTrackAnalysis*//***//*removed _this*/);
}
CvBlobTrackFVGen * new_CvBlobTrackFVGen (){
	return new CvBlobTrackFVGen();
}
void AddBlob (CvBlobTrackFVGen * _this , CvBlob * pBlob ){
	_this->AddBlob(/*CvBlobTrackFVGen*//***//*removed _this*//*CvBlob*//***/pBlob);
}
void Process (CvBlobTrackFVGen * _this , IplImage * pImg , IplImage * pFG ){
	_this->Process(/*CvBlobTrackFVGen*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pFG);
}
void Release (CvBlobTrackFVGen * _this ){
	_this->Release(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
int GetFVSize (CvBlobTrackFVGen * _this ){
	return _this->GetFVSize(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
int GetFVNum (CvBlobTrackFVGen * _this ){
	return _this->GetFVNum(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
float * GetFV (CvBlobTrackFVGen * _this , int index , int * pFVID ){
	return _this->GetFV(/*CvBlobTrackFVGen*//***//*removed _this*//*int*/index , /*int*//***/pFVID);
}
float * GetFVVar (CvBlobTrackFVGen * _this ){
	return _this->GetFVVar(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
float * GetFVMin (CvBlobTrackFVGen * _this ){
	return _this->GetFVMin(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
float * GetFVMax (CvBlobTrackFVGen * _this ){
	return _this->GetFVMax(/*CvBlobTrackFVGen*//***//*removed _this*/);
}
void delete_CvBlobTrackAnalysisOne (CvBlobTrackAnalysisOne * _this ){
	delete _this;
}
int Process (CvBlobTrackAnalysisOne * _this , CvBlob * pBlob , IplImage * pImg , IplImage * pFG ){
	return _this->Process(/*CvBlobTrackAnalysisOne*//***//*removed _this*//*CvBlob*//***/pBlob , /*IplImage*//***/pImg , /*IplImage*//***/pFG);
}
void Release (CvBlobTrackAnalysisOne * _this ){
	_this->Release(/*CvBlobTrackAnalysisOne*//***//*removed _this*/);
}
double GetHeight (CvBlobTrackAnalysisHeight * _this , CvBlob * pB ){
	return _this->GetHeight(/*CvBlobTrackAnalysisHeight*//***//*removed _this*//*CvBlob*//***/pB);
}
CvBlobTrackerAuto * new_CvBlobTrackerAuto (){
	return new CvBlobTrackerAuto();
}
void Process (CvBlobTrackerAuto * _this , IplImage * pImg , IplImage * pMask ){
	_this->Process(/*CvBlobTrackerAuto*//***//*removed _this*//*IplImage*//***/pImg , /*IplImage*//***/pMask);
}
CvBlob * GetBlob (CvBlobTrackerAuto * _this , int index ){
	return _this->GetBlob(/*CvBlobTrackerAuto*//***//*removed _this*//*int*/index);
}
CvBlob * GetBlobByID (CvBlobTrackerAuto * _this , int ID ){
	return _this->GetBlobByID(/*CvBlobTrackerAuto*//***//*removed _this*//*int*/ID);
}
int GetBlobNum (CvBlobTrackerAuto * _this ){
	return _this->GetBlobNum(/*CvBlobTrackerAuto*//***//*removed _this*/);
}
IplImage * GetFGMask (CvBlobTrackerAuto * _this ){
	return _this->GetFGMask(/*CvBlobTrackerAuto*//***//*removed _this*/);
}
float GetState (CvBlobTrackerAuto * _this , int BlobID ){
	return _this->GetState(/*CvBlobTrackerAuto*//***//*removed _this*//*int*/BlobID);
}
const char * GetStateDesc (CvBlobTrackerAuto * _this , int BlobID ){
	return _this->GetStateDesc(/*CvBlobTrackerAuto*//***//*removed _this*//*int*/BlobID);
}
void Release (CvBlobTrackerAuto * _this ){
	_this->Release(/*CvBlobTrackerAuto*//***//*removed _this*/);
}
void delete_CvProb (CvProb * _this ){
	delete _this;
}
double Value (CvProb * _this , int * , int , int ){
	return _this->Value(/*CvProb*//***//*removed _this*//*int*//***/, /*int*/, int);
}
void AddFeature (CvProb * _this , float W , int * comps , int x , int y ){
	_this->AddFeature(/*CvProb*//***//*removed _this*//*float*/W , /*int*//***/comps , /*int*/x , /*int*/y);
}
void Scale (CvProb * _this , float factor , int x , int y ){
	_this->Scale(/*CvProb*//***//*removed _this*//*float*/factor , /*int*/x , /*int*/y);
}
void Release (CvProb * _this ){
	_this->Release(/*CvProb*//***//*removed _this*/);
}
