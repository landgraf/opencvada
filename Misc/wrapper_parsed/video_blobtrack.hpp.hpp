// CvVSModule
CvVSModule * new_CvVSModule ();
void delete_CvVSModule (CvVSModule * _this );
const char * GetParamName (CvVSModule * _this , int index );
const char * GetParamComment (CvVSModule * _this , const char * name );
double GetParam (CvVSModule * _this , const char * name );
const char * GetParamStr (CvVSModule * _this , const char * name );
void SetParam (CvVSModule * _this , const char * name , double val );
void SetParamStr (CvVSModule * _this , const char * name , const char * str );
void TransferParamsFromChild (CvVSModule * _this , CvVSModule * pM , const char * prefix );
void TransferParamsToChild (CvVSModule * _this , CvVSModule * pM , char * prefix );
void ParamUpdate (CvVSModule * _this );
const char * GetTypeName (CvVSModule * _this );
int IsModuleTypeName (CvVSModule * _this , const char * name );
char * GetModuleName (CvVSModule * _this );
int IsModuleName (CvVSModule * _this , const char * name );
void SetNickName (CvVSModule * _this , const char * pStr );
const char * GetNickName (CvVSModule * _this );
void SaveState (CvVSModule * _this , CvFileStorage * );
void LoadState (CvVSModule * _this , CvFileStorage * , CvFileNode * );
void Release (CvVSModule * _this );

// CvFGDetector
CvFGDetector * new_CvFGDetector ();
IplImage * GetMask (CvFGDetector * _this );
void Process (CvFGDetector * _this , IplImage * pImg );
void Release (CvFGDetector * _this );

// CvBlobSeq
CvBlobSeq * new_CvBlobSeq (int BlobSize );
void delete_CvBlobSeq (CvBlobSeq * _this );
CvBlob * GetBlob (CvBlobSeq * _this , int BlobIndex );
CvBlob * GetBlobByID (CvBlobSeq * _this , int BlobID );
void DelBlob (CvBlobSeq * _this , int BlobIndex );
void DelBlobByID (CvBlobSeq * _this , int BlobID );
void Clear (CvBlobSeq * _this );
void AddBlob (CvBlobSeq * _this , CvBlob * pB );
int GetBlobNum (CvBlobSeq * _this );
void Write (CvBlobSeq * _this , CvFileStorage * fs , const char * name );
void Load (CvBlobSeq * _this , CvFileStorage * fs , CvFileNode * node );
void AddFormat (CvBlobSeq * _this , const char * str );

// CvBlobTrackSeq
CvBlobTrackSeq * new_CvBlobTrackSeq (int TrackSize );
void delete_CvBlobTrackSeq (CvBlobTrackSeq * _this );
CvBlobTrack * GetBlobTrack (CvBlobTrackSeq * _this , int TrackIndex );
CvBlobTrack * GetBlobTrackByID (CvBlobTrackSeq * _this , int TrackID );
void DelBlobTrack (CvBlobTrackSeq * _this , int TrackIndex );
void DelBlobTrackByID (CvBlobTrackSeq * _this , int TrackID );
void Clear (CvBlobTrackSeq * _this );
void AddBlobTrack (CvBlobTrackSeq * _this , int TrackID , int StartFrame );
int GetBlobTrackNum (CvBlobTrackSeq * _this );

// CvBlobDetector
CvBlobDetector * new_CvBlobDetector ();
int DetectNewBlob (CvBlobDetector * _this , IplImage * pImg , IplImage * pImgFG , CvBlobSeq * pNewBlobList , CvBlobSeq * pOldBlobList );
void Release (CvBlobDetector * _this );

// CvObjectDetector
CvObjectDetector * new_CvObjectDetector (const char * );
void delete_CvObjectDetector (CvObjectDetector * _this );
bool Load (CvObjectDetector * _this , const char * );
CvSize GetMinWindowSize (CvObjectDetector * _this );
int GetMaxBorderSize (CvObjectDetector * _this );
void Detect (CvObjectDetector * _this , const CvArr * , CvBlobSeq * );

// CvImageDrawer
CvImageDrawer * new_CvImageDrawer ();
void delete_CvImageDrawer (CvImageDrawer * _this );
void SetShapes (CvImageDrawer * _this , const CvDrawShape * shapes , int num );
IplImage * Draw (CvImageDrawer * _this , const CvArr * src , CvBlobSeq * blob_seq , const CvSeq * roi_seq );
IplImage * GetImage (CvImageDrawer * _this );

// CvBlobTrackGen
CvBlobTrackGen * new_CvBlobTrackGen ();
void SetFileName (CvBlobTrackGen * _this , char * pFileName );
void AddBlob (CvBlobTrackGen * _this , CvBlob * pBlob );
void Process (CvBlobTrackGen * _this , IplImage * pImg , IplImage * pFG );
void Release (CvBlobTrackGen * _this );

// CvBlobTracker
CvBlobTracker * new_CvBlobTracker ();
CvBlob * AddBlob (CvBlobTracker * _this , CvBlob * pBlob , IplImage * pImg , IplImage * pImgFG );
int GetBlobNum (CvBlobTracker * _this );
CvBlob * GetBlob (CvBlobTracker * _this , int BlobIndex );
void DelBlob (CvBlobTracker * _this , int BlobIndex );
void Process (CvBlobTracker * _this , IplImage * pImg , IplImage * pImgFG );
void Release (CvBlobTracker * _this );
void ProcessBlob (CvBlobTracker * _this , int BlobIndex , CvBlob * pBlob , IplImage * , IplImage * );
double GetConfidence (CvBlobTracker * _this , int , CvBlob * , IplImage * , IplImage * );
double GetConfidenceList (CvBlobTracker * _this , CvBlobSeq * pBlobList , IplImage * pImg , IplImage * pImgFG );
void UpdateBlob (CvBlobTracker * _this , int , CvBlob * , IplImage * , IplImage * );
void Update (CvBlobTracker * _this , IplImage * pImg , IplImage * pImgFG );
int GetBlobIndexByID (CvBlobTracker * _this , int BlobID );
CvBlob * GetBlobByID (CvBlobTracker * _this , int BlobID );
void DelBlobByID (CvBlobTracker * _this , int BlobID );
void SetBlob (CvBlobTracker * _this , int , CvBlob * );
void SetBlobByID (CvBlobTracker * _this , int BlobID , CvBlob * pBlob );
int GetBlobHypNum (CvBlobTracker * _this , int );
CvBlob * GetBlobHyp (CvBlobTracker * _this , int BlobIndex , int );
void SetBlobHyp (CvBlobTracker * _this , int , CvBlob * );

// CvBlobTrackerOne
void Init (CvBlobTrackerOne * _this , CvBlob * pBlobInit , IplImage * pImg , IplImage * pImgFG );
CvBlob * Process (CvBlobTrackerOne * _this , CvBlob * pBlobPrev , IplImage * pImg , IplImage * pImgFG );
void Release (CvBlobTrackerOne * _this );
void SkipProcess (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * );
void Update (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * );
void SetCollision (CvBlobTrackerOne * _this , int );
double GetConfidence (CvBlobTrackerOne * _this , CvBlob * , IplImage * , IplImage * , IplImage * );

// CvBlobTrackPostProc
CvBlobTrackPostProc * new_CvBlobTrackPostProc ();
void AddBlob (CvBlobTrackPostProc * _this , CvBlob * pBlob );
void Process (CvBlobTrackPostProc * _this );
int GetBlobNum (CvBlobTrackPostProc * _this );
CvBlob * GetBlob (CvBlobTrackPostProc * _this , int index );
void Release (CvBlobTrackPostProc * _this );
CvBlob * GetBlobByID (CvBlobTrackPostProc * _this , int BlobID );

// CvBlobTrackPostProcOne
CvBlobTrackPostProcOne * new_CvBlobTrackPostProcOne ();
CvBlob * Process (CvBlobTrackPostProcOne * _this , CvBlob * pBlob );
void Release (CvBlobTrackPostProcOne * _this );

// CvBlobTrackPredictor
CvBlobTrackPredictor * new_CvBlobTrackPredictor ();
CvBlob * Predict (CvBlobTrackPredictor * _this );
void Update (CvBlobTrackPredictor * _this , CvBlob * pBlob );
void Release (CvBlobTrackPredictor * _this );

// CvBlobTrackAnalysis
CvBlobTrackAnalysis * new_CvBlobTrackAnalysis ();
void AddBlob (CvBlobTrackAnalysis * _this , CvBlob * pBlob );
void Process (CvBlobTrackAnalysis * _this , IplImage * pImg , IplImage * pFG );
float GetState (CvBlobTrackAnalysis * _this , int BlobID );
const char * GetStateDesc (CvBlobTrackAnalysis * _this , int );
void SetFileName (CvBlobTrackAnalysis * _this , char * );
void Release (CvBlobTrackAnalysis * _this );

// CvBlobTrackFVGen
CvBlobTrackFVGen * new_CvBlobTrackFVGen ();
void AddBlob (CvBlobTrackFVGen * _this , CvBlob * pBlob );
void Process (CvBlobTrackFVGen * _this , IplImage * pImg , IplImage * pFG );
void Release (CvBlobTrackFVGen * _this );
int GetFVSize (CvBlobTrackFVGen * _this );
int GetFVNum (CvBlobTrackFVGen * _this );
float * GetFV (CvBlobTrackFVGen * _this , int index , int * pFVID );
float * GetFVVar (CvBlobTrackFVGen * _this );
float * GetFVMin (CvBlobTrackFVGen * _this );
float * GetFVMax (CvBlobTrackFVGen * _this );

// CvBlobTrackAnalysisOne
void delete_CvBlobTrackAnalysisOne (CvBlobTrackAnalysisOne * _this );
int Process (CvBlobTrackAnalysisOne * _this , CvBlob * pBlob , IplImage * pImg , IplImage * pFG );
void Release (CvBlobTrackAnalysisOne * _this );

// CvBlobTrackAnalysisHeight
double GetHeight (CvBlobTrackAnalysisHeight * _this , CvBlob * pB );

// CvBlobTrackerAuto
CvBlobTrackerAuto * new_CvBlobTrackerAuto ();
void Process (CvBlobTrackerAuto * _this , IplImage * pImg , IplImage * pMask );
CvBlob * GetBlob (CvBlobTrackerAuto * _this , int index );
CvBlob * GetBlobByID (CvBlobTrackerAuto * _this , int ID );
int GetBlobNum (CvBlobTrackerAuto * _this );
IplImage * GetFGMask (CvBlobTrackerAuto * _this );
float GetState (CvBlobTrackerAuto * _this , int BlobID );
const char * GetStateDesc (CvBlobTrackerAuto * _this , int BlobID );
void Release (CvBlobTrackerAuto * _this );

// CvProb
void delete_CvProb (CvProb * _this );
double Value (CvProb * _this , int * , int , int );
void AddFeature (CvProb * _this , float W , int * comps , int x , int y );
void Scale (CvProb * _this , float factor , int x , int y );
void Release (CvProb * _this );
