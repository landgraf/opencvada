#include <opencv2/opencv.hpp>
#include <opencv2/legacy/legacy.hpp>

#include <opencv2/core/internal.hpp>
#include <opencv2/video/tracking.hpp>
#include <opencv2/video/background_segm.hpp>

/* #include <opencv2/legacy/blobtrack.hpp> */
/* #include <opencv2/legacy/compat.hpp> */

/* #include <_matrix.h> */

// CvBlobTracker wrappers
CvBlobTrackerCC *new_CvBlobTrackerCC();
void delete_CvBlobTrackerCC(CvBlobTrackerCC *_this);
int CvBlobTrackerCC_GetBlobNum(CvBlobTrackerCC *_this);
CvBlob *CvBlobTrackerCC_GetBlob(CvBlobTrackerCC *_this, int BlobIndex);
void CvBlobTrackerCC_SetBlob(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob);
CvBlob *CvBlobTrackerCC_GetBlobByID(CvBlobTrackerCC *_this, int BlobID);
void CvBlobTrackerCC_DelBlob(CvBlobTrackerCC *_this, int BlobIndex);
void CvBlobTrackerCC_Release(CvBlobTrackerCC *_this);
CvBlob *CvBlobTrackerCC_AddBlob(CvBlobTrackerCC *_this, CvBlob *pBlob, IplImage *pImgFG);
void CvBlobTrackerCC_Process(CvBlobTrackerCC *_this, IplImage *pImg, IplImage *pImgFG);
void CvBlobTrackerCC_ProcessBlob(CvBlobTrackerCC *_this, CvBlob *pBlob);
double CvBlobTrackerCC_GetConfidence(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob, IplImage *pImgFG);
double CvBlobTrackerCC_GetConfidenceList(CvBlobTrackerCC *_this, CvBlobSeq *pBlobList, IplImage *pImg, IplImage *pImgFG);
void CvBlobTrackerCC_UpdateBlob(CvBlobTrackerCC *_this, int BlobIndex, IplImage *pImgFG);
void CvBlobTrackerCC_Update(CvBlobTrackerCC *_this, IplImage *pImg, IplImage *pImgFG);
int CvBlobTrackerCC_GetBlobIndexByID(CvBlobTrackerCC *_this, int BlobID);
CvBlob *CvBlobTrackerCC_GetBlobByID(CvBlobTrackerCC *_this, int BlobID);
void CvBlobTrackerCC_DelbBlobByID(CvBlobTrackerCC *_this, int BlobID);
void CvBlobTrackerCC_SetBlobByID(CvBlobtrackerCC *_this, int BlobID, CvBlob *pBlob);
void CvBlobTrackerCC_ParamUpdate(CvBlobtrackerCC *_this);
int CvBlobTrackerCC_GetBlobHypNum(CvBlobTrackerCC *_this, int BlobIndex);
CvBlob *CvBlobTrackerCC_GetBlobHyp(CvBlobTrackerCC *_this, int BlobIndex, int Hypothesis);
void CvBlobTrackerCC_SetBlobHyp(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob);

// CvBlobTrackPredictKalman wrappers
CvBlobTrackPredictKalman *new_CvBlobTrackPredictKalman();
void delete_CvBlobTrackPredictKalman(CvBlobTrackPredictKalman *_this);
CvBlob *CvBlobTrackPredictKalman_Predict(CvBlobTrackPredictKalman(CvBlobTrackPredictKalman *_this);
void CvBlobTrackPredictKalman_Update(CvBlobTrackPredictKalman *_this, CvBlob *pBlob);
void CvBlobTrackPredictKalman_Release(CvBlobTrackPredictKalman *_this);
