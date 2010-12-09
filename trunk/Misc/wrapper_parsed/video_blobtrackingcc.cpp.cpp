#include "video_blobtrackingcc.cpp.hpp"

// CvBlobTracker wrappers
CvBlobTrackerCC *new_CvBlobTrackerCC() {
	return new CvBlobTrackerCC();
}

void delete_CvBlobTrackerCC(CvBlobTrackerCC *_this) {
	delete _this;
}

int CvBlobTrackerCC_GetBlobNum(CvBlobTrackerCC *_this) {
	return _this->GetBlobNum();
}

CvBlob *CvBlobTrackerCC_GetBlob(CvBlobTrackerCC *_this, int BlobIndex) {
	return _this->GetBlob(BlobIndex);
}

void CvBlobTrackerCC_SetBlob(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob) {
	_this->SetBlob(BlobIndex, pBlob);
}

CvBlob *CvBlobTrackerCC_GetBlobByID(CvBlobTrackerCC *_this, int BlobID) {
	return _this->GetBlobByID(BlobID);
}

void CvBlobTrackerCC_DelBlob(CvBlobTrackerCC *_this, int BlobIndex) {
	_this->DelBlob(BlobIndex);
}

void CvBlobTrackerCC_Release(CvBlobTrackerCC *_this) {
	_this->Release();
}

CvBlob *CvBlobTrackerCC_AddBlob(CvBlobTrackerCC *_this, CvBlob *pBlob, IplImage *pImgFG) {
	return _this->AddBlob(pBlob, pImgFG);
}

void CvBlobTrackerCC_Process(CvBlobTrackerCC *_this, IplImage *pImg, IplImage *pImgFG) {
	_this->Process(pImg, pImgFG);
}

void CvBlobTrackerCC_ProcessBlob(CvBlobTrackerCC *_this, CvBlob *pBlob) {
	_this->ProcessBlob(pBlob);
}

double CvBlobTrackerCC_GetConfidence(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob, IplImage *pImgFG) {
	return _this->GetConfidence(BlobIndex, pBlob, pImgFG);
}

double CvBlobTrackerCC_GetConfidenceList(CvBlobTrackerCC *_this, CvBlobSeq *pBlobList, IplImage *pImg, IplImage *pImgFG) {
	return _this->GetConfidenceList(pBlobList, pImg, pImgFG);
}

void CvBlobTrackerCC_UpdateBlob(CvBlobTrackerCC *_this, int BlobIndex, IplImage *pImgFG) {
	_this->UpdateBlob(BlobIndexd, pImgFG);
}

void CvBlobTrackerCC_Update(CvBlobTrackerCC *_this, IplImage *pImg, IplImage *pImgFG) {
	_this->Update(pImg, pImgFG);
}

int CvBlobTrackerCC_GetBlobIndexByID(CvBlobTrackerCC *_this, int BlobID) {
	return this->GetBlobIndexByID(BlobID);
}

CvBlob *CvBlobTrackerCC_GetBlobByID(CvBlobTrackerCC *_this, int BlobID) {
	return _this->GetBlobByID(BlobID);
}

void CvBlobTrackerCC_DelbBlobByID(CvBlobTrackerCC *_this, int BlobID) {
	_this->DelBlobByID(BlobID);
}

void CvBlobTrackerCC_SetBlobByID(CvBlobtrackerCC *_this, int BlobID, CvBlob *pBlob) {
	_this->SetBlobByID(BlobID, pBlob);
}

void CvBlobTrackerCC_ParamUpdate(CvBlobtrackerCC *_this) {
	_this->ParamUpdate();
}

int CvBlobTrackerCC_GetBlobHypNum(CvBlobTrackerCC *_this, int BlobIndex) {
	return _this->GetBlobHypNum(BlobIndex);
}

CvBlob *CvBlobTrackerCC_GetBlobHyp(CvBlobTrackerCC *_this, int BlobIndex, int Hypothesis) {
	return _this->GetBlobHyp(BlobIndex, Hypothesis);
}

void CvBlobTrackerCC_SetBlobHyp(CvBlobTrackerCC *_this, int BlobIndex, CvBlob *pBlob) {
	_this->SetBlobHyp(BlobIndex, pBlob);
}

// CvBlobTrackPredictKalman wrappers
CvBlobTrackPredictKalman *new_CvBlobTrackPredictKalman() {
	return new CvBlobTrackPredictKalman();
}

void delete_CvBlobTrackPredictKalman(CvBlobTrackPredictKalman *_this) {
	delete _this;
}

CvBlob *CvBlobTrackPredictKalman_Predict(CvBlobTrackPredictKalman(CvBlobTrackPredictKalman *_this) {
	return _this->Predict();
}

void CvBlobTrackPredictKalman_Update(CvBlobTrackPredictKalman *_this, CvBlob *pBlob) {
	_this->Update(pBlob);
}

void CvBlobTrackPredictKalman_Release(CvBlobTrackPredictKalman *_this) {
	_this->Release();
}
