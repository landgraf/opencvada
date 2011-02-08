#include "macros.hpp"

void cvNextLinePoint(CvLineIterator * LineIterator){
  CV_NEXT_LINE_POINT(*LineIterator);
}

void cvNextSeqElem(int elem_size, CvSeqReader * reader) {
  CV_NEXT_SEQ_ELEM(elem_size, *reader);
}



/* Convert c++ vector into c vector structs */
template <typename _tp,typename _t> _tp *vector_to_c_vector(vector<_t> src){
  _tp * temp = (_tp *)malloc(sizeof(_tp));
  temp->data = (_t *)malloc(sizeof(_t) * src.size());
  temp->count = src.size();

  for (int i = 0; i < temp->count; i++){
    temp->data[i] = src[i];
  }
  return temp;
}
/* Convert c vector struct into c++ vector */
template <typename _tp,typename _t> vector<_t> c_vector_to_vector(_tp *src){
  vector<_t> temp;

  temp.resize(src->count);
  for (int i = 0; i < temp.size(); i++){
    temp[i] = src->data[i];
  }

  return temp;
}

double find_obj_compareSURFDescriptors( const float* d1, const float* d2, double best, int length )
{
  double total_cost = 0;
  assert( length % 4 == 0 );
  for( int i = 0; i < length; i += 4 )
    {
      double t0 = d1[i  ] - d2[i  ];
      double t1 = d1[i+1] - d2[i+1];
      double t2 = d1[i+2] - d2[i+2];
      double t3 = d1[i+3] - d2[i+3];
      total_cost += t0*t0 + t1*t1 + t2*t2 + t3*t3;
      if( total_cost > best )
	break;
    }
  return total_cost;
}

int find_obj_naiveNearestNeighbor( const float* vec, int laplacian,
				  const CvSeq* model_keypoints,
				  const CvSeq* model_descriptors )
{
  int length = (int)(model_descriptors->elem_size/sizeof(float));
  int i, neighbor = -1;
  double d, dist1 = 1e6, dist2 = 1e6;
  CvSeqReader reader, kreader;
  cvStartReadSeq( model_keypoints, &kreader, 0 );
  cvStartReadSeq( model_descriptors, &reader, 0 );

  for( i = 0; i < model_descriptors->total; i++ )
    {
      const CvSURFPoint* kp = (const CvSURFPoint*)kreader.ptr;
      const float* mvec = (const float*)reader.ptr;
      CV_NEXT_SEQ_ELEM( kreader.seq->elem_size, kreader );
      CV_NEXT_SEQ_ELEM( reader.seq->elem_size, reader );
      if( laplacian != kp->laplacian )
	continue;
      d = find_obj_compareSURFDescriptors( vec, mvec, dist2, length );
      if( d < dist1 )
	{
	  dist2 = dist1;
	  dist1 = d;
	  neighbor = i;
	}
      else if ( d < dist2 )
	dist2 = d;
    }
  if ( dist1 < 0.6*dist2 )
    return neighbor;
  return -1;
}

void find_obj_findPairs( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
			const CvSeq* imageKeypoints, const CvSeq* imageDescriptors, int ** c_ptpairs, int * length )
{
  int i;
  CvSeqReader reader, kreader;
  vector<int> ptpairs;
  cvStartReadSeq( objectKeypoints, &kreader );
  cvStartReadSeq( objectDescriptors, &reader );
  ptpairs.clear();
  int * temp;

  for( i = 0; i < objectDescriptors->total; i++ )
    {
      const CvSURFPoint* kp = (const CvSURFPoint*)kreader.ptr;
      const float* descriptor = (const float*)reader.ptr;
      CV_NEXT_SEQ_ELEM( kreader.seq->elem_size, kreader );
      CV_NEXT_SEQ_ELEM( reader.seq->elem_size, reader );
      int nearest_neighbor = find_obj_naiveNearestNeighbor( descriptor, kp->laplacian, imageKeypoints, imageDescriptors );
      if( nearest_neighbor >= 0 )
	{
	  ptpairs.push_back(i);
	  ptpairs.push_back(nearest_neighbor);
	}
    }

/*   _tp * temp = (_tp *)malloc(sizeof(_tp)); */
  temp = (int *)malloc(sizeof(int) * ptpairs.size());
  *length = ptpairs.size();

  for (int i = 0; i < *length; i++){
    temp[i] = ptpairs[i];
  }
  *c_ptpairs = temp;
}

/* a rough implementation for object location */
int find_obj_locatePlanarObject( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
				const CvSeq* imageKeypoints, const CvSeq* imageDescriptors,
				const CvPoint src_corners[4], CvPoint dst_corners[4] )
{
  double h[9];
  CvMat _h = cvMat(3, 3, CV_64F, h);
  vector<int> ptpairs;
  vector<CvPoint2D32f> pt1, pt2;
  CvMat _pt1, _pt2;
  int i, n;

/*   find_obj_findPairs( objectKeypoints, objectDescriptors, imageKeypoints, imageDescriptors, ptpairs ); */

  n = (int)(ptpairs.size()/2);
  if( n < 4 )
    return 0;

  pt1.resize(n);
  pt2.resize(n);
  for( i = 0; i < n; i++ )
    {
      pt1[i] = ((CvSURFPoint*)cvGetSeqElem(objectKeypoints,ptpairs[i*2]))->pt;
      pt2[i] = ((CvSURFPoint*)cvGetSeqElem(imageKeypoints,ptpairs[i*2+1]))->pt;
    }

  _pt1 = cvMat(1, n, CV_32FC2, &pt1[0] );
  _pt2 = cvMat(1, n, CV_32FC2, &pt2[0] );
  if( !cvFindHomography( &_pt1, &_pt2, &_h, CV_RANSAC, 5 ))
    return 0;

  for( i = 0; i < 4; i++ )
    {
      double x = src_corners[i].x, y = src_corners[i].y;
      double Z = 1./(h[6]*x + h[7]*y + h[8]);
      double X = (h[0]*x + h[1]*y + h[2])*Z;
      double Y = (h[3]*x + h[4]*y + h[5])*Z;
      dst_corners[i] = cvPoint(cvRound(X), cvRound(Y));
    }

  return 1;
}


