#include <opencv2/opencv.hpp>
#include <iostream>


typedef struct _elem {
  float x;
  float y;
  float radius;
} elem;

int main (int argc, char * argv[]){
  CvCapture * capture;
  if(argc)
    capture = cvCreateFileCapture(argv[1]);
  else
    return -1;
  int frames = 0;
  IplImage * frame = cvQueryFrame(capture);
  for (;frame != 0;frame = cvQueryFrame(capture)){
    CvMat * storage = cvCreateMat(1,10000,CV_32FC3);
    IplImage * bw = cvCreateImage(cvGetSize(frame),8,1);
    IplImage * recolor = cvCreateImage(cvGetSize(frame),8,3);
    cvCvtColor(frame,bw,CV_BGR2GRAY);
    cvCanny(bw,bw,10.0,240.0);
    CvSeq * circles = cvHoughCircles(bw,storage,CV_HOUGH_GRADIENT,1.0, bw->height / 10,300.0, 30.0);
    cvCvtColor(bw,recolor,CV_GRAY2BGR);
    for(int i = 0;i < storage->cols; i++){
      elem circle = CV_MAT_ELEM(*storage,elem,0,i);
      cvDrawCircle(recolor,cvPoint(cvRound(circle.x),cvRound(circle.y)),cvRound(circle.radius),CV_RGB(255,0,0));
    }
    frames++;
    cvReleaseImage(&bw);
    cvReleaseImage(&recolor);
    cvReleaseMat(&storage);
  }
  cout << frames << endl;
  cvReleaseCapture(&capture);
}
