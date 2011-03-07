#include <opencv2/opencv.hpp>
#include <iostream>

int main (int argc, char * argv[]){
  IplImage * image = cvLoadImage(argv[1]);
  IplImage * image_small = cvCreateImage(cvSize(640,480),8,3);
  cvResize(image,image_small);
  cvSave("small.png",image_small);

  cvReleaseImage(&image);
  cvReleaseImage(&image_small);
}
