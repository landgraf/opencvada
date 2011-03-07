#include <opencv2/opencv.hpp>
#include <iostream>
#include <string>

string list[100];

int main (int argc, char * argv[]){
  IplImage * image = cvLoadImage(argv[1]);
  for(int i = 0; i < 100; i++){
    list[i] = string(i,'n');
    cvNamedWindow(list[i].c_str());
  }

  for(int i = 0; i < 100; i++)
    cvShowImage(list[i].c_str(),image);

  for(int i = 0; i < 100; i++)
    cvDestroyWindow(list[i].c_str());

  cvReleaseImage(&image);
}
