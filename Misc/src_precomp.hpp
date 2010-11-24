


    
    





static inline Point normalizeAnchor( Point anchor, Size ksize )
;

void preprocess2DKernel( const Mat& kernel, vector<Point>& coords, vector<uchar>& coeffs );
void crossCorr( const Mat& src, const Mat& templ, Mat& dst,
                Size corrsize, int ctype,
                Point anchor=Point(0,0), double delta=0,
                int borderType=BORDER_REFLECT_101 );