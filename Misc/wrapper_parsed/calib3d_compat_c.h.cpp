void cvFindFundamentalMatrix (int * points1 , int * points2 , int numpoints , int CV_UNREFERENCED method , float * matrix ){
	cvFindFundamentalMatrix(/*int*//***/points1 , /*int*//***/points2 , /*int*/numpoints , /*int*//*CV_UNREFERENCED*/method , /*float*//***/matrix);
}
intcvFindChessBoardCornerGuesses (const void * arr , void * CV_UNREFERENCED thresharr , CvMemStorage * CV_UNREFERENCED storage , CvSize pattern_size , CvPoint2D32f * corners , int * corner_count ){
	return intcvFindChessBoardCornerGuesses(/*const*//*void*//***/arr , /*void*//***//*CV_UNREFERENCED*/thresharr , /*CvMemStorage*//***//*CV_UNREFERENCED*/storage , /*CvSize*/pattern_size , /*CvPoint2D32f*//***/corners , /*int*//***/corner_count);
}
void cvCalibrateCamera (int image_count , int * _point_counts , CvSize image_size , CvPoint2D32f * _image_points , CvPoint3D32f * _object_points , float * _distortion_coeffs , float * _camera_matrix , float * _translation_vectors , float * _rotation_matrices , int flags ){
	cvCalibrateCamera(/*int*/image_count , /*int*//***/_point_counts , /*CvSize*/image_size , /*CvPoint2D32f*//***/_image_points , /*CvPoint3D32f*//***/_object_points , /*float*//***/_distortion_coeffs , /*float*//***/_camera_matrix , /*float*//***/_translation_vectors , /*float*//***/_rotation_matrices , /*int*/flags);
}
void cvCalibrateCamera_64d (int image_count , int * _point_counts , CvSize image_size , CvPoint2D64f * _image_points , CvPoint3D64f * _object_points , double * _distortion_coeffs , double * _camera_matrix , double * _translation_vectors , double * _rotation_matrices , int flags ){
	cvCalibrateCamera_64d(/*int*/image_count , /*int*//***/_point_counts , /*CvSize*/image_size , /*CvPoint2D64f*//***/_image_points , /*CvPoint3D64f*//***/_object_points , /*double*//***/_distortion_coeffs , /*double*//***/_camera_matrix , /*double*//***/_translation_vectors , /*double*//***/_rotation_matrices , /*int*/flags);
}
void cvFindExtrinsicCameraParams (int point_count , CvSize CV_UNREFERENCED image_size , CvPoint2D32f * _image_points , CvPoint3D32f * _object_points , float * focal_length , CvPoint2D32f principal_point , float * _distortion_coeffs , float * _rotation_vector , float * _translation_vector ){
	cvFindExtrinsicCameraParams(/*int*/point_count , /*CvSize*//*CV_UNREFERENCED*/image_size , /*CvPoint2D32f*//***/_image_points , /*CvPoint3D32f*//***/_object_points , /*float*//***/focal_length , /*CvPoint2D32f*/principal_point , /*float*//***/_distortion_coeffs , /*float*//***/_rotation_vector , /*float*//***/_translation_vector);
}
void cvFindExtrinsicCameraParams_64d (int point_count , CvSize CV_UNREFERENCED image_size , CvPoint2D64f * _image_points , CvPoint3D64f * _object_points , double * focal_length , CvPoint2D64f principal_point , double * _distortion_coeffs , double * _rotation_vector , double * _translation_vector ){
	cvFindExtrinsicCameraParams_64d(/*int*/point_count , /*CvSize*//*CV_UNREFERENCED*/image_size , /*CvPoint2D64f*//***/_image_points , /*CvPoint3D64f*//***/_object_points , /*double*//***/focal_length , /*CvPoint2D64f*/principal_point , /*double*//***/_distortion_coeffs , /*double*//***/_rotation_vector , /*double*//***/_translation_vector);
}
void cvRodrigues (CvMat * rotation_matrix , CvMat * rotation_vector , CvMat * jacobian , int conv_type ){
	cvRodrigues(/*CvMat*//***/rotation_matrix , /*CvMat*//***/rotation_vector , /*CvMat*//***/jacobian , /*int*/conv_type);
}
void cvProjectPoints (int point_count , CvPoint3D64f * _object_points , double * _rotation_vector , double * _translation_vector , double * focal_length , CvPoint2D64f principal_point , double * _distortion , CvPoint2D64f * _image_points , double * _deriv_points_rotation_matrix , double * _deriv_points_translation_vect , double * _deriv_points_focal , double * _deriv_points_principal_point , double * _deriv_points_distortion_coeffs ){
	cvProjectPoints(/*int*/point_count , /*CvPoint3D64f*//***/_object_points , /*double*//***/_rotation_vector , /*double*//***/_translation_vector , /*double*//***/focal_length , /*CvPoint2D64f*/principal_point , /*double*//***/_distortion , /*CvPoint2D64f*//***/_image_points , /*double*//***/_deriv_points_rotation_matrix , /*double*//***/_deriv_points_translation_vect , /*double*//***/_deriv_points_focal , /*double*//***/_deriv_points_principal_point , /*double*//***/_deriv_points_distortion_coeffs);
}
void cvProjectPointsSimple (int point_count , CvPoint3D64f * _object_points , double * _rotation_matrix , double * _translation_vector , double * _camera_matrix , double * _distortion , CvPoint2D64f * _image_points ){
	cvProjectPointsSimple(/*int*/point_count , /*CvPoint3D64f*//***/_object_points , /*double*//***/_rotation_matrix , /*double*//***/_translation_vector , /*double*//***/_camera_matrix , /*double*//***/_distortion , /*CvPoint2D64f*//***/_image_points);
}
