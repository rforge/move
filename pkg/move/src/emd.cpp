#include "emd_hat.hpp"
#include "emd_hat_signatures_interface.hpp"
#include "emd_hat_impl.hpp"
#include <vector>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#define d2r (M_PI / 180.0) 

//calculate haversine distance for linear distance
//http://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
double haversine_km(feature_tt *F1,feature_tt *F2) //calculate distance for 2 coordinates (F1, F2)
{
    double dlong = (F2->X - F1->X) * d2r;
    double dlat = (F2->Y - F1->Y) * d2r;
    double a = std::min(pow(sin(dlat/2.0), 2) + cos(F1->Y*d2r) * cos(F2->Y*d2r) * pow(sin(dlong/2.0), 2), double (1));
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    double d = 6367 * c;

    return d;
}
double haversine_km_xy(double *X1, double *Y1, double *X2, double *Y2) //calculate distance for 2 points (X1,Y1 and X2,Y2)
{
    double dlong = (*X2 - *X1) * d2r;
    double dlat = (*Y2 - *Y1) * d2r;
    double a = std::min(pow(sin(dlat/2.0), 2) + cos(*Y1*d2r) * cos(*Y2*d2r) * pow(sin(dlong/2.0), 2),double (1));
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    double d = 6367 * c;

    return d;
}


//calculate euclidean distance for a linear distance 
double eucl_dist(feature_tt *F1, feature_tt *F2) //calculate distance for 2 coordinates (F1, F2)
{
	double dX =F1->X-F2->X,dY=F1->Y-F2->Y;
	return sqrt(dX*dX+dY*dY);
}

double eucl_dist_xy(double *X1, double *Y1, double *X2, double *Y2) //calculate distance for 2 points (X1,Y1 and X2,Y2)
{
	double dX =*X1-*X2,dY=*Y1-*Y2; 
	return sqrt(dX*dX+dY*dY);
}

//Calculation of the EMD; to be used in R
//without a ground distance (slow variant) 
//P and Q are the start and the aim distribution, where 
//x's and y's are the coordinates of the feature, 
//n is the number of feature,
//w is the weight (or z-value, or height),
//th is the threshold value, gc describes as integer/boolean the usage of a grand circle 
extern "C" {
void emdR(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc)
{
    signature_tt<double> Psig2;
    signature_tt<double> Qsig2;
 	Qsig2.n= *Qn;
 	Psig2.n= *Pn;
    Psig2.Features= new feature_tt[Pn[0]];
    Qsig2.Features= new feature_tt[Qn[0]];
    Psig2.Weights= new double[Pn[0]];
    Qsig2.Weights= new double[Qn[0]];
    {for (unsigned int i=0; i<Qn[0]; ++i) {
        Qsig2.Features[i].X= Qx[i];
        Qsig2.Features[i].Y= Qy[i];
    }}
    Qsig2.Weights= Qw;
    {for (unsigned int i=0; i<Pn[0]; ++i) {
        Psig2.Features[i].X= Px[i];
        Psig2.Features[i].Y= Py[i];
    }}
    Psig2.Weights= Pw;
    double (*op)(feature_tt *F1,feature_tt *F2) = {NULL};
    if(*gc){
	op=&haversine_km;
    }else{
	op=&eucl_dist;
    }
    *res= emd_hat_signature_interface<double>(&Psig2, &Qsig2, *op,-1);
//    *res=eucl_dist(&Psig2.Features[0], &Psig2.Features[3]);
}

}

//Calculation of the EMD; to be used in R
//with a ground distance (faster variant) 
extern "C" {
void emdR_gd(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc)
{
    double THRESHOLD=*th;
    double (*op_xy)(double *X1, double *Y1, double *X2, double *Y2) = {NULL};
    if(*gc){				//if great circle is set to TRUE
	op_xy=&haversine_km_xy;	//the memory address of the haversine distance is set for op_xy
    }else{					
	op_xy=&eucl_dist_xy;	//else the memory address of the euclidean distance is set for op_xy
    }
//#### Define cost matrix ####
    std::vector< std::vector<double> > cost_mat; // for emd_hat version, defined globally only because of Rubner's version
    std::vector<double> cost_mat_row(*Pn);
    for (unsigned int i=0; i<*Qn; ++i) cost_mat.push_back(cost_mat_row);
    std::vector<double> Qim,Pim;
    for (unsigned int i=0; i<*Pn; ++i) Pim.push_back(Pw[i]);
    for (unsigned int i=0; i<*Qn; ++i) Qim.push_back(Qw[i]);
    int max_cost_mat= -1;
    int j= -1;
    for (unsigned int j=0; j<*Qn; j++) {   //going through all features of the aimed raster/distribution/histogram
            for (unsigned int i=0; i<*Pn; i++) {		//and connect all P features to each Q feature
                    cost_mat[i][j]= std::min(THRESHOLD,(op_xy(&Px[j],&Py[j],&Qx[i],&Qy[i])));  //creating the matrix column- and row-wise; by either taking the smalles: THRESHOLD or calculate distance op_xy 
                    if (cost_mat[i][j]>max_cost_mat) max_cost_mat= cost_mat[i][j]; //assigning the larger cost (either the calculated cost_mat, or the max_cost_mat) ??Why the larger one?
            }
    }
    *res= emd_hat_gd_metric<double>()(Qim,Pim, cost_mat,THRESHOLD);
}
}
//
//
//
//
//
//
//
//
//
//std::vector< std::vector<int> > cost_mat; // for emd_hat version, defined globally only because of Rubner's version
//
//int cost_mat_dist_int(feature_tt *F1, feature_tt *F2) { return cost_mat[*F1][*F2]; } // for emd_hat_signatures_interface
//
//
//void readImage(const char* im_name,
//               unsigned int& im_R,
//               unsigned int& im_C,
//               std::vector<int>& im) {
//    
//    std::fstream fin(im_name);
//    if (!fin) goto readImageErrLabel;
//    
//    fin >> im_R;
//    if (!fin) goto readImageErrLabel;
//    fin >> im_C;
//    if (!fin) goto readImageErrLabel;
//    
//    int tmp;
//    while (fin >> tmp) {
//        im.push_back(tmp);
//    }
//    
//    if (im.size()==im_R*im_C) return;
//readImageErrLabel:
//    std::cerr << "Image " << im_name << " has a problem in its format" << std::endl;
//    exit(1);
//    
//} // readImage
////--------------------------------------------------------------------------------------------
//
//
//
//
//int main( int argc, char* argv[]) {
//
//    //-----------------------------------------------
//    // Read images
//    const char* im1_name= "FastEMD/cameraman.txt";
//    const char* im2_name= "FastEMD/rice.txt";
//    unsigned int im1_R, im1_C, im2_R, im2_C;
//    std::vector<int> im1,im2;
//    readImage(im1_name, im1_R, im1_C, im1);
//    readImage(im2_name, im2_R, im2_C, im2);
//    if ( (im1_R!=im2_R) || (im1_C!=im2_C) ) {
//        std::cerr << "Images should be of the same size" << std::endl;
//    }
//    //-----------------------------------------------
//
//    
//    //-----------------------------------------------
//    // The ground distance - thresholded Euclidean distance.
//    // Since everything is ints, we multiply by COST_MULT_FACTOR.
//    const int COST_MULT_FACTOR= 1000;
//    const int THRESHOLD= 3*COST_MULT_FACTOR;//1.412*COST_MULT_FACTOR;
//    // std::vector< std::vector<int> > cost_mat; // here it's defined as global for Rubner's interfaces.
//                                                   // If you do not use Rubner's interface it's a better idea
//                                                   // not to use globals.
//    std::vector<int> cost_mat_row(im1_R*im1_C);
//    for (unsigned int i=0; i<im1_R*im1_C; ++i) cost_mat.push_back(cost_mat_row);
//    int max_cost_mat= -1;
//    int j= -1;
//    for (unsigned int c1=0; c1<im1_C; ++c1) {
//        for (unsigned int r1=0; r1<im1_R; ++r1) {
//            ++j;
//            int i= -1;
//            for (unsigned int c2=0; c2<im1_C; ++c2) {
//                for (unsigned int r2=0; r2<im1_R; ++r2) {
//                    ++i;
//                    cost_mat[i][j]= std::min(THRESHOLD,static_cast<int>(COST_MULT_FACTOR*sqrt((r1-r2)*(r1-r2)+(c1-c2)*(c1-c2))));
//                    if (cost_mat[i][j]>max_cost_mat) max_cost_mat= cost_mat[i][j];
//                }
//            }
//		}
//	}
//    //-----------------------------------------------
//    
//    
//
//    //-----------------------------------------------
//    // Convert to FastEMD with Rubner's interface
//    //-----------------------------------------------
//    signature_tt<int> Psig2;
//    signature_tt<int> Qsig2;
//    Psig2.n= im1_R*im1_C;
//    Qsig2.n= im1_R*im1_C;
//    Psig2.Features= new feature_tt[im1_R*im1_C];
//    Qsig2.Features= new feature_tt[im1_R*im1_C];
//    {for (unsigned int i=0; i<im1_R*im1_C; ++i) {
//        Psig2.Features[i]= i;
//        Qsig2.Features[i]= i;
//    }}
//    Psig2.Weights= new int[im1_R*im1_C];
//    Qsig2.Weights= new int[im1_R*im1_C];
//    {for (unsigned int i=0; i<im1_R*im1_C; ++i) {
//        Psig2.Weights[i]= im1[i];
//        Qsig2.Weights[i]= im2[i];
//    }}
//    //-----------------------------------------------
//    
//    tictoc timer;
//    timer.tic();
//    int emd_hat_gd_metric_val= emd_hat_gd_metric<int>()(im1,im2, cost_mat,THRESHOLD);
//    timer.toc();
//    std::cout << "emd_hat_gd_metric time in seconds: " << timer.totalTimeSec() << std::endl;
//
//    timer.clear();
//    timer.tic();
//    int emd_hat_val= emd_hat<int>()(im1,im2, cost_mat,THRESHOLD);
//    timer.toc();
//    std::cout << "emd_hat time in seconds: " << timer.totalTimeSec() << std::endl;
//
//    timer.clear();
//    timer.tic();
//    int emd_hat_signatures_interface_val= emd_hat_signature_interface<int>(&Psig2, &Qsig2, cost_mat_dist_int,-1);
//    timer.toc();
//    std::cout << "emd_hat_signatures_interface time in seconds: " << timer.totalTimeSec() << std::endl;
//
//    delete[] Psig2.Features;
//    delete[] Qsig2.Features;
//    delete[] Psig2.Weights;
//    delete[] Qsig2.Weights;
//
//    if (emd_hat_gd_metric_val!=emd_hat_val||
//        emd_hat_gd_metric_val!=emd_hat_signatures_interface_val
//        ) {
//        std::cerr << "EMDs that were computed with different interfaces are different!" << std::endl;
//        std::cerr << "emd_hat_gd_metric_val==" << emd_hat_gd_metric_val << std::endl;
//        std::cerr << "emd_hat_val==" << emd_hat_val << std::endl;
//        std::cerr << "emd_hat_signatures_interface_val==" << emd_hat_signatures_interface_val << std::endl;
//        return 1;
//    }
//} // end main
//    
//    
//// Copyright (c) 2009-2011, Ofir Pele
//// All rights reserved.
//
//// Redistribution and use in source and binary forms, with or without
//// modification, are permitted provided that the following conditions are
//// met: 
////    * Redistributions of source code must retain the above copyright
////    notice, this list of conditions and the following disclaimer.
////    * Redistributions in binary form must reproduce the above copyright
////    notice, this list of conditions and the following disclaimer in the
////    documentation and/or other materials provided with the distribution.
////    * Neither the name of the The Hebrew University of Jerusalem nor the
////    names of its contributors may be used to endorse or promote products
////    derived from this software without specific prior written permission.
//
//// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
//// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
//// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
//// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
