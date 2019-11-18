
/*
% GNU Public Licence Copyright (c) Colin Torney
% Comments and questions to colin.j.torney@gmail.com

% This code is provided freely, however when using this code you are asked to cite our related paper: 
% Berdahl, A., Torney, C.J., Ioannou, C.C., Faria, J. & Couzin, I.D. (2013) Emergent sensing of complex environments by mobile animal groups, Science
*/

#include <pngwriter.h>
#include <math.h>
#include <iostream>
#include <sstream>
#include <noise.h>


using namespace std;
int main(int argc, char** argv) 
{


    int randseed=1;
    int seedoffset=321;
    if (argc>1)
        randseed=atoi(argv[1]);
    srand(randseed+seedoffset);


    int nx=500;
    int ny=300;

    int N=512;//powf(2,ceil(log2(ny)));
    clsNoise noise(N,0.025,randseed+seedoffset);
    
    float scale_noise=0.1;

    double* n_array = noise.get_address();
    float sigma=50;
    float amp=1.50;
    float x;
    float y;

    char fontname[35];
    sprintf(fontname, "/usr/share/X11/fonts/TTF/luximbi.ttf");
    //left
    float xmin=5;
    //right
    float xmax=490;
    //bottom edge
    float ymin=20;
    //top edge
    float ymax=300;

    float omega=0.01;
    float speed=1;


    int tmax=2880;

    float width = 1.0/(2.0*pow(sigma,1));
    
    int xrange = xmax-xmin;
    int yrange = ymax-ymin;

    int switch_time=0;
    float heading;

    int offset=20;
    x = (xmin+offset) + ((xrange-offset) * (rand()/(RAND_MAX+1.0)));
    y = (ymin+offset) + ((yrange-offset) * (rand()/(RAND_MAX+1.0)));

    //  tmax=200;
    float nmax=0.0;
    for (int i=0;i<N;i++)
        if (nmax<n_array[i])
            nmax=n_array[i];


    char maskname[11];
    //sprintf(maskname, "%02dnmask.png",randseed);
    sprintf(maskname, "nmask.png");
    pngwriter mask(1, 1, 0, maskname);
    mask.readfromfile(maskname);

    for (int t=0;t<tmax;t++)
    {

        //update new heading
        if (t==switch_time)
        {
            float nextx =(xmin+offset) + ((xrange-offset) * (rand()/(RAND_MAX+1.0)));
            float nexty =(ymin+offset) + ((yrange-offset) * (rand()/(RAND_MAX+1.0)));

            //           cout<<x<<":"<<y<<endl;
            //           cout<<nextx<<":"<<nexty<<":";
            switch_time = t+1+(int)(sqrt(pow(x-nextx,2)+pow(y-nexty,2))/speed);
            //         cout<<switch_time<<endl;
            heading = atan2(nexty-y, nextx-x);
            //           cout<<heading<<endl<<";;;"<<endl;
        }

        char fname[15];
        sprintf(fname, "output/%d/f%05d.png",randseed, t);

        pngwriter png(nx,ny,0.0,fname);

        x+=speed*cos(heading);
        y+=speed*sin(heading);
        float nav=0.0;
        int nc = 0;
        for(int i = xmin; i < xmax;i++)
            for(int j = ymin; j < ymax;j++)
            {
                nav+=n_array[N*j+i];
                nc++;
            }
        nav/=(nmax*(float)(nc));
        //       y=ymin+(ymax-ymin)*(1.0+offset+cos(omega*t) + offset*cos(omega*t*0.5) )/(2.0*(1.0+offset));
        for(int i = xmin; i < xmax;i++)
            for(int j = ymin; j < ymax;j++)
            {
                float x_d=i-x;
                float y_d=j-y;


                //               float grey = 1.0-amp*exp(-(powf(x_d,2)+pow(y_d,2))*width);
                //float grey = 1.0-amp*exp(-powf(pow(x_d,2)+pow(y_d,2),0.5)*width) + scale_noise*powf(n_array[N*((int)floor(1.0*j))+(int)floor(1.0*i)]/nmax,3);
                float grey = 1.0-amp*exp(-powf(pow(x_d,2)+pow(y_d,2),0.5)*width) + scale_noise*((n_array[N*j+i]/nmax)-nav);
                //              float grey = 0.5-scale_noise*powf(n_array[N*((int)floor(j))+(int)floor(i)]/(0.5*nmax),1);
                //               cout<<grey<<endl;

                if ((float)mask.read(i,j)/256.0>0.5)
                    grey=0.0;
                png.plot(i,j, grey, grey, grey);
            }
        char frstring[15];
        sprintf(frstring, "%1d-%04d", randseed,t);
        png.plot_text_utf8(fontname,11,348,1,0.0, frstring,1.0,1.0,1.0);


        png.close();
        mask.close();
        noise.advance_timestep();

    }

    return 0;
}

