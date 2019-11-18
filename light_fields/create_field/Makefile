
PREFIX=/home/ctorney/local



OBJS = noise.o

CXX=g++

CXXFLAGS= -O3 -Wall -Wno-deprecated `freetype-config --cflags` 

INC=  -I$(PREFIX)/include/

LIBS= -L$(PREFIX)/lib/ -lz -lpng -lpngwriter -lgsl -lgslcblas -lm -lfreetype



all: pnggrad

pnggrad: ${OBJS} main.cpp noise.cpp
	$(CXX) $(CXXFLAGS) $(INC) ${OBJS} main.cpp -o pnggrad $(LIBS) 


${OBJS}: %.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $(INC) noise.cpp -o noise.o $(LIBS) 





