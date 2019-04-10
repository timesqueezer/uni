#include <iostream>
#include <stdio.h>
#include <cmath>

#include "flowdatasource.h"
#include "tornado.c"

FlowDataSource::FlowDataSource() {}

FlowDataSource::~FlowDataSource()
{
    delete[] cartesianDataGrid;
}

void FlowDataSource::createData(int n_size, int time)
{
    if (cartesianDataGrid==nullptr) {
        delete[] cartesianDataGrid;
    }

    size = n_size;
    cartesianDataGrid = new float[size * size * size * 3];

    gen_tornado(size, size, size, time, cartesianDataGrid);
}

float FlowDataSource::getDataValue(int iz, int iy, int ix, int ic)
{
    int index = iz;
    index += iy * size;
    index += ix * iy * size * 3;
    index += ic;
    return cartesianDataGrid[index];
}

float FlowDataSource::getLength(int iz, int iy, int ix)
{
    float c0 = getDataValue(iz, iy, ix, 0);
    float c1 = getDataValue(iz, iy, ix, 1);
    float c2 = getDataValue(iz, iy, ix, 2);

    return sqrt(c0*c0
                +c1*c1
                +c2*c2);
}

void FlowDataSource::printValuesOfHorizontalSlice(int iz)
{
    for (int iy = 0; iy < size; ++iy)
    {
        for (int ix = 0; ix < size; ++ix)
        {
            float vx = getDataValue(iz, iy, iz, 0);
            float vy = getDataValue(iz, iy, iz, 1);
            float vz = getDataValue(iz, iy, iz, 2);

            // std::cout << '(' << vx << ',' << vy << ',' << vz << ") ";
            printf("(%.2f,%.2f,%.2f) ", vx, vy, vz);
        }
        std::cout << std::endl;
    }
}

float FlowDataSource::getMaxValueOfComponent(int ic)
{
    float max = 0.0;

    for (int iz = 0; iz < size; ++iz)
    {
        for (int iy = 0; iy < size; ++iy)
        {
            for (int ix = 0; ix < size; ++ix)
            {
                float dv = abs(getDataValue(iz, iy, ix, ic));
                if (dv > max)
                    max = dv;
            }
        }
    }

    return max;
}

float FlowDataSource::getMaxLength()
{
    float max = 0.0;

    for (int iz = 0; iz < size; ++iz)
    {
        for (int iy = 0; iy < size; ++iy)
        {
            for (int ix = 0; ix < size; ++ix)
            {
                float dv = getLength(iz, iy, ix);
                if (dv > max)
                    max = dv;
            }
        }
    }

    return max;
}
