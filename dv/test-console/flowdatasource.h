#ifndef FLOWDATASOURCE_H
#define FLOWDATASOURCE_H


class FlowDataSource
{
    float* cartesianDataGrid;
    int size;

    public:
        FlowDataSource();
        ~FlowDataSource();

        void createData(int size, int time);

        float getDataValue(int iz, int iy, int ix, int ic);

        float getLength(int iz, int iy, int ix);

        void printValuesOfHorizontalSlice(int iz);

        float getMaxValueOfComponent(int ic);

        float getMaxLength();
};

#endif // FLOWDATASOURCE_H
