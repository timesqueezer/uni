#ifndef HORIZONTALSLICETOCONTOURLINEMAPPER_H
#define HORIZONTALSLICETOCONTOURLINEMAPPER_H

#include <iostream>

#include <QVector>
#include <QVector3D>

#include "flowdatasource.h"

class HorizontalSliceToContourLineMapper
{
public:
    HorizontalSliceToContourLineMapper();

    void setDataSource(FlowDataSource* source);

    QVector<QVector3D> mapSliceToContourLineSegments(int iz);
    int getDataSize();

private:
    FlowDataSource* flowDataSource;
};

#endif // HORIZONTALSLICETOCONTOURLINEMAPPER_H
