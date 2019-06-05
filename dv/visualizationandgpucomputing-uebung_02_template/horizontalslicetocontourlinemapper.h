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
    QVector3D isoCrossing(QVector3D p1, QVector3D p2, float v1, float v2, float isoValue);
    void debugCrossing(QVector3D vec1, QVector3D vec2, float v1, float v2, QVector3D isoC);
};

#endif // HORIZONTALSLICETOCONTOURLINEMAPPER_H
