#ifndef STREAMLINEMAPPER_H
#define STREAMLINEMAPPER_H

#include <iostream>
#include <cmath>

#include <QVector>
#include <QVector3D>

#include "flowdatasource.h"

class StreamLineMapper
{
public:
    StreamLineMapper();

    void setDataSource(FlowDataSource* source);

    QVector<QVector3D> mapToStreamLines(int iz);
    int getDataSize();

private:
    FlowDataSource* flowDataSource;

    QVector3D getInterpolatedDataValue(QVector3D, float);
};

#endif // STREAMLINEMAPPER_H
