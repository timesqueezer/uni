#ifndef HORIZONTALSLICETOIMAGEMAPPER_H
#define HORIZONTALSLICETOIMAGEMAPPER_H

#include <QImage>

#include "flowdatasource.h"

class HorizontalSliceToImageMapper
{
public:
    HorizontalSliceToImageMapper();

    void setDataSource(FlowDataSource* source);

    QImage mapSliceToImage(int iz);
    int getDataSize();

private:
    FlowDataSource* flowDataSource;
};

#endif // HORIZONTALSLICETOIMAGEMAPPER_H
