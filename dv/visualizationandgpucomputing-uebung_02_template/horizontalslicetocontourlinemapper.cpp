#include "horizontalslicetocontourlinemapper.h"

HorizontalSliceToContourLineMapper::HorizontalSliceToContourLineMapper()
{

}

void HorizontalSliceToContourLineMapper::setDataSource(FlowDataSource* source) {
    flowDataSource = source;
}

QVector<QVector3D> HorizontalSliceToContourLineMapper::mapSliceToContourLineSegments(int iz) {
    QVector<QVector3D> list;

    /*QVector3D vec1(0.5, 1, 1);
    QVector3D vec2(1, 0.75, 1);
    QVector3D vec3(0.25, 1, 1);
    QVector3D vec4(0.5, 1, 1);

    list.append(vec1);
    list.append(vec2);
    list.append(vec3);
    list.append(vec4);*/

    int dataSize = flowDataSource->getDataSize();
    float isoValue = 0.05;

    float zPos = (float)iz / (float)dataSize;
    float cellSize = 1.0 / (float)dataSize;
    float halfSize = cellSize / 0.5;

    for (int iy = 0; iy < dataSize - 1; ++iy) {
        for (int ix = 0; ix < dataSize - 1; ++ix) {
            float v1 = flowDataSource->getDataValue(ix, iy, iz, 0);
            float v2 = flowDataSource->getDataValue(ix+1, iy, iz, 0);
            float v3 = flowDataSource->getDataValue(ix+1, iy+1, iz, 0);
            float v4 = flowDataSource->getDataValue(ix, iy+1, iz, 0);

            int bitMask = 0;
            if (v1 > isoValue)
                bitMask |= 1<<3;

            if (v2 > isoValue)
                bitMask |= 1<<2;

            if (v3 > isoValue)
                bitMask |= 1<<1;

            if (v4 > isoValue)
                bitMask |= 1;

            float xPos = (float)ix / (float)dataSize;
            float yPos = (float)iy / (float)dataSize;

            // std::cout << bitMask << std::endl;

            if (bitMask == 1) {
                list.append(QVector3D(xPos + 0  , yPos + halfSize, zPos));
                list.append(QVector3D(xPos + halfSize, yPos + cellSize, zPos));
            } else if (bitMask == 2) {
                list.append(QVector3D(xPos + cellSize, yPos + halfSize, zPos));
                list.append(QVector3D(xPos + halfSize, yPos + cellSize, zPos));
            } else if (bitMask == 3) {
                list.append(QVector3D(xPos + cellSize, yPos + halfSize, zPos));
                list.append(QVector3D(xPos + halfSize, yPos + cellSize, zPos));
            } else if (bitMask == 4) {
                list.append(QVector3D(xPos + cellSize, yPos + halfSize, zPos));
                list.append(QVector3D(xPos + halfSize, yPos + cellSize, zPos));
            }
        }
    }

    return list;
}

int HorizontalSliceToContourLineMapper::getDataSize()
{
    return flowDataSource->getDataSize();
}
