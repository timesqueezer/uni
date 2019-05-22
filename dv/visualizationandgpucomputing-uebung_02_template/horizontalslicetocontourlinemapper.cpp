#include "horizontalslicetocontourlinemapper.h"

HorizontalSliceToContourLineMapper::HorizontalSliceToContourLineMapper()
{

}

void HorizontalSliceToContourLineMapper::setDataSource(FlowDataSource* source) {
    flowDataSource = source;
}

QVector<QVector3D> HorizontalSliceToContourLineMapper::mapSliceToContourLineSegments(int iz) {
    QVector<QVector3D> list;

    /*QVector3D vec1(1, 1, 1);
    QVector3D vec2(1, 0, 0);
    QVector3D vec3(0, 1, 0);
    QVector3D vec4(1, 1, 1);

    list.append(vec1);
    list.append(vec2);
    list.append(vec3);
    list.append(vec4);*/

    int dataSize = flowDataSource->getDataSize();
    float isoValue = 0.0;

    float zPos = 1.001 - ((float)iz / (float)dataSize);
    float cellSize = 1.0 / (float)dataSize;
    float halfSize = cellSize / 0.5;

    // draw debug grid
    for (int iy = 0; iy < 4 - 1; ++iy) {
        for (int ix = 0; ix < 4 - 1; ++ix) {
            float xPos = (float)ix / (float)dataSize;
            float yPos = (float)iy / (float)dataSize;
            list.append(QVector3D(xPos, yPos, zPos));
            list.append(QVector3D(xPos+cellSize, yPos, zPos));
            list.append(QVector3D(xPos, yPos, zPos));
            list.append(QVector3D(xPos, yPos+cellSize, zPos));
        }
    }

    for (int iy = 0; iy < dataSize - 1; ++iy) {
        for (int ix = 0; ix < dataSize - 1; ++ix) {
            float v_sw = flowDataSource->getDataValue(iz, iy, ix, 0);
            float v_nw = flowDataSource->getDataValue(iz, iy+1, ix, 0);
            float v_ne = flowDataSource->getDataValue(iz, iy+1, ix+1, 0);
            float v_se = flowDataSource->getDataValue(iz, iy, ix+1, 0);

            int bitMask = 0;
            if (v_nw > isoValue)
                bitMask |= 8;

            if (v_ne > isoValue)
                bitMask |= 4;

            if (v_se > isoValue)
                bitMask |= 2;

            if (v_sw > isoValue)
                bitMask |= 1;

            float xPos = (float)ix / (float)dataSize;
            float yPos = (float)iy / (float)dataSize;

            QVector3D vec_w(xPos, yPos + halfSize, zPos);
            QVector3D vec_e(xPos + cellSize, yPos + halfSize, zPos);
            QVector3D vec_n(xPos + halfSize, yPos + cellSize, zPos);
            QVector3D vec_s(xPos + halfSize, yPos, zPos);

            // std::cout << bitMask << std::endl;

            switch (bitMask) {
            case 1:
            case 14:
                list.append(vec_w);
                list.append(vec_s);
                break;

            case 2:
            case 13:
                list.append(vec_e);
                list.append(vec_s);
                break;

            case 3:
            case 12:
                list.append(vec_w);
                list.append(vec_e);
                break;

            case 4:
            case 11:
                list.append(vec_n);
                list.append(vec_e);
                break;

            case 5:
            case 10:
                list.append(vec_w);
                list.append(vec_n);
                list.append(vec_e);
                list.append(vec_s);
                break;

            case 6:
            case 9:
                list.append(vec_n);
                list.append(vec_s);
                break;

            case 7:
            case 8:
                list.append(vec_w);
                list.append(vec_n);
                break;
            }
        }
    }

    return list;
}

int HorizontalSliceToContourLineMapper::getDataSize()
{
    return flowDataSource->getDataSize();
}
