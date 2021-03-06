#include "horizontalslicetocontourlinemapper.h"

HorizontalSliceToContourLineMapper::HorizontalSliceToContourLineMapper()
{

}

void HorizontalSliceToContourLineMapper::setDataSource(FlowDataSource* source) {
    flowDataSource = source;
}

QVector<QVector3D> HorizontalSliceToContourLineMapper::mapSliceToContourLineSegments(int iz) {
    QVector<QVector3D> list;

    int dataSize = flowDataSource->getDataSize();
    float isoValue = 0.01;

    float zPos = 1 - ((float)iz / (float)dataSize);
    float cellSize = 1.0 / (float)dataSize;
    // float halfSize = cellSize * 0.5;

    // draw debug grid
    /*for (int iy = 0; iy < dataSize; ++iy) {
        for (int ix = 0; ix < dataSize; ++ix) {
            float xPos = (float)ix / (float)dataSize;
            float yPos = (float)iy / (float)dataSize;
            list.append(QVector3D(xPos, yPos, zPos));
            list.append(QVector3D(xPos+cellSize, yPos, zPos));
            list.append(QVector3D(xPos, yPos, zPos));
            list.append(QVector3D(xPos, yPos+cellSize, zPos));
        }
    }*/

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

            QVector3D vec_nw(xPos, yPos + cellSize, zPos);
            QVector3D vec_ne(xPos + cellSize, yPos + cellSize, zPos);
            QVector3D vec_se(xPos + cellSize, yPos, zPos);
            QVector3D vec_sw(xPos, yPos, zPos);

            int neswMask = 0;

            switch (bitMask) {
            case 1:
            case 14:
                neswMask = 12;
                break;

            case 2:
            case 13:
                neswMask = 6;
                break;

            case 3:
            case 12:
                neswMask = 10;
                break;

            case 4:
            case 11:
                neswMask = 3;
                break;

            case 5:
            case 10:
                neswMask = 15;
                break;

            case 6:
            case 9:
                neswMask = 5;
                break;

            case 7:
            case 8:
                neswMask = 9;
                break;
            }

            if ((neswMask & 1) == 1) {
                QVector3D isoC = isoCrossing(vec_nw, vec_ne, v_nw, v_ne, isoValue);
                // debugCrossing(vec_nw, vec_ne, v_nw, v_ne, isoC);
                list.append(isoC);
            }
            if ((neswMask & 2) == 2) {
                QVector3D isoC = isoCrossing(vec_ne, vec_se, v_ne, v_se, isoValue);
                // debugCrossing(vec_ne, vec_se, v_ne, v_se, isoC);
                list.append(isoC);
            }
            if ((neswMask & 4) == 4) {
                QVector3D isoC = isoCrossing(vec_se, vec_sw, v_se, v_sw, isoValue);
                // debugCrossing(vec_se, vec_sw, v_se, v_sw, isoC);
                list.append(isoC);
            }
            if ((neswMask & 8) == 8) {
                QVector3D isoC = isoCrossing(vec_sw, vec_nw, v_sw, v_nw, isoValue);
                // debugCrossing(vec_sw, vec_nw, v_sw, v_nw, isoC);
                list.append(isoC);
            }
        }
    }

    return list;
}

int HorizontalSliceToContourLineMapper::getDataSize()
{
    return flowDataSource->getDataSize();
}

QVector3D HorizontalSliceToContourLineMapper::isoCrossing(QVector3D p1, QVector3D p2, float v1, float v2, float isoValue)
{
    QVector3D isoC = p1 + (
        (p2 - p1) * ( abs(isoValue - v1) / abs(v2 - v1) )
    );

    return isoC;
}

void HorizontalSliceToContourLineMapper::debugCrossing(QVector3D vec1, QVector3D vec2, float v1, float v2, QVector3D isoC) {
    std::cout << "p1, p2, v1, v2, isoCrossing" << std::endl;
    std::cout << "(" << vec1.x() << "," << vec1.y() << "," << vec1.z() << "), ";
    std::cout << "(" << vec2.x() << "," << vec2.y() << "," << vec2.z() << "), ";
    std::cout << v1 << ", " << v2 << ", ";
    std::cout << "(" << isoC.x() << "," << isoC.y() << "," << isoC.z() << ")" << std::endl;
}
