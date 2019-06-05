#include "streamlinemapper.h"

StreamLineMapper::StreamLineMapper()
{

}

void StreamLineMapper::setDataSource(FlowDataSource* source) {
    flowDataSource = source;
}

QVector<QVector3D> StreamLineMapper::mapToStreamLines(int iz) {
    QVector<QVector3D> list;

    int dataSize = flowDataSource->getDataSize();

    float zPos = 1.001 - ((float)iz / (float)dataSize);
    float cellSize = 1.0 / static_cast<float>(dataSize);
    float stepSize = 0.1;
    float maxStep = 0.5;

    std::cout << "cellSize: " << cellSize << std::endl;

    for (int iy = 0; iy < dataSize; ++iy) {
        for (int ix = 0; ix < dataSize; ++ix) {
            float xPos = static_cast<float>(ix) / static_cast<float>(dataSize);
            float yPos = static_cast<float>(iy) / static_cast<float>(dataSize);

            QVector3D current_pos(xPos, yPos, zPos);
            QVector3D direction(
                flowDataSource->getDataValue(iz, iy, ix, 0),
                flowDataSource->getDataValue(iz, iy, ix, 1),
                flowDataSource->getDataValue(iz, iy, ix, 2)
            );

            for (float step = 0; step < maxStep; step += stepSize) {
                // direction.normalize();
                if (current_pos.x() > 1 || current_pos.y() > 1 || current_pos.z() > 1) {
                    std::cout << "break at step " << step << std::endl;
                    break;
                }

                list.append(current_pos);
                current_pos += direction * stepSize;
                list.append(current_pos);
                // std::cout << direction.length() << std::endl;
                direction = getInterpolatedDataValue(current_pos, static_cast<float>(dataSize));
            }
        }
    }

    /*list.append(QVector3D(1, 1, 1));
    list.append(QVector3D(0, 0, 0));*/

    return list;
}

QVector3D StreamLineMapper::getInterpolatedDataValue(QVector3D current_pos, float cellSize) {
    float x1 = floor(current_pos.x() * cellSize);
    float x2 = ceil(current_pos.x() * cellSize);
    float y1 = floor(current_pos.y() * cellSize);
    float y2 = ceil(current_pos.y() * cellSize);
    float z1 = floor(current_pos.z() * cellSize);
    float z2 = ceil(current_pos.z() * cellSize);
    std::cout << x1 << " " << x2 << " " << current_pos.x() * cellSize << std::endl;

    QVector3D x1y1z1(
        flowDataSource->getDataValue(z1, y1, x1, 0),
        flowDataSource->getDataValue(z1, y1, x1, 1),
        flowDataSource->getDataValue(z1, y1, x1, 2)
    );

    QVector3D x1y1z2(
        flowDataSource->getDataValue(z2, y1, x1, 0),
        flowDataSource->getDataValue(z2, y1, x1, 1),
        flowDataSource->getDataValue(z2, y1, x1, 2)
    );

    QVector3D x1y2z1(
        flowDataSource->getDataValue(z1, y2, x1, 0),
        flowDataSource->getDataValue(z1, y2, x1, 1),
        flowDataSource->getDataValue(z1, y2, x1, 2)
    );

    QVector3D x1y2z2(
        flowDataSource->getDataValue(z2, y2, x1, 0),
        flowDataSource->getDataValue(z2, y2, x1, 1),
        flowDataSource->getDataValue(z2, y2, x1, 2)
    );

    QVector3D x2y1z1(
        flowDataSource->getDataValue(z1, y1, x2, 0),
        flowDataSource->getDataValue(z1, y1, x2, 1),
        flowDataSource->getDataValue(z1, y1, x2, 2)
    );

    QVector3D x2y1z2(
        flowDataSource->getDataValue(z2, y1, x2, 0),
        flowDataSource->getDataValue(z2, y1, x2, 1),
        flowDataSource->getDataValue(z2, y1, x2, 2)
    );

    QVector3D x2y2z1(
        flowDataSource->getDataValue(z1, y2, x2, 0),
        flowDataSource->getDataValue(z1, y2, x2, 1),
        flowDataSource->getDataValue(z1, y2, x2, 2)
    );

    QVector3D x2y2z2(
        flowDataSource->getDataValue(z2, y2, x2, 0),
        flowDataSource->getDataValue(z2, y2, x2, 1),
        flowDataSource->getDataValue(z2, y2, x2, 2)
    );

    float x_d = (current_pos.x() - x1) / (x2 - x1);
    float y_d = (current_pos.y() - y1) / (y2 - y1);
    float z_d = (current_pos.z() - z1) / (z2 - z1);

    QVector3D c00( ( x1y1z1 * (1 - x_d) ) + ( x2y1z1 * x_d ) );
    QVector3D c01( ( x1y1z2 * (1 - x_d) ) + ( x2y1z2 * x_d ) );
    QVector3D c10( ( x1y2z1 * (1 - x_d) ) + ( x2y2z1 * x_d ) );
    QVector3D c11( ( x1y2z2 * (1 - x_d) ) + ( x2y2z2 * x_d ) );

    QVector3D c0( ( c00 * (1 - z_d) ) + ( c10 * z_d ) );
    QVector3D c1( ( c01 * (1 - z_d) ) + ( c11 * z_d ) );

    QVector3D c( c0 * (1 - y_d) + ( c1 * y_d ) );

    return c;
}

int StreamLineMapper::getDataSize()
{
    return flowDataSource->getDataSize();
}
