#include "horizontalslicetoimagemapper.h"

HorizontalSliceToImageMapper::HorizontalSliceToImageMapper()
{

}

void HorizontalSliceToImageMapper::setDataSource(FlowDataSource* source) {
    flowDataSource = source;
}

QImage HorizontalSliceToImageMapper::mapSliceToImage(int iz) {
    int dataSize = flowDataSource->getDataSize();
    QImage img(dataSize, dataSize, QImage::Format_RGB32);

    for (int iy = 0; iy < dataSize; ++iy) {
        for (int ix = 0; ix < dataSize; ++ix) {
            int v = (int) flowDataSource->getDataValue(ix, iy, iz, 0);
            v *= 3 * 255;
            QColor c;
            if (v >= 0) {
                c.setRed(v);
            } else {
                c.setBlue(-v);
            }

            img.setPixelColor(ix, iy, c);
        }
    }

    return img;
}
