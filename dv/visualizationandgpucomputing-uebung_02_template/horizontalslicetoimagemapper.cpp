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
            float v = flowDataSource->getDataValue(iz, iy, ix, 0);
            v *= 3 * 255;
            int v_int = static_cast<int>(v);
            QColor c;
            if (v_int >= 0) {
                c.setRed(v_int);
            } else {
                c.setBlue(-v_int);
            }

            img.setPixelColor(ix, iy, c);
        }
    }

    return img;
}

int HorizontalSliceToImageMapper::getDataSize()
{
    return flowDataSource->getDataSize();
}
