#ifndef DATAVOLUMEBOUNDINGBOXRENDERER_H
#define DATAVOLUMEBOUNDINGBOXRENDERER_H

#include <QOpenGLFunctions>
#include <QOpenGLShaderProgram>
#include <QOpenGLBuffer>


class DataVolumeBoundingBoxRenderer : protected QOpenGLFunctions
{
public:
    DataVolumeBoundingBoxRenderer();
    virtual ~DataVolumeBoundingBoxRenderer();

    // Draw the bounding box to the current OpenGL viewport.
    void drawBoundingBox(QMatrix4x4 mvpMatrix);

private:
    void initOpenGLShaders();
    void initBoundingBoxGeometry();

    QOpenGLShaderProgram shaderProgram;
    QOpenGLBuffer vertexBuffer;
};

#endif // DATAVOLUMEBOUNDINGBOXRENDERER_H
