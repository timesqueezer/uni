#ifndef HORIZONTALSLICERENDERER_H
#define HORIZONTALSLICERENDERER_H

#include <iostream>

#include <QOpenGLFunctions>
#include <QOpenGLShaderProgram>
#include <QOpenGLBuffer>
#include <QOpenGLTexture>

#include "horizontalslicetoimagemapper.h"

class HorizontalSliceRenderer : protected QOpenGLFunctions
{
public:
    HorizontalSliceRenderer();
    ~HorizontalSliceRenderer();

    void setMapper(HorizontalSliceToImageMapper*);

    void drawImage(QMatrix4x4 mvpMatrix);
    void init();

    void moveSlice(int steps);

private:
    void initOpenGLShaders();

    HorizontalSliceToImageMapper* mapper;

    QOpenGLShaderProgram shaderProgram;
    QOpenGLBuffer vertexBuffer;
    QOpenGLTexture* texture;

    int slice;
};

#endif // HORIZONTALSLICERENDERER_H
