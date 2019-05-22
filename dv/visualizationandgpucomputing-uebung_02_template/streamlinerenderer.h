#ifndef STREAMLINERENDERER_H
#define STREAMLINERENDERER_H

#include <iostream>

#include <QOpenGLFunctions>
#include <QOpenGLShaderProgram>
#include <QOpenGLBuffer>
#include <QOpenGLTexture>

#include "streamlinemapper.h"

class StreamLineRenderer : protected QOpenGLFunctions
{
public:
    StreamLineRenderer();
    ~StreamLineRenderer();

    void setMapper(StreamLineMapper*);

    void drawImage(QMatrix4x4 mvpMatrix);
    void init();

    void moveSlice(int steps);

private:
    void initOpenGLShaders();

    StreamLineMapper* mapper;

    QOpenGLShaderProgram shaderProgram;
    QOpenGLBuffer vertexBuffer;

    int slice;
    int numVertices;
};

#endif // STREAMLINERENDERER_H
