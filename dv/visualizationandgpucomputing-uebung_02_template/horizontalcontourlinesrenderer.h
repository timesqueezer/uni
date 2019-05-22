#ifndef HORIZONTALCONTOURLINESRENDERER_H
#define HORIZONTALCONTOURLINESRENDERER_H

#include <iostream>

#include <QOpenGLFunctions>
#include <QOpenGLShaderProgram>
#include <QOpenGLBuffer>
#include <QOpenGLTexture>

#include "horizontalslicetocontourlinemapper.h"

class HorizontalContourLinesRenderer : protected QOpenGLFunctions
{
public:
    HorizontalContourLinesRenderer();
    ~HorizontalContourLinesRenderer();

    void setMapper(HorizontalSliceToContourLineMapper*);

    void drawImage(QMatrix4x4 mvpMatrix);
    void init();

    void moveSlice(int steps);

private:
    void initOpenGLShaders();

    HorizontalSliceToContourLineMapper* mapper;

    QOpenGLShaderProgram shaderProgram;
    QOpenGLBuffer vertexBuffer;

    int slice;
    int numVertices;
};

#endif // HORIZONTALCONTOURLINESRENDERER_H
