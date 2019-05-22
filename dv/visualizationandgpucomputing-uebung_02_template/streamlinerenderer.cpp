#include "streamlinerenderer.h"

StreamLineRenderer::StreamLineRenderer()
{
    initializeOpenGLFunctions();
    initOpenGLShaders();

    slice = 4;
}

StreamLineRenderer::~StreamLineRenderer() {
    vertexBuffer.destroy();
}

void StreamLineRenderer::setMapper(StreamLineMapper* mapper) {
    this->mapper = mapper;
}

void StreamLineRenderer::drawImage(QMatrix4x4 mvpMatrix)
{
    shaderProgram.bind();

    vertexBuffer.bind();
    shaderProgram.setAttributeBuffer("vertexPosition", GL_FLOAT, 0, 3, 3*sizeof(float));
    shaderProgram.enableAttributeArray("vertexPosition");

    shaderProgram.setUniformValue("mvpMatrix", mvpMatrix);

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glLineWidth(1);
    glDrawArrays(GL_LINES, 0, numVertices);
}


void StreamLineRenderer::initOpenGLShaders()
{
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Vertex, "slines_vshader.glsl");
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Fragment, "slines_fshader.glsl");
    shaderProgram.link();
}


void StreamLineRenderer::init()
{
    QVector<QVector3D> list = mapper->mapToStreamLines(slice);
    numVertices = list.length();

    std::cout << "numVertices: " << numVertices << std::endl;

    // Create vertex buffer and upload vertex data to buffer.
    vertexBuffer.create(); // make sure to destroy in destructor!
    vertexBuffer.bind();
    vertexBuffer.allocate(list.data(), numVertices * sizeof(QVector3D));
}

void StreamLineRenderer::moveSlice(int steps)
{
    slice += steps;
    int dataSize = mapper->getDataSize();

    if (slice < 0) {
        slice = 0;
    } else if (slice >= dataSize) {
        slice = dataSize - 1;
    }

    init();
}
