#include "horizontalcontourlinesrenderer.h"

HorizontalContourLinesRenderer::HorizontalContourLinesRenderer()
{
    texture = new QOpenGLTexture(QOpenGLTexture::Target2D);

    initializeOpenGLFunctions();
    initOpenGLShaders();

    slice = 64;
}

HorizontalContourLinesRenderer::~HorizontalContourLinesRenderer() {
    vertexBuffer.destroy();

    texture->destroy();

    delete texture;
}

void HorizontalContourLinesRenderer::setMapper(HorizontalSliceToContourLineMapper* mapper) {
    this->mapper = mapper;
}

void HorizontalContourLinesRenderer::drawImage(QMatrix4x4 mvpMatrix)
{
    // Tell OpenGL to use the shader program of this class.
    shaderProgram.bind();

    // Bind and enable the vertex buffer.
    vertexBuffer.bind();
    shaderProgram.setAttributeBuffer("vertexPosition", GL_FLOAT, 0, 3, 3*sizeof(float));
    shaderProgram.enableAttributeArray("vertexPosition");

    // Set the model-view-projection matrix as a uniform value.
    shaderProgram.setUniformValue("mvpMatrix", mvpMatrix);

    // Issue OpenGL draw commands.
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glLineWidth(4);
    glDrawArrays(GL_LINES, 0, numVertices);
}


void HorizontalContourLinesRenderer::initOpenGLShaders()
{
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Vertex, "clines_vshader.glsl");
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Fragment, "clines_fshader.glsl");
    shaderProgram.link();
}


void HorizontalContourLinesRenderer::init()
{
    QVector<QVector3D> list = mapper->mapSliceToContourLineSegments(slice);
    numVertices = list.length();

    std::cout << "numVertices: " << numVertices << std::endl;

    float* bufferData = new float[numVertices*3];
    for (int i = 0; i < numVertices; i++) {
        bufferData[i] = list.data()[i].x();
        bufferData[i+1] = list.data()[i].y();
        bufferData[i+2] = list.data()[i].z();

        std::cout << i << ": " << list.data()[i].x() << ", " << list.data()[i].y() << ", " << list.data()[i].z() << std::endl;
    }

    // Create vertex buffer and upload vertex data to buffer.
    vertexBuffer.create(); // make sure to destroy in destructor!
    vertexBuffer.bind();
    vertexBuffer.allocate(bufferData, numVertices * 3 * sizeof(float));
}

void HorizontalContourLinesRenderer::moveSlice(int steps)
{
}
