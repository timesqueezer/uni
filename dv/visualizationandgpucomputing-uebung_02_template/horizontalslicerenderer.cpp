#include "horizontalslicerenderer.h"

HorizontalSliceRenderer::HorizontalSliceRenderer()
{
    texture = new QOpenGLTexture(QOpenGLTexture::Target2D);

    initializeOpenGLFunctions();
    initOpenGLShaders();
}

HorizontalSliceRenderer::~HorizontalSliceRenderer() {
    vertexBuffer.destroy();

    texture->destroy();

    delete texture;
}

void HorizontalSliceRenderer::setMapper(HorizontalSliceToImageMapper* mapper) {
    this->mapper = mapper;
}

void HorizontalSliceRenderer::drawImage(QMatrix4x4 mvpMatrix)
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
    glLineWidth(5);
    glDrawArrays(GL_LINE_STRIP, 0, 4);

    const int textureUnit = 0; // select a texture unit
    texture->bind(textureUnit);
    shaderProgram.setUniformValue("texture", textureUnit);
}


void HorizontalSliceRenderer::initOpenGLShaders()
{
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Vertex, "hslice_vshader.glsl");
    shaderProgram.addShaderFromSourceFile(QOpenGLShader::Fragment, "hslice_fshader.glsl");
    shaderProgram.link();
}


void HorizontalSliceRenderer::init()
{
    // Vertices of a unit cube that represents the bounding box.
    const unsigned int numVertices = 4;
    float unitCubeVertices[numVertices][3] = {
        {0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0},
    };

    // Create vertex buffer and upload vertex data to buffer.
    vertexBuffer.create(); // make sure to destroy in destructor!
    vertexBuffer.bind();
    vertexBuffer.allocate(unitCubeVertices, numVertices * 3 * sizeof(float));

    QImage img = mapper->mapSliceToImage(0);

    texture->create();
    texture->setWrapMode(QOpenGLTexture::ClampToEdge);
    texture->setData(img);

}
