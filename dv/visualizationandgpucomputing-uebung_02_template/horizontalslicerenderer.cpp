#include "horizontalslicerenderer.h"

HorizontalSliceRenderer::HorizontalSliceRenderer()
{
    texture = new QOpenGLTexture(QOpenGLTexture::Target2D);

    initializeOpenGLFunctions();
    initOpenGLShaders();

    slice = 4;
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

    shaderProgram.setAttributeBuffer("texCoord", GL_FLOAT, 0, 3, 3*sizeof(float));
    shaderProgram.enableAttributeArray("texCoord");

    // Set the model-view-projection matrix as a uniform value.
    shaderProgram.setUniformValue("mvpMatrix", mvpMatrix);

    // Issue OpenGL draw commands.
    // glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    // glLineWidth(5);
    // glDrawArrays(GL_LINE_STRIP, 0, 16);

    const int textureUnit = 0; // select a texture unit
    texture->bind(textureUnit);
    shaderProgram.setUniformValue("texture", textureUnit);

    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 6);

    // shaderProgram.bindAttributeLocation("texCoord", 1);
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
    const unsigned int numVertices = 6;
    float unitCubeVertices[numVertices][3] = {
        {0, 0, 1}, {1, 0, 1}, {0, 1, 1}, {0, 1, 1},
        {1, 1, 1}, {1, 0, 1}
    };

    // Create vertex buffer and upload vertex data to buffer.
    vertexBuffer.create(); // make sure to destroy in destructor!
    vertexBuffer.bind();
    vertexBuffer.allocate(unitCubeVertices, numVertices * 3 * sizeof(float));

    QImage img = mapper->mapSliceToImage(slice);

    img.save("test.png", "PNG");

    texture->create();
    texture->setWrapMode(QOpenGLTexture::ClampToEdge);
    texture->setData(img);

}

void HorizontalSliceRenderer::moveSlice(int steps)
{
    slice += steps;
    int dataSize = mapper->getDataSize();

    if (slice < 0) {
        slice = 0;
    } else if (slice >= dataSize) {
        slice = dataSize - 1;
    }

    float zPos = 1 - (static_cast<float>(slice) / static_cast<float>(dataSize));

    std::cout << "Slice: " << slice << "zPos:" << zPos << std::endl;

    const unsigned int numVertices = 6;
    float unitCubeVertices[numVertices][3] = {
        {0, 0, zPos}, {1, 0, zPos}, {0, 1, zPos}, {0, 1, zPos},
        {1, 1, zPos}, {1, 0, zPos}
    };

    vertexBuffer.bind();
    vertexBuffer.allocate(unitCubeVertices, numVertices * 3 * sizeof(float));

    texture->destroy();

    QImage img = mapper->mapSliceToImage(slice);
    texture->create();
    texture->setWrapMode(QOpenGLTexture::ClampToEdge);
    texture->setData(img);
}
