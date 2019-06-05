#include "opengldisplaywidget.h"

#include <QMouseEvent>
#include "math.h"


OpenGLDisplayWidget::OpenGLDisplayWidget(QWidget *parent)
    : QOpenGLWidget(parent),
      distanceToCamera(-8.0)
{
    setFocusPolicy(Qt::StrongFocus);
}


OpenGLDisplayWidget::~OpenGLDisplayWidget()
{
    // Clean up visualization pipeline.
    delete flowDataSource;
    delete horizontalSliceToImageMapper;
    delete horizontalSliceToContourLineMapper;
    delete streamLineMapper;

    delete bboxRenderer;
    delete horizontalSliceRenderer;
    delete horizontalContourLinesRenderer;
    delete streamLineRenderer;
}


void OpenGLDisplayWidget::initializeGL()
{
    // Qt-specific method to access the OpenGL funtions in this class.
    initializeOpenGLFunctions();

    // Set the backgound color of the OpenGL display.
    glClearColor(0, 0, 0, 1);

    // Enable depth buffer.
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);

    // Our own initialization of the visualization pipeline.
    initVisualizationPipeline();
}


void OpenGLDisplayWidget::resizeGL(int w, int h)
{
    // Calculate aspect ratio of the current viewport.
    float aspectRatio = float(w) / std::max(1, h);

    // Reset projection and set new perspective projection.
    projectionMatrix.setToIdentity();
    projectionMatrix.perspective(45.0, aspectRatio, 0.05, 25.0);

    // Update model-view-projection matrix with new projection.
    updateMVPMatrix();
}


void OpenGLDisplayWidget::paintGL()
{
    // Clear color and depth buffer.
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Call renderer modules.
    bboxRenderer->drawBoundingBox(mvpMatrix);
    horizontalSliceRenderer->drawImage(mvpMatrix);
    horizontalContourLinesRenderer->drawImage(mvpMatrix);
    // streamLineRenderer->drawImage(mvpMatrix);
}


void OpenGLDisplayWidget::mousePressEvent(QMouseEvent *e)
{
    // Save the current position of the mouse pointer for subsequent use
    // in mouseMoveEvent().
    lastMousePosition = QVector2D(e->localPos());
}


void OpenGLDisplayWidget::mouseMoveEvent(QMouseEvent *e)
{
    // If the user holds the left mouse button while moving the mouse, update
    // the rotation angles that specify from which side the grid visualization
    // is viewed.
    if (e->buttons() & Qt::LeftButton)
    {
        // Vector that points from the last stored position of the mouse
        // pointer to the current position.
        QVector2D mousePosDifference = QVector2D(e->localPos()) - lastMousePosition;

        // Update rotation angles in x and y direction. The factor "10" is an
        // arbitrary scaling constant that controls the sensitivity of the
        // mouse.
        rotationAngles.setX(
                    fmod(rotationAngles.x() + mousePosDifference.x()/10.,
                         360.));
        rotationAngles.setY(
                    fmod(rotationAngles.y() + mousePosDifference.y()/10.,
                         360.));

        // Store current position of mouse pointer for next call to this method.
        lastMousePosition = QVector2D(e->localPos());

        // Update model-view-projection matrix with new rotation angles.
        updateMVPMatrix();

        // Redraw OpenGL.
        update();
    }
}


void OpenGLDisplayWidget::wheelEvent(QWheelEvent *e)
{
    // Update distance of the camera to the rendered visualization. The factor
    // "500" is arbitrary and controls that sensitivity of the mouse.
    distanceToCamera += e->delta() / 500.;

    // Update model-view-projection matrix with new distance to camera.
    updateMVPMatrix();

    // Redraw OpenGL.
    update();
}


void OpenGLDisplayWidget::keyPressEvent(QKeyEvent *e)
{
    if (e->key() == Qt::Key_Up)
    {
        horizontalSliceRenderer->moveSlice(4);
        horizontalContourLinesRenderer->moveSlice(4);
        streamLineRenderer->moveSlice(4);
    }
    else if (e->key() == Qt::Key_Down)
    {
        horizontalSliceRenderer->moveSlice(-4);
        horizontalContourLinesRenderer->moveSlice(-4);
        streamLineRenderer->moveSlice(-4);
    }
    else
    {
        return;
    }

    // Redraw OpenGL.
    update();
}


void OpenGLDisplayWidget::updateMVPMatrix()
{
    // Calculate a simple model view transformation from rotation angles
    // and distance to camera.
    // NOTE: Read from bottom to top.

    QMatrix4x4 mvMatrix;
    mvMatrix.translate(0.0, 0.0, distanceToCamera);
    mvMatrix.rotate(rotationAngles.y(), QVector3D(1.0, 0.0, 0.0));
    mvMatrix.rotate(rotationAngles.x(), QVector3D(0.0, 1.0, 0.0));
    mvMatrix.translate(-1.0, -1.0, -1.0);
    mvMatrix.scale(2.0);

    mvpMatrix = projectionMatrix * mvMatrix;
}


void OpenGLDisplayWidget::initVisualizationPipeline()
{
    // Initialize the visualization pipeline:

    // Initialize data source(s).
    flowDataSource = new FlowDataSource();
    flowDataSource->createData(64, 0);

    // Initialize mapper modules.
    horizontalSliceToImageMapper = new HorizontalSliceToImageMapper();
    horizontalSliceToImageMapper->setDataSource(flowDataSource);

    horizontalSliceToContourLineMapper = new HorizontalSliceToContourLineMapper();
    horizontalSliceToContourLineMapper->setDataSource(flowDataSource);

    streamLineMapper = new StreamLineMapper();
    streamLineMapper->setDataSource(flowDataSource);

    // Initialize rendering modules.
    bboxRenderer = new DataVolumeBoundingBoxRenderer();

    horizontalSliceRenderer = new HorizontalSliceRenderer();
    horizontalSliceRenderer->setMapper(horizontalSliceToImageMapper);
    horizontalSliceRenderer->init();

    horizontalContourLinesRenderer = new HorizontalContourLinesRenderer();
    horizontalContourLinesRenderer->setMapper(horizontalSliceToContourLineMapper);
    horizontalContourLinesRenderer->init();

    streamLineRenderer = new StreamLineRenderer();
    streamLineRenderer->setMapper(streamLineMapper);
    streamLineRenderer->init();

    // flowDataSource->printValuesOfHorizontalSlice(128);
}
