#version 400

uniform mat4 mvpMatrix;
in vec4 vertexPosition;

smooth out vec2 texCoord;

void main(void)
{
    gl_Position = mvpMatrix * vertexPosition;
    // qt_TexCoord0 = qt_MultiTexCoord0;
}
