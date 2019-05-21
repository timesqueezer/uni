#version 140
uniform mat4 mvpMatrix;
in vec4 vertexPosition;

smooth out vec4 texCoord;

void main(void)
{
    gl_Position = mvpMatrix * vertexPosition;
    texCoord = vertexPosition;
}
