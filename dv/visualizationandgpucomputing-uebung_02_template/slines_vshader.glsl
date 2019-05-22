#version 140
uniform mat4 mvpMatrix;
in vec4 vertexPosition;

void main(void)
{
    gl_Position = mvpMatrix * vertexPosition;
}
