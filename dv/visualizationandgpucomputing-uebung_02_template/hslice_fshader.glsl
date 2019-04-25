#version 400

uniform sampler2D texture;
smooth in vec2 texCoord;

void main()
{
    gl_FragColor = vec4(0, 1, 0, 1); //texture2D(texture, texCoord);
}
