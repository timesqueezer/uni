#version 140
uniform sampler2D texture;
smooth in vec4 texCoord;

void main()
{
    // gl_FragColor = vec4(0, 1, 0, 1);
    gl_FragColor = texture2D(texture, texCoord.st);
}
