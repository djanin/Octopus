#version 410
layout (location = 0) in vec3 skypos;
out vec3 texCoord;

uniform mat4 mvp;

vec3 pushVector (vec3 v) {
  float n = max (abs(v.x),max (abs(v.y),abs(v.z)));
  return vec3(1/n)*v;
  }


void main() {
gl_Position = (mvp*vec4(skypos,1)).xyww;
texCoord = skypos;
}