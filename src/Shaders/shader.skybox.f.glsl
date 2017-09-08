#version 330
in vec3 texCoord;
out vec4 outColor;
uniform samplerCube skymap;

vec3 pushVector (vec3 v) {
  float n = max (abs(v.x),max (abs(v.y),abs(v.z)));
  return vec3(1/n)*v;
  }

void main() {
    outColor =   // vec4 (1,1,1,1);
       texture(skymap,(texCoord));
}