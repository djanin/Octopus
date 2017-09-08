#version 330 core
in vec4 f_color;
in vec3 f_normal;
in vec3 f_vertex;
in vec4 f_position;

out vec4 outColor;

uniform sampler2D myTexture;
uniform samplerCube cubemap;

float shine = 1;

vec4 brighter (vec4 c) {

     return vec4(sqrt(c.r),sqrt(c.g),sqrt(c.b),c.a);
     
}
vec4 getColor (vec3 n, vec4 color) {
  float inc = dot (n,normalize(vec3(3,0.7,3)));
  float e = pow ((1+shine+inc)/(2+shine), 2);
  if (e <= 0) {return vec4 (0,0,0,0);};
  return vec4(e*color[0],e*color[1],e*color[2],color[3]);
}

vec3 pushVector (vec3 v) {
  float n = max (abs(v.x),max (abs(v.y),abs(v.z)));
  return vec3(1/n)*v;
  }

void main(void) {
  vec4 text_color =
       texture(cubemap,pushVector(reflect(normalize(f_vertex),f_normal)));
       					//	pushVector(f_normal)));
  outColor = // vec4(1, 0, 0, 1);
     // f_color;
    // vec4(f_normal,1);
    // text_color;
    getColor(f_normal,vec4(vec3(0.4)*f_color.rgb,0)+ text_color);
    // getColor(f_normal,f_color);
  
}

