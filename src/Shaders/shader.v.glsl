#version 330 core

// vertex shader

#define MAX_RES 35


// pen position
layout (location = 0) in vec3 m1;
layout (location = 1) in vec3 m2;
layout (location = 2) in vec3 m3;
layout (location = 3) in vec3 pen;

// pen shape
layout (location = 4) in vec4 info;
layout (location = 5) in vec4 c1;
layout (location = 6) in vec4 c2;

			// various constant
#define PI 3.1415926535897932384626433832795
#define EMPTY_PEN 0
#define CYCLE_PEN 1
#define POLYGON_PEN 2
#define GEAR_PEN 3
			// end pen shape types
 
out mat3 geom_mat;
out mat3 geom_inv_mat;
//out vec3 geom_m2;
//out vec3 geom_m3;
out vec3 geom_pen;

out int geom_shape;
out int geom_nb_vertex;
out float geom_angle;
out float geom_radius;
out vec4 geom_color1;
out vec4 geom_color2;

out  float geom_extreme; // length of the longest "radius" of the induced "cube"
out int geom_res;

out vec4 f_color;
out int geom_cull;

uniform mat4 mvp;
uniform sampler2DRect myTexture;

mat4 i_mvp = inverse(mvp);

float exRadius();

float resolution();
int cull_object();

void main(void) {
  // copy cat to the geometry shader
  geom_pen = pen;
  geom_mat = mat3(m1,m2,m3);
  geom_inv_mat = inverse(geom_mat);
  // copy cat to the geometry shader
  geom_shape= int(info[0]);
  // if (geom_shape <= CYCLE_PEN) geom_nb_vertex= MAX_RES;
  // else
  geom_nb_vertex= int(info[1]);
				
  geom_radius= info[2];
  geom_angle= info[3];
  
  geom_color1 = c1;
  geom_color2 = c1;
  geom_cull = cull_object();


  geom_extreme = exRadius();
  geom_res = //MAX_RES;
  	   int(resolution());
  
  // only used when bypassing the geometry shader
  gl_Position =  mvp * vec4(pen, 1.0);
  f_color = vec4 (1,1,1,1); // normalize(pen);

}



#define RES_FACTOR 500 // empirically set


float resolution()
// resolution factor depending on visible radius TODO : could be thought over again
{
  int nb=geom_nb_vertex;
  float view_angle = abs(atan(geom_extreme,abs(pen.z))); // solid angle in the view model
  if (geom_shape == GEAR_PEN) nb = 12*geom_nb_vertex;
  if (geom_shape == POLYGON_PEN) nb = geom_nb_vertex;
  float res = max(3,min(min(nb,MAX_RES),MAX_RES*RES_FACTOR*pow(sin(view_angle),1)));
  return res;
}


vec2 extreme_point(vec3 v)
// vector in view model of the extreme point in direction v of the eptahedron induced by position "sphere"
{
  vec4 v1 =mvp*vec4(geom_pen + geom_mat*(vec3(geom_radius)*v),1);
  vec4 v2 = mvp*vec4(geom_pen,1);  
  return ((v1/v1.w - v2/v2.w).xy);
}


float exRadius()
{
  return max(max(length(extreme_point(vec3(0,0,1))),
	     length(extreme_point(vec3(0,0,-1)))),
	 max(max(length(extreme_point(vec3(1,0,0))),
		 length(extreme_point(vec3(-1,0,0)))),
	     max(length(extreme_point(vec3(0,1,0))),
		 length(extreme_point(vec3(0,-1,0))))));
}


int cull_object() {
  // radius around pen position in view model
  float exr = exRadius();
 // pen position in view model
  vec4 penv = mvp*vec4(geom_pen,1);
  float m = length(vec3(0,0,0.5)
		   - (penv.xyz/penv.z)) - exr;
  return int(m > 2);
}

