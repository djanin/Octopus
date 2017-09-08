#version 410 core
// geometry shader (drawing extrusion)
layout(lines_adjacency) in;
			layout (triangle_strip, max_vertices = 256) out; 
			// layout (line_strip, max_vertices = 256) out; 

			// geometry shader inputs
in mat3 geom_mat[];
in mat3 geom_inv_mat[];
in vec3 geom_pen[];
			
in int geom_shape[];
in int geom_nb_vertex[];
in float geom_angle[];
in float geom_radius[];
in vec4 geom_color1[];
in vec4 geom_color2[];
// in vec4 geom_extreme[]; // underdebug
in float geom_res[];

uniform mat4 mvp;
			// model matrices are given by geom_mat[]			


			// geometry shader outputs			
out vec4 f_color;
out vec3 f_normal;
out vec3 f_vertex;
			
			// Various macros
			
#define MAX_RES 120
			// maximum number of vertex per position cycle

#define ALMOST_ZERO 0.00001
			
			// begin pen shape types
#define EMPTY_PEN 0
#define CYCLE_PEN 1
#define POLYGON_PEN 2
#define GEAR_PEN 3
			// end pen shape types
#define PI 3.1415926535897932384626433832795
			
			
			// Prototypes for the main function
vec3 vertex(int n, int i, int res);
			
vec3 normal(int n,int i,int res, vec3 sn);

vec3 spanNormal1();
vec3 spanNormal2();

			
/*******************************************************************/

			/*
# define SHIFT 1 // the bigger the more closed is the computed view

			// TODO : should be redone: any position "sphere" that has
			// a visible point should be computed (hence with window size as arguement)
			

vec4 getExtreme(int i, vec3 v)
{
  return (mvp*vec4(geom_pen[i] + geom_mat[i]
		   *(vec3(geom_radius[i])*(geom_inv_mat[i] * v)),1));
}
			*/
			
#define SIZE_X 40
#define SIZE_Y 40
bool cutOut (int i)
{
 return false;

/*
  return (geom_extreme[i].x < - SIZE_X) || // upmost point out of screen
    (geom_extreme[i].y > SIZE_X) || // lowest point out of screen 
    (geom_extreme[i].z < - SIZE_Y) || // rightmost point out of screen
    (geom_extreme[i].w > SIZE_Y); //leftmost point out of screen
*/
}

  
  /* // possible code for cutout
  vec4 extreme = getExtreme(i,vec3(0,0,-1));
  //mvp*vec4(geom_pen[i] + geom_mat[i]
  //			   *(vec3(geom_radius[i])*(geom_inv_mat[i] * )),1);
  return (dot(extreme,vec4(-SHIFT,0,1,1)) < 0) ||
    (dot(extreme,vec4(SHIFT,0,1,1)) < 0) ||
    (dot(extreme,vec4(0,-SHIFT,1,1)) < 0) ||
    (dot(extreme,vec4(0,SHIFT,1,1)) < 0);
  */

#define RES_FACTOR 0.02

float face_radius(int i)
{
  return //geom_nb_vertex[i]
    MAX_RES*(mvp*vec4(
				     //geom_pen[i]+ geom_mat[i]*(vec3(geom_radius[i])*(geom_inv_mat[i] * vec3(1,0,0)))
				     geom_radius[i],geom_radius[i],abs(geom_pen[i].z)
								       ,1)).x*RES_FACTOR;
}



int resolution()
{
  return // max(3, min(MAX_RES,int(max(geom_res[1],geom_res[2]))));
  max(3, min(MAX_RES,int(max(face_radius(1),face_radius(2)))));
}


int div(int n, int q)
{
  return int(floor(float(n*10000)/float(q*10000)));
}

// Various global values that only depends on position 1 and 2
int res = max(3, min(MAX_RES,int(max(face_radius(1),face_radius(2))))); //resolution();
int nb = min(res,max(geom_nb_vertex[1],geom_nb_vertex[2]));



void main()
{
  // out of sight pairs of positions generate no vertices
  if (cutOut(1) && cutOut(2))  return;
  // under debug
  
  if ((geom_shape[1] != EMPTY_PEN) && (geom_shape[2] != EMPTY_PEN))
      
    // no triangulation otherwise
    {
      // resolution is smaller (from 3 to MAX_RES) when far
      // from the eye...
      

      //int nb = min(res,max(geom_nb_vertex[1],geom_nb_vertex[2]));

      // Vertex production
      for (int i=0; i<=res; i++) {
	// Vertex 1
	f_vertex =  // b_vertex (1, nb, i,res);
	   vertex(1,i,res);
	f_normal = // b_normal(1,nb,i,res,spanNormal1());
	   normal(1,i,res,spanNormal1());
	f_color =  geom_color1[1];
	gl_Position = mvp * (vec4(f_vertex,1));
	EmitVertex();
	
	// Vertex 2
	f_vertex = // b_vertex (2, nb, i,res);
	   vertex(2,i,res);
	f_normal = // b_normal(2,nb,i,res,spanNormal2());
	   normal(2,i,res,spanNormal2());
	f_color =  geom_color1[2];
	gl_Position = mvp *  (vec4(f_vertex,1));
	EmitVertex();
      }
      EndPrimitive();
    }
}

/********************* Auxilliary function **************************************/


vec3 cycle_vertex(int n, int i, int res);
vec3 poly_vertex(int n, int i, int res);			
vec3 gear_vertex(int n, int i, int res);
vec3 computeNormal4(vec3 x, vec3 x1, vec3 x2, vec3 x3, vec3 x4);
vec3 computeNormal3 (vec3 v,vec3 v1,vec3 v2,vec3 v3);


// Vertex and normal computation per position and index

vec3 vertex(int n, int i, int res)
// gets the ith vertex around position n with resoluttion res
{
  if (geom_shape[n] <= CYCLE_PEN)
    return cycle_vertex(n,i,res);
  else  // if (geom_shape[n] == POLYGON_PEN)
    return  poly_vertex(n,i,res);
  // else return gear_vertex(n,i,res);
  // Remark : bad handling og switch in (my) glsl
}

vec3 normal(int n,int i,int res, vec3 sn)
// gets the normal of the ith vertex around position n = 1 or 2
{
  vec3 result;
  if (n == 1)
    // vertex 1
    {
      if (geom_radius[1] == 0)
	// span point
	result = sn;
      else
	// latteral point
	return computeNormal4(vertex(1,i,res), // vertex 1
				vertex(0,i,res),vertex(1,i+1,res), // neigboorhood
				vertex(2,i,res),vertex(1,i-1,res));// neigboorhood
    }
  else
    // vertex 2
    {
      if (geom_radius[2] == 0)
	// span point
	result = sn;
      else
	// latteral point
	return computeNormal4(vertex(2,i,res), // vertex 2
				vertex(1,i,res),vertex(2,i+1,res),// neigboorhood
				vertex(3,i,res),vertex(2,i-1,res));// neigboorhood
    }
  return result;
}
  
  

vec3 cycle_vertex(int n, int i, int res)
// gets the ith vertex on a cycle arc 
{
  int resn=int(geom_res[n]);
  float a0 = radians(geom_angle[n])*i/resn;
  return geom_pen[n] + geom_mat[n]
    *vec3(geom_radius[n]*cos (a0),geom_radius[n]*sin (a0),0);
}

vec3 poly_vertex(int n, int i, int res)
// gets the ith vertex on a regular polygon arc
{
  //int nb = min(int(geom_res[n]),geom_nb_vertex[n]);
  int i_ref = div(div (i*nb,res)*res,nb);
  if (i==res) i_ref=res;
  float a = radians(geom_angle[n])*i/res;
  float d0 = radians(geom_angle[n]/nb/2);  
  float d = d0 - (radians(geom_angle[n])*(i-i_ref))/res;
  float r = min(geom_radius[n],geom_radius[n]*cos(d0)/cos(d)); // for better stability ?
  if  (geom_shape[n] <= CYCLE_PEN)
    return geom_pen[n] + geom_mat[n]
    *vec3(geom_radius[n]*cos (a),geom_radius[n]*sin (a),0);
  else 
    return geom_pen[n] + geom_mat[n]
      *vec3(r*cos (a),
	    r*sin (a),0);
}

/*
vec3 poly_vertex0(int n, int i, int res)
// gets the ith vertex on a regular polygon arc 
{
  float ratio, i0, i1, w0,w1,a0,a1;
  ratio = floor((i*geom_nb_vertex[n])/res);
  i0 = (ratio*res)/geom_nb_vertex[n];
  i1 = ((ratio+1)*res)/geom_nb_vertex[n];
  w1 = (i*geom_nb_vertex[n])/res-ratio;
  w0 = (1-w1);
  a0 = radians(geom_angle[n])*i0/res;
  a1 = radians(geom_angle[n])*i1/res;
  return geom_pen[n] + geom_mat[n]*
    vec3(geom_radius[n]*(w0*cos (a0)+w1*cos(a1)),
	 geom_radius[n]*(w0*sin (a0)+w1*sin(a1)),0);
}

vec3 gear_vertex(int n, int i, int res)
// gets the ith vertex on a gear 
{
  float ratio = floor((2*i*geom_nb_vertex[n])/res);
  float tsize = PI*geom_radius[n]/geom_nb_vertex[n]/2;// half tooth size
  float a0 = radians(geom_angle[n])*i/res;
  if (mod(ratio,2) == 0)
    return geom_pen[n] + geom_mat[n]
      *vec3((geom_radius[n]+0.9*tsize)*cos (a0),(geom_radius[n]+0.9*tsize)*sin (a0),0);
  else
    return geom_pen[n] + geom_mat[n]
      *vec3((geom_radius[n]-1.1*tsize)*cos (a0),(geom_radius[n]-1.1*tsize)*sin (a0),0); 
}

*/

vec3 inverse (vec3 v)
// a bit of inverse geometry (for normals)
{
  float q = dot(v,v);
  if (q == 0) return vec3(0);
  else return (vec3 (v.x/q, v.y/q,v.z/q));
}

vec3 computeNormal4(vec3 x, vec3 x1, vec3 x2, vec3 x3, vec3 x4)
// normal computation "as if" x1, x2, x3, x4 goes around x on the same sphere
{
  return normalize // normalization could postponed to fragment shader
    (inverse(cross (x1 -x, x2 -x)) +
		   inverse(cross (x2 -x, x3 -x)) +
		   inverse(cross (x3 -x, x4 -x)) +
		   inverse(cross (x4 -x, x1 -x)));
}




vec3 computeNormal3(vec3 v,vec3 v1,vec3 v2,vec3 v3)
  // normal computation "as if" v1, v2, v3 go around v on the same sphere
{
  return normalize
		   (inverse(cross (v1 -v, v2 -v)) +
		    inverse(cross (v2 -v, v3 -v)) +
		    inverse(cross (v3 -v, v1 -v))
		    );		   
  
}

vec3 spanNormal1()
{
  vec3 v0 = geom_pen[1];
  vec3 v1 = cycle_vertex(2,2,3);
  vec3 v2 = cycle_vertex(2,1,3);
  vec3 v3 = cycle_vertex(2,0,3);
  return computeNormal3 (v0,v1,v2,v3);
}

vec3 spanNormal2()
{
  vec3 v0 = geom_pen[2];
  vec3 v1 = cycle_vertex(1,0,3);
  vec3 v2 = cycle_vertex(1,1,3);
  vec3 v3 = cycle_vertex(1,2,3);
  return computeNormal3 (v0,v1,v2,v3);
}

