#version 330 core
// geometry shader (drawing extrusion)

layout(lines_adjacency) in;

layout (triangle_strip, max_vertices =  72) out;
// layout (line_strip, max_vertices = 72) out; 

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
in int geom_res[];
in int geom_cull[];
			// model view projection matrix
uniform mat4 mvp;
uniform sampler2DRect myTexture;

			// geometry shader outputs			
out vec4 f_color;
out vec3 f_normal;
out vec3 f_vertex;
out vec4 f_position;
			// various constants
#define PI 3.1415926535897932384626433832795
#define EMPTY_PEN 0
#define CYCLE_PEN 1
#define POLYGON_PEN 2
#define GEAR_PEN 3
			// Prototypes for the main function
vec3 vertex(int n, int i, int res);
vec3 normal1(int i,int j);
vec3 normal2(int i,int j);
vec3 normalp1(int f, int i,int j);
vec3 normalp2(int f, int i,int j);
vec4 color (vec3 n, vec4 color);
bool cull_segment();

bool cull_segment()
{
	mat4 imvp = inverse(mvp);
	return false;
}

void main()
{
  if ((geom_shape[1] != EMPTY_PEN) && (geom_shape[2] != EMPTY_PEN)
  && ((geom_cull[1] !=1) || (geom_cull[2]!=1)) )
    // no triangulation otherwise
    {
      // Pen 1 and 2 indices
      int i1 = 0;
      int i2 = 0;
      int r = max(geom_res[1],geom_res[2]);
      for (int i=0; i<= r; i++)
      {
	// Vertices computation
	vec3 v1 = vertex(1,i1,geom_res[1]);
	vec3 v2 = vertex(2,i2,geom_res[2]);
	vec3 n1 = normal1(i1,i2);
	vec3 n2 = normal2(i2,i1);
	if (((geom_shape[1] != POLYGON_PEN) || (geom_radius[1] == 0))
	    && ((geom_shape[2] != POLYGON_PEN) || (geom_radius[2] == 0)))
	  // non polygon extrusion with smooth edge
	  {
	    // Vertices emission common to both face
	    f_vertex = v1;
	    f_normal = n1;
	    f_color = geom_color1[1];
	    f_position =  mvp * (vec4(v1,1));
	    gl_Position = f_position;
	    EmitVertex();
	    
	    f_vertex = v2;
	    f_normal = n2;
	    f_color = geom_color1[2] ;
	    f_position = mvp *  (vec4(v2,1));
	    gl_Position = f_position;
	    EmitVertex();
	  }
	  else
	  // polygon extrusion necessitates duplications of points with distinct normals for sharp edge
	  {
	    // Vertices emission previous face
	    f_vertex = v1;
	    if (geom_shape[1] != POLYGON_PEN)
	      f_normal = n1;
	    else f_normal = normalp1(0,i1,i2);
	    f_color = geom_color1[1];
	    f_position =  mvp * (vec4(v1,1));
	    gl_Position = f_position;
	    EmitVertex();
	    
	    f_vertex = v2;
	    if (geom_shape[2] != POLYGON_PEN)
	      f_normal = n2;
	    else f_normal = normalp2(0,i2,i1);
	    f_color = geom_color1[2] ;
	    f_position = mvp *  (vec4(v2,1));
	    gl_Position = f_position;
	    EmitVertex();
	    
	    // Vertices emission next face
	    f_vertex = v1;
	    if (geom_shape[1] != POLYGON_PEN)
	      f_normal = n1;
	    else f_normal = normalp1(1,i1,i2);
	    f_color = geom_color1[1];
	    f_position =  mvp * (vec4(v1,1));
	    gl_Position = f_position;
	    EmitVertex();
	    
	    f_vertex = v2;
	    if (geom_shape[2] != POLYGON_PEN)
	      f_normal = n2;
	    else f_normal = normalp2(1,i2,i1);
	    f_color = geom_color1[2] ;
	    f_position = mvp *  (vec4(v2,1));
	    gl_Position = f_position;
	    EmitVertex();
	  }
	// indices increase
	if (i1*r <= i*geom_res[1])
	  i1 = i1 + 1;
	if (i2*r <= i*geom_res[2])
	  i2 = i2 + 1;
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
vec3 computeNormal2 (vec3 v,vec3 v1,vec3 v2);


// Vertex and normal computation per position and index

vec3 vertex(int n, int i, int res)
// gets the ith vertex on a cycle arc 
{

  float a0 = radians(geom_angle[n])*i/res;
  float radius_mod=1;
  if (geom_shape[n] == POLYGON_PEN) {
    int i_ref = int((floor((i*geom_nb_vertex[n])/res))*res/geom_nb_vertex[n]);
    // index of the previous corner of the polygon
    float d0 = radians(geom_angle[n]/geom_nb_vertex[n]/2);
    // half angle between two corners
    float d = d0 - (radians(geom_angle[n])*(i-i_ref))/res;
    // angle between previous corner and current point
    radius_mod = min(1,abs(cos(d0)/cos(d)));
  }
  if (geom_shape[n] == GEAR_PEN)
    {
    float tsize = radians(geom_angle[n])/geom_nb_vertex[n]/4;
    radius_mod = 1+tsize*cos(radians(360*float(i)*geom_nb_vertex[n]/res));
    }
  return geom_pen[n] + geom_mat[n]
    *vec3(radius_mod*geom_radius[n]*cos (a0),radius_mod*geom_radius[n]*sin (a0),0);
}


vec3 normal1(int i, int j)
// gets the ith vertex normal on first position
{
  vec3 vn;
  if (geom_radius[1] != 0)
    vn=computeNormal4(vertex(1,i,geom_res[1]),
		      vertex(0,i,geom_res[1]),
		      vertex(1,i+1,geom_res[1]),
		      vertex(2,i,geom_res[1]),
		      vertex(1,i-1,geom_res[1]));
  else if (geom_shape[1] > 1)
    vn = computeNormal3 (vertex(1,j,geom_res[2]),
			 vertex(2,j+1,geom_res[2]),
			 vertex(2,j,geom_res[2]),
			 vertex(2,j-1,geom_res[2]));
  else vn = computeNormal4(vertex(1,0,1),
			   cycle_vertex(2,3,4),
			   cycle_vertex(2,2,4),
			   cycle_vertex(2,1,4),
			   cycle_vertex(2,0,4));
    
  return vn;
}

vec3 normalp1(int f, int i, int j)
// gets the ith vertex normal on first position when polygon
{
  vec3 vn;
  if (geom_radius[1] != 0)
    {
      if (f==0) 
	vn=computeNormal3(vertex(1,i,geom_res[1]),
			  vertex(2,i,geom_res[1]),
			  vertex(1,i-1,geom_res[1]),
			  vertex(0,i,geom_res[1]));
      else
	vn=computeNormal3(vertex(1,i,geom_res[1]),
			  vertex(0,i,geom_res[1]),
			  vertex(1,i+1,geom_res[1]),
			  vertex(2,i,geom_res[1]));
    }
  else
  /*  vn = computeNormal3 (vertex(1,j,geom_res[2]),
			 vertex(2,j+1,geom_res[2]),
			 vertex(2,j,geom_res[2]),
			 vertex(2,j-1,geom_res[2])); */
  
    {
      if (f==0)
	vn = computeNormal2 (vertex(1,j,geom_res[2]),
			     vertex(2,j,geom_res[2]),
			     vertex(2,j-1,geom_res[2]));
      else
	vn = computeNormal2 (vertex(1,j,geom_res[2]),
			     vertex(2,j+1,geom_res[2]),
			     vertex(2,j,geom_res[2]));
    }
    
  return vn;
}

vec3 normal2(int i, int j)
// gets the ith vertex normal on the second position
{
  vec3 vn;
  if (geom_radius[2] != 0)
    vn=computeNormal4(vertex(2,i,geom_res[2]),
		      vertex(1,i,geom_res[2]),
		      vertex(2,i+1,geom_res[2]),
		      vertex(3,i,geom_res[2]),
		      vertex(2,i-1,geom_res[2]));
  else if (geom_shape[2] > 1)
    vn = computeNormal3 (vertex(2,j,geom_res[1]),
			     vertex(1,j-1,geom_res[1]),
			     vertex(1,j,geom_res[1]),
			     vertex(1,j+1,geom_res[1])
			     );
      
  else vn = computeNormal4(vertex(2,0,1),
			   cycle_vertex(1,0,4),
			   cycle_vertex(1,1,4),
			   cycle_vertex(1,2,4),
			   cycle_vertex(1,3,4));
    
  return vn;
}


vec3 normalp2(int f, int i, int j)
// gets the ith vertex normal on second position when polygon
{
  vec3 vn;
  if (geom_radius[2] != 0)
    {
      if (f==0) 
	vn=computeNormal3(vertex(2,i,geom_res[2]),
			  vertex(3,i,geom_res[2]),
			  vertex(2,i-1,geom_res[2]),
			  vertex(1,i,geom_res[2]));
      else
	vn=computeNormal3(vertex(2,i,geom_res[2]),
			  vertex(1,i,geom_res[2]),
			  vertex(2,i+1,geom_res[2]),
			  vertex(3,i,geom_res[2]));
    }
  else
  /*   vn = computeNormal2 (vertex(2,j,geom_res[1]),
			     vertex(1,j-1,geom_res[1]),
			     vertex(1,j,geom_res[1])
			     vertex(1,j+1,geom_res[1])
			     ); */
  
    {
      if (f==0)
	vn = computeNormal2 (vertex(2,j,geom_res[1]),
			     vertex(1,j-1,geom_res[1]),
			     vertex(1,j,geom_res[1])); 
      else
	vn = computeNormal2 (vertex(2,j,geom_res[1]),
			     vertex(1,j,geom_res[1]),
			     vertex(1,j+1,geom_res[1]));
    } 

  
  return vn;
}

vec3 cycle_vertex(int n, int i, int res)
// gets the ith vertex on a cycle arc 
{
  float a = radians(geom_angle[n])*i/res;
  return geom_pen[n] + geom_mat[n]
    *vec3(geom_radius[n]*cos (a),geom_radius[n]*sin (a),0);
}


vec3 inverse (vec3 v)
// a bit of inverse geometry (for normals)
{
  float q = dot(v,v);
  if (q == 0) return vec3(0);
  else return (vec3 (v.x/q, v.y/q,v.z/q));
}

vec3 computeNormal4(vec3 x, vec3 x1, vec3 x2, vec3 x3, vec3 x4)
// normal computation "as if" x1, x2, x3, x4 are around x on the same sphere
{
  return  normalize
    //    (cross(inverse(x1 - x) + inverse (x - x3), inverse(x2 - x) + inverse (x - x4)));
  
     (inverse(cross (x1 -x, x2 -x)) +
		   inverse(cross (x2 -x, x3 -x)) +
		   inverse(cross (x3 -x, x4 -x)) +
		   inverse(cross (x4 -x, x1 -x)));
}


vec3 computeNormal3(vec3 v,vec3 v1,vec3 v2,vec3 v3)
  // normal computation from triangles (v,v1,v2) and (v,v2,v3)
{
  return normalize
		   (inverse(cross (v1 -v, v2 -v))
		    + inverse(cross (v2 -v, v3 -v))
		    );		   
  
}

vec3 computeNormal2(vec3 v,vec3 v1,vec3 v2)
  // normal computation from triangles (v,v1,v2)
{
  return normalize (cross (v1 -v, v2 -v));
  
}
 
