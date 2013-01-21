varying vec4 color;


uniform float time;
uniform float count;


void main() {
  color = gl_Color;

  vec4 v = vec4(gl_Vertex);
//  if(v.x > 0.0 && v.y > 0.0) {
//    v.x = sin(v.x) + sin(count/100.0)/2.0;
//    v.y = sin(v.y) + sin(count/100.0)/2.0;
//  }
//  else if (v.x < -0.0 && v.y < 0.0) {
//    v.x = sin(v.x) - sin(count/100.0)/2.0;
//    v.y = sin(v.y) - sin(count/100.0)/2.0;
//  }

  gl_Position = gl_ModelViewProjectionMatrix * v;
}
