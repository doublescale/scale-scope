#version 330 core

layout(location = 0) in vec3 pos0;
layout(location = 1) in vec3 pos1;
layout(location = 2) in vec3 nrm0;
layout(location = 3) in vec3 nrm1;
layout(location = 4) in vec2 uvc;

uniform float frame_t;
uniform mat4 model_mat;
uniform mat4 view_mat;

out vec3 normal;
out vec2 uv;

void main() {
  vec3 pos = (1 - frame_t) * pos0 + frame_t * pos1;
  vec3 nrm = (1 - frame_t) * nrm0 + frame_t * nrm1;
  vec4 proj_pos = view_mat * (model_mat * vec4(pos, 1));
  normal = (model_mat * vec4(nrm, 0)).xyz;
  uv = vec2(uvc.x, 1-uvc.y);
  gl_Position = proj_pos;
}
