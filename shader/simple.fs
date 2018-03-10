#version 330 core

in vec3 normal;
in vec2 uv;
out vec3 color;

uniform sampler2D tex;

float norm_dot(vec3 n, vec3 dir) {
  float a = 0.2;
  return max(0.10, a + (1 - a) * dot(n, normalize(dir)));
}

void main() {
  vec3 nn = normalize(normal);
  vec3 ldir = vec3(1, 2, 4);
  color = norm_dot(nn, ldir) * vec3(texture(tex, uv));
}
