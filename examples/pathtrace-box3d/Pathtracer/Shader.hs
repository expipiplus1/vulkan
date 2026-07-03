{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The path-tracing compute shader, compiled to SPIR-V at build time.

The @pathtrace-reflect@ kernel plus a sun-driven day/night sky and an emissive
material for the glowies. @buffer_reference@ needs SPIR-V 1.3+, so this is
compiled with @--target-env vulkan1.2@ ('compileShaderQ' rather than the plain
@comp@ quasiquoter). The whole interface is reflected in "Pathtracer".
-}
module Pathtracer.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)

{- | Scene intersection walks a BVH whose nodes are linked by 64-bit
@buffer_reference@ device addresses (BDA):

  * @Camera@ UBO        (std140) -> generated Haskell record (Geomancy.Vec3);
      carries the sun direction and the night-time emissive ramp;
  * @Scene@ input SSBO  (binding 1, readonly)  -> leaf sphere geometry, indexed
      by a BVH leaf's @sphereIndex@;
  * @Image@ output SSBO (binding 2, writeonly) -> descriptor set layout;
  * @BvhNode@ (buffer_reference) -> generated record with DeviceAddress children;
  * @Frame@ push constants (std430) carry the root BvhNode device address;
  * @SAMPLES@ \/ @MAX_BOUNCES@ spec constants -> the pipeline's SpecializationInfo.
-}
code :: ByteString
code =
  $( compileShaderQ
       (Just "vulkan1.2")
       "comp"
       Nothing
       [glsl|
        #version 460
        #extension GL_EXT_buffer_reference : require

        layout(local_size_x = 16, local_size_y = 16) in;

        // Compile-time tunables, specialized at pipeline creation.
        layout(constant_id = 0) const uint SAMPLES = 16u;
        layout(constant_id = 1) const uint MAX_BOUNCES = 8u;

        // All members are vec3/vec4 (std140 base alignment 16). The sun fields
        // ride one vec4: a scalar after a vec3 would be packed into its tail by
        // glslang (offset 76) but padded to 80 by the generated host record — a
        // mismatch the layout guardrail does not yet catch.
        layout(set = 0, binding = 0, std140) uniform Camera {
          vec3 origin;
          vec3 lowerLeft;
          vec3 horizontal;
          vec3 vertical;
          vec4 sunGlow; // xyz: unit vector toward the sun (may point below the
                        // horizon), w: emissive ramp — 0 in daylight, 1 at night
        } cam;

        struct Sphere {
          vec4 centerRadius; // xyz: center, w: radius
          vec4 albedo;       // rgb: albedo (emissive: the glow colour)
          vec4 material;     // x: type (0 lambertian, 1 metal, 2 dielectric,
                             //          3 emissive, 4 beacon),
                             // y: fuzz / emissive strength, z: ior;
                             // beacon: yzw is the emission RGB
        };

        layout(set = 0, binding = 1, std430) readonly buffer Scene {
          Sphere spheres[];
        };

        layout(set = 0, binding = 2, std430) writeonly buffer Image {
          vec4 pixels[];
        };

        // A BVH node reached by device address. Internal nodes have child
        // addresses and sphereIndex < 0; leaves have sphereIndex >= 0 (into the
        // Scene buffer) and null children. Fields are ordered by non-increasing
        // alignment (16,16,8,8,4).
        layout(buffer_reference) buffer BvhNode;          // fwd decl enables self-reference
        layout(buffer_reference, std430) buffer BvhNode {
          vec4 boundsMin;   // xyz AABB min
          vec4 boundsMax;   // xyz AABB max
          BvhNode left;     // child device address (null for a leaf)
          BvhNode right;    // child device address (null for a leaf)
          int sphereIndex;  // >= 0: leaf sphere index; < 0: internal node
        };

        layout(push_constant, std430) uniform Frame {
          BvhNode root;      // entry address into the BVH
          uvec2 resolution;  // image size in pixels
          uvec2 tileOffset;  // first pixel (x, y) this dispatch (tile) covers
          uint  seed;        // varies sampling between runs
          uint  pad0;        // pad the block to its 8-byte alignment, so the
                             // range matches the (aligned) host record exactly
        } frame;

        struct Ray {
          vec3 o;
          vec3 d;
        };

        struct Hit {
          float t;
          vec3 p;
          vec3 n;
          bool front;
          vec4 albedo;
          vec4 material;
        };

        // PCG hash RNG, advanced in place.
        uint pcg(inout uint s) {
          s = s * 747796405u + 2891336453u;
          uint w = ((s >> ((s >> 28u) + 4u)) ^ s) * 277803737u;
          return (w >> 22u) ^ w;
        }

        float rnd(inout uint s) {
          return float(pcg(s)) * (1.0 / 4294967296.0);
        }

        // Uniformly distributed point on the unit sphere.
        vec3 rndUnit(inout uint s) {
          float z = rnd(s) * 2.0 - 1.0;
          float a = rnd(s) * 6.28318530718;
          float r = sqrt(max(0.0, 1.0 - z * z));
          return vec3(r * cos(a), r * sin(a), z);
        }

        bool hitSphere(Sphere sp, Ray ray, float tmin, float tmax, inout Hit h) {
          vec3 center = sp.centerRadius.xyz;
          float radius = sp.centerRadius.w;
          vec3 oc = ray.o - center;
          float a = dot(ray.d, ray.d);
          float halfB = dot(oc, ray.d);
          float c = dot(oc, oc) - radius * radius;
          float disc = halfB * halfB - a * c;
          if (disc < 0.0) {
            return false;
          }
          float sq = sqrt(disc);
          float t = (-halfB - sq) / a;
          if (t < tmin || t > tmax) {
            t = (-halfB + sq) / a;
            if (t < tmin || t > tmax) {
              return false;
            }
          }
          h.t = t;
          h.p = ray.o + t * ray.d;
          vec3 outwardN = (h.p - center) / radius;
          h.front = dot(ray.d, outwardN) < 0.0;
          h.n = h.front ? outwardN : -outwardN;
          h.albedo = sp.albedo;
          h.material = sp.material;
          return true;
        }

        // Ray vs AABB slab test, gated to [tmin, tmax].
        bool aabbHit(vec3 bmin, vec3 bmax, Ray ray, float tmin, float tmax) {
          vec3 inv = 1.0 / ray.d;
          vec3 t0 = (bmin - ray.o) * inv;
          vec3 t1 = (bmax - ray.o) * inv;
          vec3 tsmall = min(t0, t1);
          vec3 tbig = max(t0, t1);
          float lo = max(max(tsmall.x, tsmall.y), max(tsmall.z, tmin));
          float hi = min(min(tbig.x, tbig.y), min(tbig.z, tmax));
          return hi >= lo;
        }

        // Max BVH traversal depth. A balanced median-split BVH of N leaves is
        // ~log2(N) deep, so this covers far more spheres than the host will
        // ever generate; the bound guards against a pathologically deep tree (a
        // subtree is skipped rather than overflowing the stack).
        const int BVH_STACK = 64;

        // Walk the BVH from the root device address, hopping pointers to find
        // the closest sphere hit. An explicit stack stands in for recursion.
        bool worldHit(Ray ray, float tmin, float tmax, out Hit h) {
          bool hitAny = false;
          float closest = tmax;

          BvhNode stack[BVH_STACK];
          int sp = 0;
          stack[sp++] = frame.root;

          while (sp > 0) {
            BvhNode node = stack[--sp];
            if (!aabbHit(node.boundsMin.xyz, node.boundsMax.xyz, ray, tmin, closest)) {
              continue;
            }
            if (node.sphereIndex >= 0) {
              Hit tmp;
              if (hitSphere(spheres[node.sphereIndex], ray, tmin, closest, tmp)) {
                hitAny = true;
                closest = tmp.t;
                h = tmp;
              }
            } else if (sp + 2 <= BVH_STACK) {
              stack[sp++] = node.left;
              stack[sp++] = node.right;
            }
          }
          return hitAny;
        }

        // A simple analytic day/night sky driven by the sun direction: a
        // vertical gradient blending day and night by sun elevation, a warm
        // band around a low sun, and the disc itself. The sky is the scene's
        // environment light, so its brightness curve *is* the day/night
        // lighting.
        vec3 skyColor(vec3 rawDir) {
          vec3 dir = normalize(rawDir);
          float sunH = cam.sunGlow.y;
          float day = smoothstep(-0.12, 0.25, sunH);
          float dusk = clamp(1.0 - abs(sunH) * 3.0, 0.0, 1.0);

          float t = 0.5 * (dir.y + 1.0);
          vec3 dayCol = mix(vec3(1.0), vec3(0.5, 0.7, 1.0), t);
          vec3 nightCol = mix(vec3(0.035, 0.045, 0.08), vec3(0.002, 0.003, 0.012), t);
          vec3 sky = mix(nightCol, dayCol, day);

          float sunAmount = max(dot(dir, cam.sunGlow.xyz), 0.0);
          // Warm glow spreading from a rising/setting sun.
          sky += dusk * vec3(0.9, 0.35, 0.08) * pow(sunAmount, 4.0);
          // The disc: small and hot, cut off once the sun is below the horizon.
          sky += vec3(1.0, 0.85, 0.6) * pow(sunAmount, 2000.0) * 20.0
               * smoothstep(-0.03, 0.05, sunH);
          return sky;
        }

        float schlick(float cosine, float ref) {
          float r0 = (1.0 - ref) / (1.0 + ref);
          r0 = r0 * r0;
          return r0 + (1.0 - r0) * pow(1.0 - cosine, 5.0);
        }

        vec3 rayColor(Ray ray, inout uint s) {
          vec3 radiance = vec3(0.0);
          vec3 attenuation = vec3(1.0);
          for (uint bounce = 0u; bounce < MAX_BOUNCES; ++bounce) {
            Hit h;
            if (!worldHit(ray, 0.001, 1e30, h)) {
              return radiance + attenuation * skyColor(ray.d);
            }

            int mat = int(h.material.x + 0.5);
            if (mat == 1) {
              // Metal: reflect, perturbed by fuzz.
              vec3 refl = reflect(normalize(ray.d), h.n);
              vec3 dir = refl + h.material.y * rndUnit(s);
              if (dot(dir, h.n) <= 0.0) {
                return radiance;
              }
              ray = Ray(h.p, dir);
              attenuation *= h.albedo.rgb;
            } else if (mat == 2) {
              // Dielectric: refract or reflect (Schlick).
              float ior = h.material.z;
              float ratio = h.front ? (1.0 / ior) : ior;
              vec3 ud = normalize(ray.d);
              float cosT = min(dot(-ud, h.n), 1.0);
              float sinT = sqrt(max(0.0, 1.0 - cosT * cosT));
              vec3 dir;
              if (ratio * sinT > 1.0 || schlick(cosT, ratio) > rnd(s)) {
                dir = reflect(ud, h.n);
              } else {
                dir = refract(ud, h.n, ratio);
              }
              ray = Ray(h.p, dir);
            } else {
              // Lambertian (0), night glowies (3), and the beacon (4): the
              // emissive kinds add their glow, then all scatter diffusely — by
              // day a glowy is just a matte ball, and the beacon is one
              // whenever it isn't pulsing.
              if (mat == 3) {
                radiance += attenuation * h.albedo.rgb * h.material.y * cam.sunGlow.w;
              } else if (mat == 4) {
                radiance += attenuation * h.material.yzw;
              }
              vec3 dir = h.n + rndUnit(s);
              if (dot(dir, dir) < 1e-8) {
                dir = h.n;
              }
              ray = Ray(h.p, dir);
              attenuation *= h.albedo.rgb;
            }
          }
          return radiance; // exceeded the bounce budget
        }

        void main() {
          // gl_GlobalInvocationID is relative to this dispatch; tileOffset
          // places it into the full image. The headless host renders in tiles —
          // each pixel's worker runs its whole sample loop, so tile area ×
          // SAMPLES bounds how much work a single submission can be, keeping
          // every submission far under the GPU's hang-recovery watchdog at any
          // resolution or sample count.
          uvec2 gid = gl_GlobalInvocationID.xy + frame.tileOffset;
          if (gid.x >= frame.resolution.x || gid.y >= frame.resolution.y) {
            return;
          }

          uint s = (gid.x * 1973u + gid.y * 9277u + frame.seed * 26699u) | 1u;

          // Stratified (jittered) sampling: the pixel splits into an sx × sy
          // grid of strata, one sample in each — free variance reduction over
          // independent uniform samples. sx is the largest divisor of SAMPLES
          // not above its square root, so every sample count is covered exactly
          // (powers of two give near-square grids; a prime would degenerate to
          // 1 × SAMPLES rows).
          uint sx = max(1u, uint(sqrt(float(SAMPLES))));
          while (SAMPLES % sx != 0u) {
            sx--;
          }
          uint sy = SAMPLES / sx;

          vec3 color = vec3(0.0);
          for (uint i = 0u; i < SAMPLES; ++i) {
            float du = (float(i % sx) + rnd(s)) / float(sx);
            float dv = (float(i / sx) + rnd(s)) / float(sy);
            float u = (float(gid.x) + du) / float(frame.resolution.x);
            float v = (float(gid.y) + dv) / float(frame.resolution.y);
            // Image rows grow downward; flip v so the top row maps to the top
            // of the viewport.
            vec3 dir =
              cam.lowerLeft + u * cam.horizontal + (1.0 - v) * cam.vertical - cam.origin;
            color += rayColor(Ray(cam.origin, dir), s);
          }
          color /= float(SAMPLES);
          color = sqrt(color); // gamma 2.0

          uint idx = gid.y * frame.resolution.x + gid.x;
          pixels[idx] = vec4(color, 1.0);
        }
       |]
   )
