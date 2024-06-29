Title: Ray Tracing with Functional Languages
Date: 2016-03-12
Category: Programming
Tags: programming, graphics, racket
Status: published
Summary: The advantages of using the functional paradigm with ray tracing.
Author: marble

There are a few notable programming features intrinsic to functional languages that make
them well suited for implementing algorithms like ray tracing. We'll be using a simple ray
tracer implemented in Typed Racket as the article of discussion.

For all of the Typed Racket source code for the ray tracer mentioned here, see my GitHub
page at [https://github.com/afritz1/AnardilRayTracer](https://github.com/afritz1/AnardilRayTracer).

### A ray tracing overview

For a little bit of history on the matter, ray tracing has been done on computers since
the 80's, and possibly even before that. Pixar already had some nicely rendered images
around that time, too. Unfortunately, ray tracing didn't really pick up much momentum
until decades later because it was too computationally intensive for computers at the
time. The typical rendering method used instead was rasterization, which was faster but
slightly less intuitive to learn, and ended up being what graphics cards were designed for
doing still to this day.

Ray tracing is distinctly different from rasterization in that the latter finds which
pixels each object covers, but ray tracing is the other way around. It finds which object
each pixel covers. As a result, ray tracing can treat each pixel independently of others,
and that is a benefit which will be explained later on.

The purpose of ray tracing is to mathematically model the interaction of light with a set
of objects and their materials in order to color an image. The rendering process can be
broken apart into several pieces, but describing every single one of those parts here
would get into too much detail because this isn't quite a ray tracing tutorial, so the
highlights will be mentioned instead.

At the beginning of the design, there is a definition of a camera; a 3D point in space
with its own set of axes:

```Scheme
; Camera definition.
(struct Camera
  ((eye : Float3)
   (forward : Float3)
   (right : Float3)
   (up : Float3)))
```

From this definition, an imaginary image plane can be projected in front of the camera
like a viewing frustum, and each ray is created by obtaining the difference from the
camera's eye to uniformly spaced points in the image plane. Each pixel is usually colored
one by one in a loop by calling a ray tracing function like this one shown below.

```Scheme
; Trace a ray through the world and obtain a color.
(: World-ray-trace (-> World Ray Float3))
(define (World-ray-trace world ray)
  (define nearest-hit (World-nearest-hit world ray))
  (if (< (Intersection-t nearest-hit) INTERSECTION-T-MAX)
      (World-get-color-at world ray nearest-hit)
      (World-background-color world)))
```

If no shape is hit, then the pixel color is the world's background color. Otherwise, the
`World-get-color-at` function calculates the shaded color at a particular intersection
point. The value for `INTERSECTION-T-MAX` is essentially infinity.

This next function is used to find the nearest intersected shape (in this case, a sphere)
by a given ray. It is highly versatile, in the sense that the rays traced through every
pixel for ray-object intersections are the same kind of rays checking towards a light to
see if a point is in shadow.

```Scheme
; Check every shape to find the nearest hit shape by a given ray.
(: World-nearest-hit (-> World Ray Intersection))
(define (World-nearest-hit world ray)
  (let loop ((nearest-shape (Intersection-empty))
             (spheres (World-spheres world)))
    (if (null? spheres)
        nearest-shape
	   ; Try intersecting a sphere, and overwrite the nearest hit if
	   ; the current try is closer.
        (let ((current-try (Sphere-intersect (car spheres) ray)))
          (loop
           (if (< (Intersection-t current-try)
                  (Intersection-t nearest-shape))
               current-try nearest-shape)
           (cdr spheres))))))
```

The definition of the sphere intersection routine is left out here for simplicity.

One of the nice features of ray tracing is its modular nature. Simply changing the
behavior for how to color one pixel changes how to color every other pixel as well. The
ability to add or remove effects is a great option to have, both during development and in
practice, as some effects are either unnecessary or unnoticed by the user and only add to
the rendering time, especially in some real-time applications like video games where it
should be easy to tune graphics options.

Several advanced effects in ray tracing can be "distributed"; that is, they take some
number of samples within a certain domain as an approximation of an integral, whether it
be something like the percentage of light visible at a point or the percentage of ambient
occlusion at a point. With regards to point sampling, only one point within the domain of
the integral is sampled, and that leads to a very fast but unrealistic approximation which
gives effects like sharp shadows and perfect reflections. In order to more accurately
model the appearance of a physically-based effect, sampling within the domain of the
integral either tens, hundreds, or even thousands of times will be necessary for the
average result to converge to an acceptable value.

Here is one such "distributed" ray tracing function for approximating the amount of
ambient light at an intersection point. `EPSILON` is a small number (less than 0.0001) and
`WORLD-AMBIENT-SAMPLES` is the number of random samples to take before averaging the
result. `WORLD-MAX-OCCLUSION-DISTANCE` affects the overall strength of ambient occlusion
everywhere in the scene and can be modified to achieve a desired look.

```Scheme
; Get the percentage of ambient light visible at a point.
; A returned value of 1.0 means there is no ambient occlusion there.
(: World-get-ambient-percent (-> World Float3 Float3 Float))
(define (World-get-ambient-percent world point normal)
  ; Offset the point from the intersection surface slightly to avoid
  ; self-intersections due to float imprecision.
  (define point-offset
    (Float3-add point (Float3-scale-by-scalar normal EPSILON)))
  (let loop ((n 0)
             (percent 0.0))
    (if (= n WORLD-AMBIENT-SAMPLES)
        (exact->inexact (/ percent WORLD-AMBIENT-SAMPLES))
        ; Cast a ray in a random direction in a hemisphere and see what
        ; geometry (if any) is hit.
        (let* ((hemisphere-direction
                (Float3-random-hemisphere-direction normal))
               (hemisphere-ray
                (Ray point-offset hemisphere-direction))
               (current-try (World-nearest-hit world hemisphere-ray))
			   ; Closer geometry has a greater effect on occlusion.
               (new-percent
                (if (> (Intersection-t current-try)
                       WORLD-MAX-OCCLUSION-DISTANCE)
                    1.0
                    (exact->inexact (/ (Intersection-t current-try)
                                       WORLD-MAX-OCCLUSION-DISTANCE)))))
          (loop (+ n 1)
                (+ percent new-percent))))))
```

The naive way to approximate the integral (as is done here) is to randomly sample within
the domain multiple times, sum up each sample's value, and average the result. The good
news about this method is that it will eventually give a reasonable result. The bad news
is that it converges very slowly. In most cases it's because not all samples are evenly
spaced within the sampling domain (like a circle or a hemisphere), which can cause a high
variance between each pixel's result, and so a more careful distribution of samples can
greatly reduce the number of total samples needed. One such topic in this domain is called
stochastic sampling, but that is outside the scope of this article.

In reality, all of the illumination components would be obtained through the weighted
summation of colors from countless rays bouncing all around the scene. This is done in
many professional rendering systems which are based on a more complex version of ray
tracing called "path tracing".

### The functional advantage

Ray tracing can be done entirely with an immutable data structure, and that fits right in
with the paradigm of functional programming. In most imperative languages, one must
explicitly tag a variable as a constant, but in most functional languages, constant
qualification is the default behavior. In that case, writing `mutable` or `set!` for
example becomes required when the programmer wants to change the state of a variable in a
function.

Furthermore, stateless functions, which are a common appearance in functional languages,
are easier to prove correct because they do not change state in between function calls,
and thus add to the overall convenience and cleanliness of the program. Since all
components of the ray tracing process are pure functions, ray tracing goes well with this
programming style that comes free with the functional paradigm.

Every pixel's color can be calculated independently of other pixels, so ray tracing is
inherently a very parallelizable algorithm. Since all of the scene data is immutable for
the entire process as well, it becomes even more trivial to parallelize because there are
never any race conditions between threads. It is often called an "embarrassingly parallel"
problem in that regard.

### The functional disadvantage

Unfortunately, one of the reasons why functional languages aren't used more often in
computer graphics is generally because of their dependence on the heap. In a
computationally intensive application like ray tracing where millions or even billions of
calculations are done to produce the final image, every step of the process should be made
as efficient as possible. This is where imperative languages shine, especially low-level
ones like C that allow the programmer to explicitly avoid the heap by restricting
themselves to working only with the stack and global variables.

Some compute frameworks like OpenCL even remove the notion of a stack, because all
functions are inlined at compile time, and as a result, all runtime calculations are done
in a constant-size chunk of memory. This is how some early languages like FORTRAN worked,
and part of the reason why they were so fast is because they didn't spend time
manipulating any stack or heap data structure.

### Another strategy

In any case, ray tracing with a functional language still lends itself to many
opportunities for logical optimization such as parallelization as described above. One
possible next step is to separate the algorithm's definition from its execution schedule.
This way, the execution can be organized to run in a certain order that is independent of
the algorithm, and this can have a significant impact on performance depending on the
platform the algorithm is run on, whether that be a CPU, GPU, or other compute device.

One modern instance of this "separation" design practice is seen in the Halide programming
language. What's interesting about Halide is that it is a functional language that is
compiled inside of a C++ program into specially scheduled instructions which often times
can outpace even hand-optimized assembly for applications like image processing. The
Halide website is at [https://halide-lang.org/](https://halide-lang.org/). Other
domain-specific languages which focus on certain sets of problems like ray tracing may be
following in Halide's footsteps in the near future.

### Conclusion

The ray tracing algorithm can be written in many different programming languages, and each
language has its own pros and cons. The concept of a paradigm is to give a preference
towards a certain programming style and a way of thinking, and it's the paradigm of
functional languages like Racket that allows ray tracing to be written in a more native
and concise manner.
