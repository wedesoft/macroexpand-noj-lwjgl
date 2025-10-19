# Macroexpand Noj LWJGL

Small example program using LWJGL's OpenGL bindings to render data from the [NASA CGI Moon Kit](https://svs.gsfc.nasa.gov/4720/).

Presented at [Macroexpand Noj 2025](https://scicloj.github.io/macroexpand-2025/).

![Moon program screenshot](moon.jpg)

Run it likes this.
On the first run the program will download color and elevation data from the CGI Moon Kit.

```Shell
clj -M moon.clj
```

You can click and drag to rotate the Moon.
See [demo video](https://www.youtube.com/watch?v=UdTs5tH3DxQ).

See [Macroexpand Noj slides](https://www.wedesoft.de/downloads/clojure-lwjgl.pdf) for more information.
