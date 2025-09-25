# Macroexpand Noj LWJGL

Small example program using LWJGL's OpenGL bindings to render data from the [NASA CGI Moon Kit](https://svs.gsfc.nasa.gov/4720/).

![Moon program screenshot](moon.jpg)

Run it likes this.
On the first run the program will download color and elevation data from the CGI Moon Kit.

```Shell
clj -M moon.clj
```

You can click and drag to rotate the Moon.

[![Moon program demo video](https://i.ytimg.com/vi/UdTs5tH3DxQ/hqdefault.jpg)](https://www.youtube.com/watch?v=UdTs5tH3DxQ)
