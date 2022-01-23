## Tests

As we allow users to select a subset of OpenCV modules, all tests have corresponding tags and, by default, they will be
enabled by the availability of the module (i.e., will be tested if the corresponding module is compiled). 

However, some test data is relative large thus we don't store them in git (to avoid long cloning time and/or wasting the
disk space if you don't want to run tests). Therefore, we have a special tag named `:require_downloading`. If you'd like
to test the ones that require downloading test data, you can do

```shell
mix test --include require_downloading
```

Another special tag is `:require_ffmpeg`. OpenCV can compile and use video module when any of the following points is met
1. your system has native support for camera
2. your system has native support for some video files
3. ffmpeg related libraries are installed

Therefore, the availability of the video module does not imply that some tests can decode or encode the test video files
correctly, as the formats specified in these tests may not support by the native library provided by your system. 

Another thing is that, even if it is possible to test the camera, it would require setting up a virtual camera in the CI
environment, and that would introduce much work. So we won't consider writing test for cameras at the moment. 

Therefore, if you have ffmpeg installed and the OpenCV library is compiled with ffmpeg, then you can enable these tests
by

```shell
mix test --include require_ffmpeg
```
