import Config

config :evision,
  enabled_modules: [
    :calib3d,
    :core,
    :features2d,
    :flann,
    :highgui,
    :imgcodecs,
    :imgproc,
    :ml,
    :photo,
    :stitching,
    :ts,
    :video,
    :videoio,
    :dnn
  ]

config :evision,
  disabled_modules: [
    # not supported yet
    :gapi,
    # no need for this
    :world,
    # no need for this
    :python2,
    # no need for this
    :python3,
    # no need for this
    :java
  ]

config :evision,
  enabled_img_codecs: [
    :png,
    :jpeg,
    :tiff,
    :webp,
    :openjpeg,
    :jasper,
    :openexr
  ]

# To make things easier, you can set `compile_mode` to `only_enabled_modules` so that only modules specified in `enabled_modules`
# will be compiled. Like-wise, set `except_disabled_modules` to only exclude modules in `disabled_modules`. By default, the value
# of `compile_mode` is `auto`, which means to leave unspecified modules to CMake to decide.
config :evision, compile_mode: "auto"
