import Config

config :evision, enabled_modules: [
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
  :videoio
]

config :evision, disabled_modules: [
  :dnn,
  :gapi,
  :world,
  :python2,
  :python3
]

config :evision, enabled_img_coder: [
  :png,
  :jpeg,
  :tiff,
  :webp,
  :openjpeg,
  :jasper,
  :openexr
]
