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
  :dnn,     # not supported yet
  :gapi,    # not supported yet
  :world,   # no need for this
  :python2, # no need for this
  :python3  # no need for this
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
