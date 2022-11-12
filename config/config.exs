import Config

config :evision,
  unsupported_type_map: %{
    {:s, 64} => {:f, 64},
    {:u, 64} => {:f, 64},
    {:u, 32} => {:f, 32}
  }

config :evision, display_inline_image_iterm2: true

# Only valid when `display_inline_image_iterm2` is `true` and using in iTerm2
##
## Maximum size for the image to be rendered in iTerm2, {height, width}
##
## Image will not be displayed if either its height or width exceeds these limits.
config :evision, display_inline_image_max_size: {8192, 8192}

# Only valid when `:kino` >= 0.7 and using in livebook
#
## Image Encoding Type
##
## When rendering a 2D image with Kino in Livebook
## the image will first be encoded into either :png or :jpeg
##
## - :png usually has better quality because it is lossless compression,
##    however, it uses more bandwidth to transfer
##
## - :jpeg require less bandwidth to pass from the backend to the livebook frontend,
##    but it is lossy compression
##
## The default value is :png
config :evision, kino_render_image_encoding: :png

# Only valid when `:kino` >= 0.7 and using in livebook
#
## Maximum size for the image to be rendered in Kino, {height, width}
##
## Image will not be rendered in Kino if either its height or width exceeds these limits.
config :evision, kino_render_image_max_size: {8192, 8192}

# Only valid when `:kino` >= 0.7 and using in livebook
#
## Configure the order of Kino.Render tabs in livebook
## Default order is `[:image, :raw, :numerical]`
##   and the corresponding tabs will be:
##   Image | Raw | Numerical
##
## Note that the `:image` tab will be ignored if the `Evision.Mat` is not a 2D image.
##
## Also, it's possible to specify any combination (any subset) of these tabs,
##   including the empty one, `[]`, and in that case, the output content in the livebook
##   cell will be the same as `:raw` but without any tabs
##   -- simply put, `[]` means to only do the basic inspect and not use Kino.Layout.tabs
##
## **It's worth noting that `[] != nil`, because `nil` is default return value when `kino_render_tab_order`**
## **is not configured -- hence evision will use the default order, `[:image, :raw, :numerical]` in such case**
##
## When only specifying one type, i.e., `[:image]`, `[:raw]` or `[:numerical]`,
##   only one tab will be there
##
## Furthermore, when `kino_render_tab_order` is configured to `[:image]`, and the
##   `Evision.Mat` is not a 2D image,
##   then it will automatically fallback to `:raw`
##   -- simply put, `:image` in this case (only specifying one type) means:
##      display the Evision.Mat as an image whenever possible, and fallback to `:raw`
##      if it's not a 2D image
config :evision,
  kino_render_tab_order: [
    :image,
    :raw,
    :numerical
  ]
