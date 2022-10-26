import Config

config :evision,
  unsupported_type_map: %{
    {:s, 64} => {:f, 64},
    {:u, 64} => {:f, 64},
    {:u, 32} => {:f, 32}
  }

config :evision, display_inline_image_iterm2: true
config :evision, display_inline_image_max_size: {8192, 8192}
