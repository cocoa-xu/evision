import Config

config :evision,
  unsupported_type_map: %{
    {:s, 64} => {:f, 64},
    {:u, 64} => {:f, 64},
    {:u, 32} => {:f, 32}
  }
