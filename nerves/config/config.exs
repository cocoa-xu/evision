import Config

# Enable the Nerves integration with Mix
Application.start(:nerves_bootstrap)

config :nerves_evision, target: Mix.target()

# Customize non-Elixir parts of the firmware. See
# https://hexdocs.pm/nerves/advanced-configuration.html for details.

config :nerves, :firmware,
  rootfs_overlay: "rootfs_overlay",
  provisioning: "config/provisioning.conf"

# Set the SOURCE_DATE_EPOCH date for reproducible builds.
# See https://reproducible-builds.org/docs/source-date-epoch/ for more information

config :nerves, source_date_epoch: "1603310828"

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Livebook's explore section is built at compile-time
config :livebook, :explore_notebooks, []

if Mix.target() == :host do
  import_config "host.exs"
else
  import_config "target.exs"
end

import_config "evision.exs"
