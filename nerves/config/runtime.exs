import Config

mix_target = Application.get_env(:nerves_evision, :target)

# Start with Livebook defaults
Livebook.config_runtime()

# Store notebooks in a writable location on the device
notebook_path =
  if mix_target == :host do
    Path.expand("priv") <> "/"
  else
    "/data/livebook/"
  end

config :livebook,
  file_systems: [Livebook.FileSystem.Local.new(default_path: notebook_path)]

# Use the embedded runtime to run notebooks in the same VM
config :livebook,
  default_runtime: {Livebook.Runtime.Embedded, []}

# Configure plugs
config :livebook,
  plugs: [{NervesLivebook.RedirectNervesLocal, []}]

# Set the password to "nerves".
config :livebook,
  authentication_mode: :password,
  password: "nerves"

# Set the Erlang distribution cookie
config :livebook,
  cookie: :nerves_evision_cookie

# Endpoint configuration
port = if mix_target == :host, do: 8080, else: 80

config :livebook, LivebookWeb.Endpoint,
  pubsub_server: Livebook.PubSub,
  live_view: [signing_salt: "livebook"],
  http: [
    port: port,
    transport_options: [socket_opts: [:inet6]]
  ],
  code_reloader: false,
  server: true
