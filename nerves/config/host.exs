import Config

config :nerves_runtime,
  target: "host"

# Configure Nerves runtime dependencies for the host
config :nerves_runtime, Nerves.Runtime.KV.Mock, %{"nerves_fw_devpath" => "/dev/will_not_work"}

config :vintage_net,
  resolvconf: "/dev/null",
  persistence_dir: "./test_tmp/persistence",
  path: "#{File.cwd!()}/test/fixtures/root/bin"

# Turn off ntp
config :nerves_time, servers: []

config :mdns_lite, if_monitor: MdnsLite.InetMonitor
