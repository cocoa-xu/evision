# Changelog

## v0.1.2-dev
- Add x86_64 musl compilation CI test.
- Fix transpose.

## v0.1.1 (2022-08-25)
- Use OpenCV 4.6.0 by default.
- Implemented a few nx callbacks (remaining ones will be implemented in the next release).
- Deprecated the use of the `EVISION_PRECOMPILED_VERSION` environment variable. The version information will be implied by the tag:

    ```elixir
    def deps do
    [
        {:evision, "~> 0.1.1", github: "cocoa-xu/evision", tag: "v0.1.1"}
    ]
    end
    ```
  
  The value of the environment variable `EVISION_PREFER_PRECOMPILED` decides whether the precompiled artefacts will be used or not.

  From the next version (>=0.1.2), `evision` will set `EVISION_PREFER_PRECOMPILED` to `true` by default.

## v0.1.0 (2022-07-23)
First release.
