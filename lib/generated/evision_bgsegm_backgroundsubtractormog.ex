defmodule Evision.BgSegm.BackgroundSubtractorMOG do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BgSegm.BackgroundSubtractorMOG` struct.

  - **ref**. `reference()`

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    ref: reference()
  }
  @enforce_keys [:ref]
  defstruct [:ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct({:ok, %{class: Evision.BgSegm.BackgroundSubtractorMOG, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BgSegm.BackgroundSubtractorMOG, ref: ref}) do
    %T{
      ref: ref
    }
  end

  @doc false
  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
  
  @doc false
  def from_struct(%T{ref: ref}) do
    ref
  end

  @doc """
  getBackgroundRatio

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getBackgroundRatio() -> retval
  ```
  """
  @spec getBackgroundRatio(Evision.BgSegm.BackgroundSubtractorMOG.t()) :: number() | {:error, String.t()}
  def getBackgroundRatio(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_getBackgroundRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHistory

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getHistory() -> retval
  ```
  """
  @spec getHistory(Evision.BgSegm.BackgroundSubtractorMOG.t()) :: integer() | {:error, String.t()}
  def getHistory(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_getHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNMixtures

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNMixtures() -> retval
  ```
  """
  @spec getNMixtures(Evision.BgSegm.BackgroundSubtractorMOG.t()) :: integer() | {:error, String.t()}
  def getNMixtures(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_getNMixtures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNoiseSigma

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getNoiseSigma() -> retval
  ```
  """
  @spec getNoiseSigma(Evision.BgSegm.BackgroundSubtractorMOG.t()) :: number() | {:error, String.t()}
  def getNoiseSigma(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_getNoiseSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBackgroundRatio

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`
  - **backgroundRatio**: `double`

  Python prototype (for reference only):
  ```python3
  setBackgroundRatio(backgroundRatio) -> None
  ```
  """
  @spec setBackgroundRatio(Evision.BgSegm.BackgroundSubtractorMOG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setBackgroundRatio(self, backgroundRatio) when is_number(backgroundRatio)
  do
    positional = [
      backgroundRatio: Evision.Internal.Structurise.from_struct(backgroundRatio)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_setBackgroundRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHistory

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`
  - **nframes**: `integer()`

  Python prototype (for reference only):
  ```python3
  setHistory(nframes) -> None
  ```
  """
  @spec setHistory(Evision.BgSegm.BackgroundSubtractorMOG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setHistory(self, nframes) when is_integer(nframes)
  do
    positional = [
      nframes: Evision.Internal.Structurise.from_struct(nframes)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_setHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNMixtures

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`
  - **nmix**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNMixtures(nmix) -> None
  ```
  """
  @spec setNMixtures(Evision.BgSegm.BackgroundSubtractorMOG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setNMixtures(self, nmix) when is_integer(nmix)
  do
    positional = [
      nmix: Evision.Internal.Structurise.from_struct(nmix)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_setNMixtures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNoiseSigma

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`
  - **noiseSigma**: `double`

  Python prototype (for reference only):
  ```python3
  setNoiseSigma(noiseSigma) -> None
  ```
  """
  @spec setNoiseSigma(Evision.BgSegm.BackgroundSubtractorMOG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def setNoiseSigma(self, noiseSigma) when is_number(noiseSigma)
  do
    positional = [
      noiseSigma: Evision.Internal.Structurise.from_struct(noiseSigma)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorMOG_setNoiseSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
