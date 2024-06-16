defmodule Evision.ArUco.CharucoParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.CharucoParameters` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.CharucoParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.CharucoParameters, ref: ref}) do
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
  CharucoParameters
  ##### Return
  - **self**: `CharucoParameters`

  Python prototype (for reference only):
  ```python3
  CharucoParameters() -> <aruco_CharucoParameters object>
  ```
  """
  @spec charucoParameters() :: Evision.ArUco.CharucoParameters.t() | {:error, String.t()}
  def charucoParameters() do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoParameters_CharucoParameters(positional)
    |> to_struct()
  end
  @spec get_cameraMatrix(Evision.ArUco.CharucoParameters.t()) :: Evision.Mat.t()
  def get_cameraMatrix(self) do
    :evision_nif.aruco_CharucoParameters_get_cameraMatrix(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cameraMatrix(Evision.ArUco.CharucoParameters.t(), Evision.Mat.maybe_mat_in()) :: Evision.ArUco.CharucoParameters.t()
  def set_cameraMatrix(self, prop) do
    :evision_nif.aruco_CharucoParameters_set_cameraMatrix(
        Evision.Internal.Structurise.from_struct(self),
        [cameraMatrix: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_distCoeffs(Evision.ArUco.CharucoParameters.t()) :: Evision.Mat.t()
  def get_distCoeffs(self) do
    :evision_nif.aruco_CharucoParameters_get_distCoeffs(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_distCoeffs(Evision.ArUco.CharucoParameters.t(), Evision.Mat.maybe_mat_in()) :: Evision.ArUco.CharucoParameters.t()
  def set_distCoeffs(self, prop) do
    :evision_nif.aruco_CharucoParameters_set_distCoeffs(
        Evision.Internal.Structurise.from_struct(self),
        [distCoeffs: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minMarkers(Evision.ArUco.CharucoParameters.t()) :: integer()
  def get_minMarkers(self) do
    :evision_nif.aruco_CharucoParameters_get_minMarkers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minMarkers(Evision.ArUco.CharucoParameters.t(), integer()) :: Evision.ArUco.CharucoParameters.t()
  def set_minMarkers(self, prop) do
    :evision_nif.aruco_CharucoParameters_set_minMarkers(
        Evision.Internal.Structurise.from_struct(self),
        [minMarkers: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tryRefineMarkers(Evision.ArUco.CharucoParameters.t()) :: boolean()
  def get_tryRefineMarkers(self) do
    :evision_nif.aruco_CharucoParameters_get_tryRefineMarkers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tryRefineMarkers(Evision.ArUco.CharucoParameters.t(), boolean()) :: Evision.ArUco.CharucoParameters.t()
  def set_tryRefineMarkers(self, prop) do
    :evision_nif.aruco_CharucoParameters_set_tryRefineMarkers(
        Evision.Internal.Structurise.from_struct(self),
        [tryRefineMarkers: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
