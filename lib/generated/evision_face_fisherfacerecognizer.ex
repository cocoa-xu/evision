defmodule Evision.Face.FisherFaceRecognizer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.FisherFaceRecognizer` struct.

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
  def to_struct({:ok, %{class: Evision.Face.FisherFaceRecognizer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.FisherFaceRecognizer, ref: ref}) do
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
  create
  ##### Keyword Arguments
  - **num_components**: `integer()`.

    The number of components (read: Fisherfaces) kept for this Linear
    Discriminant Analysis with the Fisherfaces criterion. It's useful to keep all components, that
    means the number of your classes c (read: subjects, persons you want to recognize). If you leave
    this at the default (0) or set it to a value less-equal 0 or greater (c-1), it will be set to the
    correct number (c-1) automatically.

  - **threshold**: `double`.

    The threshold applied in the prediction. If the distance to the nearest neighbor
    is larger than the threshold, this method returns -1.

  ##### Return
  - **retval**: `FisherFaceRecognizer`

  ### Notes:
  - Training and prediction must be done on grayscale images, use cvtColor to convert between the
    color spaces.

  - **THE FISHERFACES METHOD MAKES THE ASSUMPTION, THAT THE TRAINING AND TEST IMAGES ARE OF EQUAL
    SIZE.** (caps-lock, because I got so many mails asking for this). You have to make sure your
    input data has the correct shape, else a meaningful exception is thrown. Use resize to resize
    the images.

  - This model does not support updating.

  ### Model internal data:
  - num_components see FisherFaceRecognizer::create.
  - threshold see FisherFaceRecognizer::create.
  - eigenvalues The eigenvalues for this Linear Discriminant Analysis (ordered descending).
  - eigenvectors The eigenvectors for this Linear Discriminant Analysis (ordered by their
    eigenvalue).

  - mean The sample mean calculated from the training data.
  - projections The projections of the training data.
  - labels The labels corresponding to the projections.

  Python prototype (for reference only):
  ```python3
  create([, num_components[, threshold]]) -> retval
  ```
  """
  @spec create([{:num_components, term()} | {:threshold, term()}] | nil) :: Evision.Face.FisherFaceRecognizer.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:num_components, :threshold])
    positional = [
    ]
    :evision_nif.face_face_FisherFaceRecognizer_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **num_components**: `integer()`.

    The number of components (read: Fisherfaces) kept for this Linear
    Discriminant Analysis with the Fisherfaces criterion. It's useful to keep all components, that
    means the number of your classes c (read: subjects, persons you want to recognize). If you leave
    this at the default (0) or set it to a value less-equal 0 or greater (c-1), it will be set to the
    correct number (c-1) automatically.

  - **threshold**: `double`.

    The threshold applied in the prediction. If the distance to the nearest neighbor
    is larger than the threshold, this method returns -1.

  ##### Return
  - **retval**: `FisherFaceRecognizer`

  ### Notes:
  - Training and prediction must be done on grayscale images, use cvtColor to convert between the
    color spaces.

  - **THE FISHERFACES METHOD MAKES THE ASSUMPTION, THAT THE TRAINING AND TEST IMAGES ARE OF EQUAL
    SIZE.** (caps-lock, because I got so many mails asking for this). You have to make sure your
    input data has the correct shape, else a meaningful exception is thrown. Use resize to resize
    the images.

  - This model does not support updating.

  ### Model internal data:
  - num_components see FisherFaceRecognizer::create.
  - threshold see FisherFaceRecognizer::create.
  - eigenvalues The eigenvalues for this Linear Discriminant Analysis (ordered descending).
  - eigenvectors The eigenvectors for this Linear Discriminant Analysis (ordered by their
    eigenvalue).

  - mean The sample mean calculated from the training data.
  - projections The projections of the training data.
  - labels The labels corresponding to the projections.

  Python prototype (for reference only):
  ```python3
  create([, num_components[, threshold]]) -> retval
  ```
  """
  @spec create() :: Evision.Face.FisherFaceRecognizer.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.face_face_FisherFaceRecognizer_create_static(positional)
    |> to_struct()
  end
end
