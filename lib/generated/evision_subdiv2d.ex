defmodule Evision.Subdiv2D do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Subdiv2D` struct.

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
  def to_struct({:ok, %{class: Evision.Subdiv2D, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Subdiv2D, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_PTLOC_ERROR, do: -2
  @doc enum: true
  def cv_PTLOC_OUTSIDE_RECT, do: -1
  @doc enum: true
  def cv_PTLOC_INSIDE, do: 0
  @doc enum: true
  def cv_PTLOC_VERTEX, do: 1
  @doc enum: true
  def cv_PTLOC_ON_EDGE, do: 2
  @doc enum: true
  def cv_NEXT_AROUND_ORG, do: 0
  @doc enum: true
  def cv_NEXT_AROUND_DST, do: 34
  @doc enum: true
  def cv_PREV_AROUND_ORG, do: 17
  @doc enum: true
  def cv_PREV_AROUND_DST, do: 51
  @doc enum: true
  def cv_NEXT_AROUND_LEFT, do: 19
  @doc enum: true
  def cv_NEXT_AROUND_RIGHT, do: 49
  @doc enum: true
  def cv_PREV_AROUND_LEFT, do: 32
  @doc enum: true
  def cv_PREV_AROUND_RIGHT, do: 2


  @doc """
  Subdiv2D

  ##### Positional Arguments
  - **rect**: `Rect`.

    Rectangle that includes all of the 2D points that are to be added to the subdivision.

  ##### Return
  - **self**: `Evision.Subdiv2D.t()`

  Has overloading in C++

  The function creates an empty Delaunay subdivision where 2D points can be added using the function
  insert() . All of the points to be added must be within the specified rectangle, otherwise a runtime
  error is raised.

  Python prototype (for reference only):
  ```python3
  Subdiv2D(rect) -> <Subdiv2D object>
  ```
  """
  @spec subdiv2D({number(), number(), number(), number()}) :: Evision.Subdiv2D.t() | {:error, String.t()}
  def subdiv2D(rect) when is_tuple(rect)
  do
    positional = [
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.subdiv2D_Subdiv2D(positional)
    |> to_struct()
  end

  @doc """
  Subdiv2D
  ##### Return
  - **self**: `Evision.Subdiv2D.t()`

  creates an empty Subdiv2D object.
  To create a new empty Delaunay subdivision you need to use the #initDelaunay function.

  Python prototype (for reference only):
  ```python3
  Subdiv2D() -> <Subdiv2D object>
  ```
  """
  @spec subdiv2D() :: Evision.Subdiv2D.t() | {:error, String.t()}
  def subdiv2D() do
    positional = [
    ]
    :evision_nif.subdiv2D_Subdiv2D(positional)
    |> to_struct()
  end

  @doc """
  Returns the edge destination.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`.

    Subdivision edge ID.

  ##### Return
  - **retval**: `integer()`
  - **dstpt**: `Point2f*`.

    Output vertex location.

  @returns vertex ID.

  Python prototype (for reference only):
  ```python3
  edgeDst(edge) -> retval, dstpt
  ```
  """
  @spec edgeDst(Evision.Subdiv2D.t(), integer()) :: {integer(), {number(), number()}} | {:error, String.t()}
  def edgeDst(self, edge) when is_integer(edge)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge)
    ]
    :evision_nif.subdiv2D_edgeDst(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the edge origin.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`.

    Subdivision edge ID.

  ##### Return
  - **retval**: `integer()`
  - **orgpt**: `Point2f*`.

    Output vertex location.

  @returns vertex ID.

  Python prototype (for reference only):
  ```python3
  edgeOrg(edge) -> retval, orgpt
  ```
  """
  @spec edgeOrg(Evision.Subdiv2D.t(), integer()) :: {integer(), {number(), number()}} | {:error, String.t()}
  def edgeOrg(self, edge) when is_integer(edge)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge)
    ]
    :evision_nif.subdiv2D_edgeOrg(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the subdivision vertex closest to the given point.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **pt**: `Point2f`.

    Input point.

  ##### Return
  - **retval**: `integer()`
  - **nearestPt**: `Point2f*`.

    Output subdivision vertex point.

  The function is another function that locates the input point within the subdivision. It finds the
  subdivision vertex that is the closest to the input point. It is not necessarily one of vertices
  of the facet containing the input point, though the facet (located using locate() ) is used as a
  starting point.
  @returns vertex ID.

  Python prototype (for reference only):
  ```python3
  findNearest(pt) -> retval, nearestPt
  ```
  """
  @spec findNearest(Evision.Subdiv2D.t(), {number(), number()}) :: {integer(), {number(), number()}} | {:error, String.t()}
  def findNearest(self, pt) when is_tuple(pt)
  do
    positional = [
      pt: Evision.Internal.Structurise.from_struct(pt)
    ]
    :evision_nif.subdiv2D_findNearest(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns one of the edges related to the given edge.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`.

    Subdivision edge ID.

  - **nextEdgeType**: `integer()`.

    Parameter specifying which of the related edges to return.
    The following values are possible:
    - NEXT_AROUND_ORG next around the edge origin ( eOnext on the picture below if e is the input edge)
    - NEXT_AROUND_DST next around the edge vertex ( eDnext )
    - PREV_AROUND_ORG previous around the edge origin (reversed eRnext )
    - PREV_AROUND_DST previous around the edge destination (reversed eLnext )
    - NEXT_AROUND_LEFT next around the left facet ( eLnext )
    - NEXT_AROUND_RIGHT next around the right facet ( eRnext )
    - PREV_AROUND_LEFT previous around the left facet (reversed eOnext )
    - PREV_AROUND_RIGHT previous around the right facet (reversed eDnext )

  ##### Return
  - **retval**: `integer()`

  ![sample output](pics/quadedge.png)
  @returns edge ID related to the input edge.

  Python prototype (for reference only):
  ```python3
  getEdge(edge, nextEdgeType) -> retval
  ```
  """
  @spec getEdge(Evision.Subdiv2D.t(), integer(), integer()) :: integer() | {:error, String.t()}
  def getEdge(self, edge, nextEdgeType) when is_integer(edge) and is_integer(nextEdgeType)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge),
      nextEdgeType: Evision.Internal.Structurise.from_struct(nextEdgeType)
    ]
    :evision_nif.subdiv2D_getEdge(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a list of all edges.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`

  ##### Return
  - **edgeList**: `[Vec4f]`.

    Output vector.

  The function gives each edge as a 4 numbers vector, where each two are one of the edge
  vertices. i.e. org_x = v[0], org_y = v[1], dst_x = v[2], dst_y = v[3].

  Python prototype (for reference only):
  ```python3
  getEdgeList() -> edgeList
  ```
  """
  @spec getEdgeList(Evision.Subdiv2D.t()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def getEdgeList(self) do
    positional = [
    ]
    :evision_nif.subdiv2D_getEdgeList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a list of the leading edge ID connected to each triangle.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`

  ##### Return
  - **leadingEdgeList**: `[integer()]`.

    Output vector.

  The function gives one edge ID for each triangle.

  Python prototype (for reference only):
  ```python3
  getLeadingEdgeList() -> leadingEdgeList
  ```
  """
  @spec getLeadingEdgeList(Evision.Subdiv2D.t()) :: list(integer()) | {:error, String.t()}
  def getLeadingEdgeList(self) do
    positional = [
    ]
    :evision_nif.subdiv2D_getLeadingEdgeList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a list of all triangles.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`

  ##### Return
  - **triangleList**: `[Vec6f]`.

    Output vector.

  The function gives each triangle as a 6 numbers vector, where each two are one of the triangle
  vertices. i.e. p1_x = v[0], p1_y = v[1], p2_x = v[2], p2_y = v[3], p3_x = v[4], p3_y = v[5].

  Python prototype (for reference only):
  ```python3
  getTriangleList() -> triangleList
  ```
  """
  @spec getTriangleList(Evision.Subdiv2D.t()) :: list({number(), number(), number(), number(), number(), number()}) | {:error, String.t()}
  def getTriangleList(self) do
    positional = [
    ]
    :evision_nif.subdiv2D_getTriangleList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns vertex location from vertex ID.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **vertex**: `integer()`.

    vertex ID.

  ##### Return
  - **retval**: `Point2f`
  - **firstEdge**: `int*`.

    Optional. The first edge ID which is connected to the vertex.

  @returns vertex (x,y)

  Python prototype (for reference only):
  ```python3
  getVertex(vertex) -> retval, firstEdge
  ```
  """
  @spec getVertex(Evision.Subdiv2D.t(), integer()) :: {{number(), number()}, integer()} | {:error, String.t()}
  def getVertex(self, vertex) when is_integer(vertex)
  do
    positional = [
      vertex: Evision.Internal.Structurise.from_struct(vertex)
    ]
    :evision_nif.subdiv2D_getVertex(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a list of all Voronoi facets.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **idx**: `[integer()]`.

    Vector of vertices IDs to consider. For all vertices you can pass empty vector.

  ##### Return
  - **facetList**: `[[Point2f]]`.

    Output vector of the Voronoi facets.

  - **facetCenters**: `[Point2f]`.

    Output vector of the Voronoi facets center points.

  Python prototype (for reference only):
  ```python3
  getVoronoiFacetList(idx) -> facetList, facetCenters
  ```
  """
  @spec getVoronoiFacetList(Evision.Subdiv2D.t(), list(integer())) :: {list(list({number(), number()})), list({number(), number()})} | {:error, String.t()}
  def getVoronoiFacetList(self, idx) when is_list(idx)
  do
    positional = [
      idx: Evision.Internal.Structurise.from_struct(idx)
    ]
    :evision_nif.subdiv2D_getVoronoiFacetList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates a new empty Delaunay subdivision

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **rect**: `Rect`.

    Rectangle that includes all of the 2D points that are to be added to the subdivision.

  Python prototype (for reference only):
  ```python3
  initDelaunay(rect) -> None
  ```
  """
  @spec initDelaunay(Evision.Subdiv2D.t(), {number(), number(), number(), number()}) :: Evision.Subdiv2D.t() | {:error, String.t()}
  def initDelaunay(self, rect) when is_tuple(rect)
  do
    positional = [
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.subdiv2D_initDelaunay(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Insert multiple points into a Delaunay triangulation.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **ptvec**: `[Point2f]`.

    Points to insert.

  The function inserts a vector of points into a subdivision and modifies the subdivision topology
  appropriately.

  Python prototype (for reference only):
  ```python3
  insert(ptvec) -> None
  ```
  #### Variant 2:
  Insert a single point into a Delaunay triangulation.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **pt**: `Point2f`.

    Point to insert.

  ##### Return
  - **retval**: `integer()`

  The function inserts a single point into a subdivision and modifies the subdivision topology
  appropriately. If a point with the same coordinates exists already, no new point is added.
  @returns the ID of the point.
  **Note**: If the point is outside of the triangulation specified rect a runtime error is raised.

  Python prototype (for reference only):
  ```python3
  insert(pt) -> retval
  ```

  """
  @spec insert(Evision.Subdiv2D.t(), list({number(), number()})) :: Evision.Subdiv2D.t() | {:error, String.t()}
  def insert(self, ptvec) when is_list(ptvec)
  do
    positional = [
      ptvec: Evision.Internal.Structurise.from_struct(ptvec)
    ]
    :evision_nif.subdiv2D_insert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec insert(Evision.Subdiv2D.t(), {number(), number()}) :: integer() | {:error, String.t()}
  def insert(self, pt) when is_tuple(pt)
  do
    positional = [
      pt: Evision.Internal.Structurise.from_struct(pt)
    ]
    :evision_nif.subdiv2D_insert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the location of a point within a Delaunay triangulation.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **pt**: `Point2f`.

    Point to locate.

  ##### Return
  - **retval**: `integer()`
  - **edge**: `integer()`.

    Output edge that the point belongs to or is located to the right of it.

  - **vertex**: `integer()`.

    Optional output vertex the input point coincides with.

  The function locates the input point within the subdivision and gives one of the triangle edges
  or vertices.
  @returns an integer which specify one of the following five cases for point location:
  - The point falls into some facet. The function returns #PTLOC_INSIDE and edge will contain one of
    edges of the facet.

  - The point falls onto the edge. The function returns #PTLOC_ON_EDGE and edge will contain this edge.
  - The point coincides with one of the subdivision vertices. The function returns #PTLOC_VERTEX and
    vertex will contain a pointer to the vertex.

  - The point is outside the subdivision reference rectangle. The function returns #PTLOC_OUTSIDE_RECT
    and no pointers are filled.

  - One of input arguments is invalid. A runtime error is raised or, if silent or "parent" error
    processing mode is selected, #PTLOC_ERROR is returned.

  Python prototype (for reference only):
  ```python3
  locate(pt) -> retval, edge, vertex
  ```
  """
  @spec locate(Evision.Subdiv2D.t(), {number(), number()}) :: {integer(), integer(), integer()} | {:error, String.t()}
  def locate(self, pt) when is_tuple(pt)
  do
    positional = [
      pt: Evision.Internal.Structurise.from_struct(pt)
    ]
    :evision_nif.subdiv2D_locate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns next edge around the edge origin.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`.

    Subdivision edge ID.

  ##### Return
  - **retval**: `integer()`

  @returns an integer which is next edge ID around the edge origin: eOnext on the
  picture above if e is the input edge).

  Python prototype (for reference only):
  ```python3
  nextEdge(edge) -> retval
  ```
  """
  @spec nextEdge(Evision.Subdiv2D.t(), integer()) :: integer() | {:error, String.t()}
  def nextEdge(self, edge) when is_integer(edge)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge)
    ]
    :evision_nif.subdiv2D_nextEdge(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns another edge of the same quad-edge.

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`.

    Subdivision edge ID.

  - **rotate**: `integer()`.

    Parameter specifying which of the edges of the same quad-edge as the input
    one to return. The following values are possible:
    - 0 - the input edge ( e on the picture below if e is the input edge)
    - 1 - the rotated edge ( eRot )
    - 2 - the reversed edge (reversed e (in green))
    - 3 - the reversed rotated edge (reversed eRot (in green))

  ##### Return
  - **retval**: `integer()`

  @returns one of the edges ID of the same quad-edge as the input edge.

  Python prototype (for reference only):
  ```python3
  rotateEdge(edge, rotate) -> retval
  ```
  """
  @spec rotateEdge(Evision.Subdiv2D.t(), integer(), integer()) :: integer() | {:error, String.t()}
  def rotateEdge(self, edge, rotate) when is_integer(edge) and is_integer(rotate)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge),
      rotate: Evision.Internal.Structurise.from_struct(rotate)
    ]
    :evision_nif.subdiv2D_rotateEdge(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  symEdge

  ##### Positional Arguments
  - **self**: `Evision.Subdiv2D.t()`
  - **edge**: `integer()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  symEdge(edge) -> retval
  ```
  """
  @spec symEdge(Evision.Subdiv2D.t(), integer()) :: integer() | {:error, String.t()}
  def symEdge(self, edge) when is_integer(edge)
  do
    positional = [
      edge: Evision.Internal.Structurise.from_struct(edge)
    ]
    :evision_nif.subdiv2D_symEdge(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
