import evision/types.{type DType}
import gleam/erlang.{type Reference}

pub type Mat {
  Mat(
    channels: Int,
    dims: Int,
    dtype: DType,
    raw_type: Int,
    shape: List(Int),
    ref: Reference,
  )
}

@external(erlang, "evision_mat", "full")
pub fn full(shape: shape, number: number, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "number")
pub fn number(number: number, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "at")
pub fn at(mat: Mat, position: position) -> Mat

@external(erlang, "evision_mat", "add")
pub fn add(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "add")
pub fn add_type(mat1: Mat, mat2: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "subtract")
pub fn subtract(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "subtract")
pub fn subtract_type(mat1: Mat, mat2: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "multiply")
pub fn multiply(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "multiply")
pub fn multiply_type(mat1: Mat, mat2: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "matrix_multiply")
pub fn matrix_multiply(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "matrix_multiply")
pub fn matrix_multiply_type(mat1: Mat, mat2: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "divide")
pub fn divide(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "divide")
pub fn divide_type(mat1: Mat, mat2: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "bitwise_and")
pub fn bitwise_and(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "bitwise_or")
pub fn bitwise_or(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "bitwise_xor")
pub fn bitwise_xor(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "cmp")
pub fn cmp(mat1: Mat, mat2: Mat, op: op) -> Mat

@external(erlang, "evision_mat", "logical_and")
pub fn logical_and(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "logical_or")
pub fn logical_or(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "logical_xor")
pub fn logical_xor(mat1: Mat, mat2: Mat) -> Mat

@external(erlang, "evision_mat", "abs")
pub fn abs(mat: Mat) -> Mat

@external(erlang, "evision_mat", "expm1")
pub fn expm1(mat: Mat) -> Mat

@external(erlang, "evision_mat", "clip")
pub fn clip(mat: Mat, lower: number1, upper: number2) -> Mat

@external(erlang, "evision_mat", "transpose")
pub fn transpose_by_axes_as_shape(
  mat: Mat,
  axes: axes,
  as_shape: as_shape,
) -> Mat

@external(erlang, "evision_mat", "transpose")
pub fn transpose_by_axes(mat: Mat, axes: axes) -> Mat

@external(erlang, "evision_mat", "transpose")
pub fn transpose(mat: Mat) -> Mat

@external(erlang, "evision_mat", "type")
pub fn dtype(mat: Mat) -> Mat

@external(erlang, "evision_mat", "bitwise_not")
pub fn bitwise_not(mat: Mat) -> Mat

@external(erlang, "evision_mat", "ceil")
pub fn ceil(mat: Mat) -> Mat

@external(erlang, "evision_mat", "floor")
pub fn floor(mat: Mat) -> Mat

@external(erlang, "evision_mat", "negate")
pub fn negate(mat: Mat) -> Mat

@external(erlang, "evision_mat", "round")
pub fn round(mat: Mat) -> Mat

@external(erlang, "evision_mat", "sign")
pub fn sign(mat: Mat) -> Mat

@external(erlang, "evision_mat", "setTo")
pub fn set_to(mat: Mat, value: value, mask: mask) -> Mat

@external(erlang, "evision_mat", "dot")
pub fn dot(lhs: Mat, rhs: Mat) -> Mat

@external(erlang, "evision_mat", "as_type")
pub fn as_type(mat: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "shape")
pub fn shape(mat: Mat) -> shape

@external(erlang, "evision_mat", "channels")
pub fn channels(mat: Mat) -> Int

@external(erlang, "evision_mat", "depth")
pub fn depth(mat: Mat) -> Int

@external(erlang, "evision_mat", "raw_type")
pub fn raw_type(mat: Mat) -> Int

@external(erlang, "evision_mat", "isSubmatrix")
pub fn is_submatrix(mat: Mat) -> Bool

@external(erlang, "evision_mat", "isContinuous")
pub fn is_continuous(mat: Mat) -> Bool

@external(erlang, "evision_mat", "elemSize")
pub fn elem_size(mat: Mat) -> Int

@external(erlang, "evision_mat", "elemSize1")
pub fn elem_size1(mat: Mat) -> Int

@external(erlang, "evision_mat", "size")
pub fn size(mat: Mat) -> size

@external(erlang, "evision_mat", "total")
pub fn total1(mat: Mat) -> any

@external(erlang, "evision_mat", "total")
pub fn total2(mat: Mat, start_dim: start_dim) -> any

@external(erlang, "evision_mat", "total")
pub fn total3(mat: Mat, start_dim: start_dim, end_dim: end_dim) -> any

@external(erlang, "evision_mat", "last_dim_as_channel")
pub fn last_dim_as_channel(mat: Mat) -> Mat

@external(erlang, "evision_mat", "zeros")
pub fn zeros(shape: shape, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "ones")
pub fn ones(shape: shape, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "reshape")
pub fn reshape(mat: Mat, shape: shape) -> Mat

@external(erlang, "evision_mat", "arange")
pub fn arange4(from: from, to: to, step: step, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "arange")
pub fn arange5(
  from: from,
  to: to,
  step: step,
  dtype: dtype,
  shape: shape,
) -> Mat

@external(erlang, "evision_mat", "to_batched")
pub fn to_batched3(
  mat: Mat,
  batch_size: batch_size,
  left_over: left_over,
) -> any

@external(erlang, "evision_mat", "to_batched")
pub fn to_batched4(
  mat: Mat,
  batch_size: batch_size,
  as_shape: as_shape,
  left_over: left_over,
) -> any

@external(erlang, "evision_mat", "clone")
pub fn clone(mat: Mat) -> Mat

@external(erlang, "evision_mat", "empty")
pub fn empty() -> Mat

@external(erlang, "evision_mat", "to_binary")
pub fn to_binary(mat: Mat) -> BitArray

@external(erlang, "evision_mat", "from_binary")
pub fn from_binary(
  binary: BitArray,
  dtype: dtype,
  rows: Int,
  cols: Int,
  channels: Int,
) -> Mat

@external(erlang, "evision_mat", "from_binary_by_shape")
pub fn from_binary_by_shape(binary: BitArray, dtype: dtype, shape: shape) -> Mat

@external(erlang, "evision_mat", "eye")
pub fn eye(mat: Mat, dtype: dtype) -> Mat

@external(erlang, "evision_mat", "as_shape")
pub fn as_shape(mat: Mat, as_shape: as_shape) -> Mat

@external(erlang, "evision_mat", "squeeze")
pub fn squeeze(mat: Mat) -> Mat

@external(erlang, "evision_mat", "broadcast_to")
pub fn broadcast_to2(mat: Mat, to_shape: to_shape) -> Mat

@external(erlang, "evision_mat", "broadcast_to")
pub fn broadcast_to3(
  mat: Mat,
  to_shape: to_shape,
  force_src_shape: force_src_shape,
) -> Mat
