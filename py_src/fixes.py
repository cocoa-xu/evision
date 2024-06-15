#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def evision_elixir_fixes(): 
    return [
        """
  @spec imdecode(binary(), integer()) :: Evision.Mat.maybe_mat_out()
  def imdecode(buf, flags) when is_integer(flags)
  do
      positional = [
          buf: buf,
          flags: flags
      ]
      :evision_nif.imdecode(positional)
      |> Evision.Internal.Structurise.to_struct()
  end
"""
    ]


def evision_erlang_fixes():
    return [
        """
imdecode(Buf, Flags) ->
  Ret = evision_nif:imdecode([{buf, Buf}, {flags, Flags}]),
  evision_internal_structurise:to_struct(Ret).
"""
    ]
def evision_erlang_fixes_gleam_typed():
    return [
        """
@external(erlang, "evision", "imdecode")
pub fn imdecode(buf: BitArray, flags: Int) -> Mat
"""
    ]

def evision_elixir_module_fixes(): 
    return {
      "VideoCapture": [
        ("""
  @doc \"\"\"
  Wait for ready frames from VideoCapture.

  ##### Positional Arguments
  - **streams**: `[Evision.VideoCapture]`.

    input video streams

  ##### Keyword Arguments
  - **timeoutNs**: `int64`.

    number of nanoseconds (0 - infinite)

  ##### Return
  - **retval**: `bool`
  - **readyIndex**: `[int]`.

    stream indexes with grabbed frames (ready to use .retrieve() to fetch actual frame)

  @return `true` if streamReady is not empty
  @throws Exception %Exception on stream errors (check .isOpened() to filter out malformed streams) or VideoCapture type is not supported
  The primary use of the function is in multi-camera environments.
  The method fills the ready state vector, grabs video frame, if camera is ready.
  After this call use VideoCapture::retrieve() to decode and fetch frame data.

  Python prototype (for reference only):
  ```python3
  waitAny(streams[, timeoutNs]) -> retval, readyIndex
  ```
  \"\"\"
  @spec waitAny(list(Evision.VideoCapture.t()), [{:timeoutNs, term()}] | nil) :: list(integer()) | false | {:error, String.t()}
  def waitAny(streams, opts) when is_list(streams) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:timeoutNs])
    positional = [
      streams: Evision.Internal.Structurise.from_struct(streams)
    ]
    :evision_nif.videoCapture_waitAny(positional ++ Evision.Internal.Structurise.from_struct(opts))
     |> to_struct()
  end

  @doc \"\"\"
  Wait for ready frames from VideoCapture.

  ##### Positional Arguments
  - **streams**: `[Evision.VideoCapture]`.

    input video streams

  ##### Keyword Arguments
  - **timeoutNs**: `int64`.

    number of nanoseconds (0 - infinite)

  ##### Return
  - **retval**: `bool`
  - **readyIndex**: `[int]`.

    stream indexes with grabbed frames (ready to use .retrieve() to fetch actual frame)

  @return `true` if streamReady is not empty
  @throws Exception %Exception on stream errors (check .isOpened() to filter out malformed streams) or VideoCapture type is not supported
  The primary use of the function is in multi-camera environments.
  The method fills the ready state vector, grabs video frame, if camera is ready.
  After this call use VideoCapture::retrieve() to decode and fetch frame data.

  Python prototype (for reference only):
  ```python3
  waitAny(streams[, timeoutNs]) -> retval, readyIndex
  ```
  \"\"\"
  @spec waitAny(list(Evision.VideoCapture.t())) :: list(integer()) | false | {:error, String.t()}
  def waitAny(streams) when is_list(streams)
  do
    positional = [
      streams: Evision.Internal.Structurise.from_struct(streams)
    ]
    :evision_nif.videoCapture_waitAny(positional)
    |> to_struct()
  end
  """, "")],
      "DNN": [
        ("""
  @doc \"\"\"
  Performs non maximum suppression given boxes and corresponding scores.

  ##### Positional Arguments
  - **bboxes**: `[Rect2d]`, `Nx.Tensor.t()`, `Evision.Mat.t()`.

      a set of bounding boxes to apply NMS.

  - **scores**: `[float]`.

      a set of corresponding confidences.

  - **score_threshold**: `float`.

      a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

      a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **eta**: `float`.

      a coefficient in adaptive threshold formula: \\f$nms\\_threshold_{i+1}=eta\\cdot nms\\_threshold_i\\f$.

  - **top_k**: `int`.

      if `>0`, keep at most @p top_k picked indices.

  ##### Return
  - **indices**: `[int]`.

      the kept indices of bboxes after NMS.

  Python prototype (for reference only):
  ```python3
  NMSBoxes(bboxes, scores, score_threshold, nms_threshold[, eta[, top_k]]) -> indices
  ```
  \"\"\"
  @spec nmsBoxes(list({number(), number(), number(), number()}) | Evision.Mat.t() | Nx.Tensor.t(), list(number()), number(), number(), [{:eta, term()} | {:top_k, term()}] | nil) :: list(integer()) | {:error, String.t()}
  def nmsBoxes(bboxes, scores, score_threshold, nms_threshold, opts) when is_list(bboxes) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:eta, :top_k])
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_NMSBoxes(positional ++ Evision.Internal.Structurise.from_struct(opts))
    |> to_struct()
  end

  def nmsBoxes(bboxes, scores, score_threshold, nms_threshold, opts) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    case bboxes.shape do
      {_, 4} ->
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_NMSBoxes(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end

  @doc \"\"\"
  Performs non maximum suppression given boxes and corresponding scores.

  ##### Positional Arguments
  - **bboxes**: `[Rect2d]`, `Nx.Tensor.t()`, `Evision.Mat.t()`..

      a set of bounding boxes to apply NMS.

  - **scores**: `[float]`.

      a set of corresponding confidences.

  - **score_threshold**: `float`.

      a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

      a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **eta**: `float`.

      a coefficient in adaptive threshold formula: \\f$nms\\_threshold_{i+1}=eta\\cdot nms\\_threshold_i\\f$.

  - **top_k**: `int`.

      if `>0`, keep at most @p top_k picked indices.

  ##### Return
  - **indices**: `[int]`.

      the kept indices of bboxes after NMS.

  Python prototype (for reference only):
  ```python3
  NMSBoxes(bboxes, scores, score_threshold, nms_threshold[, eta[, top_k]]) -> indices
  ```
  \"\"\"
  @spec nmsBoxes(list({number(), number(), number(), number()}) | Evision.Mat.t() | Nx.Tensor.t(), list(number()), number(), number()) :: list(integer()) | {:error, String.t()}
  def nmsBoxes(bboxes, scores, score_threshold, nms_threshold) when is_list(bboxes) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold)
  do
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_NMSBoxes(positional)
    |> to_struct()
  end

  def nmsBoxes(bboxes, scores, score_threshold, nms_threshold) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold)
  do
    case bboxes.shape do
      {_, 4} ->
        bboxes = case bboxes.__struct__ do
          Nx.Tensor ->
            Nx.as_type(bboxes, :f64)
          Evision.Mat ->
            Evision.Mat.as_type(bboxes, :f64)
          _ ->
            raise "Invalid struct, expecting Nx.Tensor or Evision.Mat"
        end
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_NMSBoxes(positional)
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end

  @doc \"\"\"
  Performs batched non maximum suppression on given boxes and corresponding scores across different classes.

  ##### Positional Arguments
  - **bboxes**: `[Rect2d]`, `Nx.Tensor.t()`, `Evision.Mat.t()`.

    a set of bounding boxes to apply NMS.

  - **scores**: `[float]`.

    a set of corresponding confidences.

  - **class_ids**: `[int]`.

    a set of corresponding class ids. Ids are integer and usually start from 0.

  - **score_threshold**: `float`.

    a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

    a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **eta**: `float`.

    a coefficient in adaptive threshold formula: \\f$nms\\_threshold_{i+1}=eta\\cdot nms\\_threshold_i\\f$.

  - **top_k**: `int`.

    if `>0`, keep at most @p top_k picked indices.

  ##### Return
  - **indices**: `[int]`.

    the kept indices of bboxes after NMS.

  Python prototype (for reference only):
  ```python3
  NMSBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold[, eta[, top_k]]) -> indices
  ```
  \"\"\"
  @spec nmsBoxesBatched(list({number(), number(), number(), number()}), list(number()), list(integer()), number(), number(), [{:eta, term()} | {:top_k, term()}] | nil) :: list(integer()) | {:error, String.t()}
  def nmsBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold, opts) when is_list(bboxes) and is_list(scores) and is_list(class_ids) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:eta, :top_k])
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      class_ids: Evision.Internal.Structurise.from_struct(class_ids),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_NMSBoxesBatched(positional ++ Evision.Internal.Structurise.from_struct(opts))
     |> to_struct()
  end

  def nmsBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold, opts) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_list(class_ids) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:eta, :top_k])
    case bboxes.shape do
      {_, 4} ->
        bboxes = case bboxes.__struct__ do
          Nx.Tensor ->
              Nx.as_type(bboxes, :f64)
          Evision.Mat ->
              Evision.Mat.as_type(bboxes, :f64)
          _ ->
              raise "Invalid struct, expecting Nx.Tensor or Evision.Mat"
        end
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          class_ids: Evision.Internal.Structurise.from_struct(class_ids),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_NMSBoxesBatched(positional ++ Evision.Internal.Structurise.from_struct(opts))
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end

  @doc \"\"\"
  Performs batched non maximum suppression on given boxes and corresponding scores across different classes.

  ##### Positional Arguments
  - **bboxes**: `[Rect2d]`, `Nx.Tensor.t()`, `Evision.Mat.t()`.

    a set of bounding boxes to apply NMS.

  - **scores**: `[float]`.

    a set of corresponding confidences.

  - **class_ids**: `[int]`.

    a set of corresponding class ids. Ids are integer and usually start from 0.

  - **score_threshold**: `float`.

    a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

    a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **eta**: `float`.

    a coefficient in adaptive threshold formula: \\f$nms\\_threshold_{i+1}=eta\\cdot nms\\_threshold_i\\f$.

  - **top_k**: `int`.

    if `>0`, keep at most @p top_k picked indices.

  ##### Return
  - **indices**: `[int]`.

    the kept indices of bboxes after NMS.

  Python prototype (for reference only):
  ```python3
  NMSBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold[, eta[, top_k]]) -> indices
  ```
  \"\"\"
  @spec nmsBoxesBatched(list({number(), number(), number(), number()}), list(number()), list(integer()), number(), number()) :: list(integer()) | {:error, String.t()}
  def nmsBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold) when is_list(bboxes) and is_list(scores) and is_list(class_ids) and is_float(score_threshold) and is_float(nms_threshold)
  do
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      class_ids: Evision.Internal.Structurise.from_struct(class_ids),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_NMSBoxesBatched(positional)
    |> to_struct()
  end

  def nmsBoxesBatched(bboxes, scores, class_ids, score_threshold, nms_threshold) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_list(class_ids) and is_float(score_threshold) and is_float(nms_threshold)
  do
    case bboxes.shape do
      {_, 4} ->
        bboxes = case bboxes.__struct__ do
          Nx.Tensor ->
              Nx.as_type(bboxes, :f64)
          Evision.Mat ->
              Evision.Mat.as_type(bboxes, :f64)
          _ ->
              raise "Invalid struct, expecting Nx.Tensor or Evision.Mat"
        end
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          class_ids: Evision.Internal.Structurise.from_struct(class_ids),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_NMSBoxesBatched(positional)
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end

  @doc \"\"\"
  Performs soft non maximum suppression given boxes and corresponding scores.
  Reference: https://arxiv.org/abs/1704.04503

  ##### Positional Arguments
  - **bboxes**: `[Rect]`, `Nx.Tensor.t()`, `Evision.Mat.t()`..

    a set of bounding boxes to apply Soft NMS.

  - **scores**: `[float]`.

    a set of corresponding confidences.

  - **score_threshold**: `float`.

    a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

    a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **top_k**: `size_t`.

    keep at most @p top_k picked indices.

  - **sigma**: `float`.

    parameter of Gaussian weighting.

  - **method**: `SoftNMSMethod`.

    Gaussian or linear.

  ##### Return
  - **updated_scores**: `[float]`.

    a set of corresponding updated confidences.

  - **indices**: `[int]`.

    the kept indices of bboxes after NMS.

  @see SoftNMSMethod

  Python prototype (for reference only):
  ```python3
  softNMSBoxes(bboxes, scores, score_threshold, nms_threshold[, top_k[, sigma[, method]]]) -> updated_scores, indices
  ```
  \"\"\"
  @spec softNMSBoxes(list({number(), number(), number(), number()}), list(number()), number(), number(), [{:top_k, term()} | {:sigma, term()} | {:method, term()}] | nil) :: {list(number()), list(integer())} | {:error, String.t()}
  def softNMSBoxes(bboxes, scores, score_threshold, nms_threshold, opts) when is_list(bboxes) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:top_k, :sigma, :method])
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_softNMSBoxes(positional ++ Evision.Internal.Structurise.from_struct(opts))
     |> to_struct()
  end

  def softNMSBoxes(bboxes, scores, score_threshold, nms_threshold, opts) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:top_k, :sigma, :method])
    case bboxes.shape do
      {_, 4} ->
        bboxes = case bboxes.__struct__ do
          Nx.Tensor ->
              Nx.as_type(bboxes, :s32)
          Evision.Mat ->
              Evision.Mat.as_type(bboxes, :s32)
          _ ->
              raise "Invalid struct, expecting Nx.Tensor or Evision.Mat"
        end
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_softNMSBoxes(positional ++ Evision.Internal.Structurise.from_struct(opts))
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end

  @doc \"\"\"
  Performs soft non maximum suppression given boxes and corresponding scores.
  Reference: https://arxiv.org/abs/1704.04503

  ##### Positional Arguments
  - **bboxes**: `[Rect]`, `Nx.Tensor.t()`, `Evision.Mat.t()`..

    a set of bounding boxes to apply Soft NMS.

  - **scores**: `[float]`.

    a set of corresponding confidences.

  - **score_threshold**: `float`.

    a threshold used to filter boxes by score.

  - **nms_threshold**: `float`.

    a threshold used in non maximum suppression.

  ##### Keyword Arguments
  - **top_k**: `size_t`.

    keep at most @p top_k picked indices.

  - **sigma**: `float`.

    parameter of Gaussian weighting.

  - **method**: `SoftNMSMethod`.

    Gaussian or linear.

  ##### Return
  - **updated_scores**: `[float]`.

    a set of corresponding updated confidences.

  - **indices**: `[int]`.

    the kept indices of bboxes after NMS.

  @see SoftNMSMethod

  Python prototype (for reference only):
  ```python3
  softNMSBoxes(bboxes, scores, score_threshold, nms_threshold[, top_k[, sigma[, method]]]) -> updated_scores, indices
  ```
  \"\"\"
  @spec softNMSBoxes(list({number(), number(), number(), number()}), list(number()), number(), number()) :: {list(number()), list(integer())} | {:error, String.t()}
  def softNMSBoxes(bboxes, scores, score_threshold, nms_threshold) when is_list(bboxes) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold)
  do
    positional = [
      bboxes: Evision.Internal.Structurise.from_struct(bboxes),
      scores: Evision.Internal.Structurise.from_struct(scores),
      score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
      nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
    ]
    :evision_nif.dnn_softNMSBoxes(positional)
    |> to_struct()
  end

  def softNMSBoxes(bboxes, scores, score_threshold, nms_threshold) when (is_struct(bboxes, Evision.Mat) or is_struct(bboxes, Nx.Tensor)) and is_list(scores) and is_float(score_threshold) and is_float(nms_threshold)
  do
    case bboxes.shape do
      {_, 4} ->
        bboxes = case bboxes.__struct__ do
          Nx.Tensor ->
              Nx.as_type(bboxes, :s32)
          Evision.Mat ->
              Evision.Mat.as_type(bboxes, :s32)
          _ ->
              raise "Invalid struct, expecting Nx.Tensor or Evision.Mat"
        end
        positional = [
          bboxes: Evision.Mat.to_binary(bboxes),
          scores: Evision.Internal.Structurise.from_struct(scores),
          score_threshold: Evision.Internal.Structurise.from_struct(score_threshold),
          nms_threshold: Evision.Internal.Structurise.from_struct(nms_threshold)
        ]
        :evision_nif.dnn_softNMSBoxes(positional)
        |> to_struct()
      invalid_shape ->
        {:error, "Expected a tensor or a mat with shape `{n, 4}`, got `#{inspect(invalid_shape)}`"}
    end
  end
""", """
  def dnn_NMSBoxes(_opts \\\\ []), do: :erlang.nif_error(:undefined)
  def dnn_NMSBoxesBatched(_opts \\\\ []), do: :erlang.nif_error(:undefined)
  def dnn_softNMSBoxes(_opts \\\\ []), do: :erlang.nif_error(:undefined)
""")
    ]}

def evision_erlang_module_fixes():
    return {"DNN": [
        ("""
-spec nmsBoxes(list({number(), number(), number(), number()}), list(number()), number(), number(), [{atom(), term()},...] | nil) -> list(integer()) | {error, binary()}.
nmsBoxes(Bboxes, Scores, Score_threshold, Nms_threshold, Options) when is_list(Bboxes), is_list(Scores), is_float(Score_threshold), is_float(Nms_threshold), is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_NMSBoxes(Positional ++ evision_internal_structurise:from_struct(Options)),
  to_struct(Ret).

-spec nmsBoxes(list({number(), number(), number(), number()}), list(number()), number(), number()) -> list(integer()) | {error, binary()}.
nmsBoxes(Bboxes, Scores, Score_threshold, Nms_threshold) when is_list(Bboxes), is_list(Scores), is_float(Score_threshold), is_float(Nms_threshold)->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_NMSBoxes(Positional),
  to_struct(Ret).

-spec nmsBoxesBatched(list({number(), number(), number(), number()}), list(number()), list(integer()), number(), number(), [{atom(), term()},...] | nil) -> list(integer()) | {error, binary()}.
nmsBoxesBatched(Bboxes, Scores, Class_ids, Score_threshold, Nms_threshold, Options) when is_list(Bboxes), is_list(Scores), is_list(Class_ids), is_float(Score_threshold), is_float(Nms_threshold), is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {class_ids, Class_ids},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_NMSBoxesBatched(Positional ++ evision_internal_structurise:from_struct(Options)),
  to_struct(Ret).

-spec nmsBoxesBatched(list({number(), number(), number(), number()}), list(number()), list(integer()), number(), number()) -> list(integer()) | {error, binary()}.
nmsBoxesBatched(Bboxes, Scores, Class_ids, Score_threshold, Nms_threshold) when is_list(Bboxes), is_list(Scores), is_list(Class_ids), is_float(Score_threshold), is_float(Nms_threshold)->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {class_ids, Class_ids},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_NMSBoxesBatched(Positional),
  to_struct(Ret).

-spec softNMSBoxes(list({number(), number(), number(), number()}), list(number()), number(), number(), [{atom(), term()},...] | nil) -> {list(number()), list(integer())} | {error, binary()}.
softNMSBoxes(Bboxes, Scores, Score_threshold, Nms_threshold, Options) when is_list(Bboxes), is_list(Scores), is_float(Score_threshold), is_float(Nms_threshold), is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_softNMSBoxes(Positional ++ evision_internal_structurise:from_struct(Options)),
  to_struct(Ret).

-spec softNMSBoxes(list({number(), number(), number(), number()}), list(number()), number(), number()) -> {list(number()), list(integer())} | {error, binary()}.
softNMSBoxes(Bboxes, Scores, Score_threshold, Nms_threshold) when is_list(Bboxes), is_list(Scores), is_float(Score_threshold), is_float(Nms_threshold)->
  Positional = [
    {bboxes, Bboxes},
    {scores, Scores},
    {score_threshold, Score_threshold},
    {nms_threshold, Nms_threshold}
  ],
  Ret = evision_nif:dnn_softNMSBoxes(Positional),
  to_struct(Ret).
""", """
dnn_NMSBoxes(_opts) ->
  not_loaded(?LINE).
dnn_NMSBoxesBatched(_opts) ->
  not_loaded(?LINE).
dnn_softNMSBoxes(_opts) ->
  not_loaded(?LINE).
""")
    ]}

def evision_gleam_module_fixes():
    return {"DNN": [
"""
@external(erlang, "evision_dnn", "nmsBoxes")
pub fn nms_boxes5(bboxes: boxes, scores: scores, score_threshold: score_threshold, nms_threshold: nms_threshold, options: options) -> any

@external(erlang, "evision_dnn", "nmsBoxes")
pub fn nms_boxes4(bboxes: boxes, scores: scores, score_threshold: score_threshold, nms_threshold: nms_threshold) -> any

@external(erlang, "evision_dnn", "nmsBoxesBatched")
pub fn nms_boxes_batched6(bboxes: boxes, scores: scores, class_ids: class_ids, score_threshold: score_threshold, nms_threshold: nms_threshold, options: options) -> any

@external(erlang, "evision_dnn", "nmsBoxesBatched")
pub fn nms_boxes_batched5(bboxes: boxes, scores: scores, class_ids: class_ids, score_threshold: score_threshold, nms_threshold: nms_threshold) -> any

@external(erlang, "evision_dnn", "softNMSBoxes")
pub fn soft_nms_boxes5(bboxes: boxes, scores: scores, score_threshold: score_threshold, nms_threshold: nms_threshold, options: options) -> any

@external(erlang, "evision_dnn", "softNMSBoxes")
pub fn soft_nms_boxes4(bboxes: boxes, scores: scores, score_threshold: score_threshold, nms_threshold: nms_threshold) -> any
"""
    ]}
