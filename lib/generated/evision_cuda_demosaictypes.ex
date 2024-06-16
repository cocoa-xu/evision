defmodule Evision.CUDA.DemosaicTypes do
  @type enum :: integer()
  @doc enum: true
  def cv_COLOR_BayerBG2BGR_MHT, do: 256
  @doc enum: true
  def cv_COLOR_BayerGB2BGR_MHT, do: 257
  @doc enum: true
  def cv_COLOR_BayerRG2BGR_MHT, do: 258
  @doc enum: true
  def cv_COLOR_BayerGR2BGR_MHT, do: 259
  @doc enum: true
  def cv_COLOR_BayerBG2RGB_MHT, do: cv_COLOR_BayerRG2BGR_MHT()
  @doc enum: true
  def cv_COLOR_BayerGB2RGB_MHT, do: cv_COLOR_BayerGR2BGR_MHT()
  @doc enum: true
  def cv_COLOR_BayerRG2RGB_MHT, do: cv_COLOR_BayerBG2BGR_MHT()
  @doc enum: true
  def cv_COLOR_BayerGR2RGB_MHT, do: cv_COLOR_BayerGB2BGR_MHT()
  @doc enum: true
  def cv_COLOR_BayerBG2GRAY_MHT, do: 260
  @doc enum: true
  def cv_COLOR_BayerGB2GRAY_MHT, do: 261
  @doc enum: true
  def cv_COLOR_BayerRG2GRAY_MHT, do: 262
  @doc enum: true
  def cv_COLOR_BayerGR2GRAY_MHT, do: 263
end
