defmodule Evision.DescriptorMatcher.MatcherType do
  @type enum :: integer()
  @doc enum: true
  def cv_FLANNBASED, do: 1
  @doc enum: true
  def cv_BRUTEFORCE, do: 2
  @doc enum: true
  def cv_BRUTEFORCE_L1, do: 3
  @doc enum: true
  def cv_BRUTEFORCE_HAMMING, do: 4
  @doc enum: true
  def cv_BRUTEFORCE_HAMMINGLUT, do: 5
  @doc enum: true
  def cv_BRUTEFORCE_SL2, do: 6
end
