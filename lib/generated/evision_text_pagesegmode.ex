defmodule Evision.Text.PageSegMode do
  @type enum :: integer()
  @doc enum: true
  def cv_PSM_OSD_ONLY, do: 0
  @doc enum: true
  def cv_PSM_AUTO_OSD, do: 1
  @doc enum: true
  def cv_PSM_AUTO_ONLY, do: 2
  @doc enum: true
  def cv_PSM_AUTO, do: 3
  @doc enum: true
  def cv_PSM_SINGLE_COLUMN, do: 4
  @doc enum: true
  def cv_PSM_SINGLE_BLOCK_VERT_TEXT, do: 5
  @doc enum: true
  def cv_PSM_SINGLE_BLOCK, do: 6
  @doc enum: true
  def cv_PSM_SINGLE_LINE, do: 7
  @doc enum: true
  def cv_PSM_SINGLE_WORD, do: 8
  @doc enum: true
  def cv_PSM_CIRCLE_WORD, do: 9
  @doc enum: true
  def cv_PSM_SINGLE_CHAR, do: 10
end
