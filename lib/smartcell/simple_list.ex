if !Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.SimpleList do
  end
else
  defmodule Evision.SmartCell.SimpleList do
    # code taken from
    # https://github.com/livebook-dev/kino_bumblebee/blob/main/lib/kino/bumblebee/scored_list.ex

    @moduledoc """
    A kino for displaying a list of labels.
    This kino is primarily used to present top classification predictions.
    ## Examples
        predictions =  [
          "malamute",
          "Siberian husky",
          "Eskimo dog",
          "Tibetan mastiff",
          "German shepherd"
        ]
        Evision.SmartCell.SimpleList.new(predictions)
    """

    use Kino.JS

    @type t :: Kino.JS.t()

    @doc """
    Creates a new kino displaying the given list of items.
    Expects a list of tuples, each element being the label and its score.
    """
    @spec new(list({String.t(), number()})) :: t()
    def new(items) when is_list(items) do
      Kino.JS.new(__MODULE__, items)
    end

    asset "main.js" do
      """
      export function init(ctx, items) {
        ctx.importCSS("main.css");
        ctx.importCSS(
          "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600&display=swap"
        );
        ctx.root.innerHTML = `
          <div class="app">
            <ol class="list">
              ${items.map((item) => (`
                <li class="item">
                  <div class="label">
                    <div>${item}</div>
                  </div>
                </li>
              `)).join("")}
            </ol>
          </div>
        `;
      }
      """
    end

    asset "main.css" do
      """
      .app {
        font-family: "JetBrains Mono", monospace;
        font-size: 14px;
        color: #8BA2FF;
      }
      .list {
        padding: 0;
        margin: 0;
      }
      .item {
        display: flex;
        justify-content: space-between;
      }
      .item:not(:first-child) {
        margin-top: 16px;
      }
      .label {
        flex-grow: 1;
        margin-right: 8px;
      }
      """
    end
  end
end
