import * as Vue from "https://cdn.jsdelivr.net/npm/vue@3.2.26/dist/vue.esm-browser.prod.js";
import { ML } from "./ml.js";
import { Zoo } from "./zoo.js";

export async function init(ctx, info) {
  await Promise.all([
    ctx.importCSS("main.css"),
    ctx.importCSS("https://fonts.googleapis.com/css2?family=Inter:wght@400;500&display=swap"),
    ctx.importCSS("https://cdn.jsdelivr.net/npm/remixicon@2.5.0/fonts/remixicon.min.css")
  ]);

  var appConfig = {};
  if (info.id.startsWith("evision.ml") && ML.SmartCell.hasOwnProperty(info.id)) {
    appConfig = ML.SmartCell[info.id](ctx, info);
  } else if (info.id == "evision.zoo") {
    appConfig = Zoo.SmartCell[info.id](ctx, info);
  }

  const app = Vue.createApp(appConfig).mount(ctx.root);

  ctx.handleEvent("update", ({ fields }) => {
    setValues(fields);
  });

  ctx.handleSync(() => {
    // Synchronously invokes change listeners
    document.activeElement &&
      document.activeElement.dispatchEvent(
        new Event("change", { bubbles: true })
      );
  });

  function setValues(fields) {
    for (const field in fields) {
      app.fields[field] = fields[field];
    }
  }
}
