const path = require("path");
module.exports = {
  chainWebpack: (config) => {
    config.resolve.alias
      .set("@", path.resolve("src"))
      .set("$common", path.resolve("src/components/common"))
      .set("$routed", path.resolve("src/components/routed"))
      .set("$modals", path.resolve("src/components/modals"))
      .set("$styles", path.resolve("src/global/styles"))
      .set("$vars", path.resolve("src/global/styles/vars.sass"));
  },
  pwa: {
    name: "My App",
    themeColor: "#4DBA87",
    msTileColor: "#000000",
    appleMobileWebAppCapable: "yes",
    appleMobileWebAppStatusBarStyle: "black",
  },
};
