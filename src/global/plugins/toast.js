export default {
  install: (app) => {
    app.config.globalProperties.$toast = (toast) => {
      app.config.globalProperties.$emitter.emit("toast", toast);
    };
  },
};
