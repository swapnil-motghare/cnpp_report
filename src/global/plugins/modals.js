export default {
  install: (app) => {
    app.config.globalProperties.$modals = {
      show(options) {
        app.config.globalProperties.$emitter.emit("showModal", options);
      },
      hide(modalName) {
        app.config.globalProperties.$emitter.emit("hideModal", modalName);
      },
    };
  },
};
