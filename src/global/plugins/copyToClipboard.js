export default {
  install: (app) => {
    app.config.globalProperties.$copyToClipboard = function (text) {
      navigator.clipboard.writeText(text).then(function () {
        console.log('Successfully copied to clipboard')
      }, function() {
        console.error('Failed to copy text to clipboard')
      })
    };
  },
};
