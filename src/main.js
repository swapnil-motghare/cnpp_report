import { createApp } from "vue";
import App from "./App.vue";
import mitt from "mitt";
import router from "./router";

const emitter = mitt();
const app = createApp(App);

app.use(router);

app.config.globalProperties.$emitter = emitter;

import toast from "./global/plugins/toast";
app.use(toast);

import regex from "./global/plugins/regex";
app.use(regex);

import time from "./global/plugins/time";
app.use(time);

import copyToClipboard from "./global/plugins/copyToClipboard";
app.use(copyToClipboard);

import modals from "./global/plugins/modals";
app.use(modals);

app.mount("#app");
