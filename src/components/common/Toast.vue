<script>
export default {
  name: "Toast",
  data() {
    return {
      testIf: false,
      toasts: [],
    };
  },
  methods: {
    readyListener() {
      this.$emitter.on("toast", (options) => {
        let toast = options;
        if (typeof options === "string" || !options) {
          toast = {
            copy: options,
            time: 2000,
          };
        }
        toast.id = String(Math.random());
        this.onToastAdd(toast);
      });
    },
    onToastAdd(toast) {
      this.toasts.push(toast);
      setTimeout(() => {
        const toastIdx = this.toasts.findIndex(
          (aToast) => aToast.id === toast.id
        );
        this.toasts.splice(toastIdx, 1);
      }, toast.time || 2500);
    },
  },
  mounted() {
    this.readyListener();
  },
};
</script>

<template lang="pug">
.toast-main
  .toast-container
    transition-group(
      name='from-bottom'
      mode='out-in'
    )
      .toast(
        v-for='toast in toasts'
        :key='toast.id'
      ) {{toast.copy}}
</template>

<style lang="sass" scoped>
@import '$styles/transitions.sass'
.toast-main
  position: fixed
  bottom: 20px
  max-width: 100%
  width: fit-content
  min-width: 110px
  left: 50%
  transform: translateX(-50%)
  transition: .25s all
  z-index: 951
  .toast-container
    position: relative
    display: grid
    align-items: end
    .toast
      @extend .font-1-bold !optional
      background-color: black
      color: white
      margin-top: 15px
      padding: 5px 15px
      transition: all .25s
      border-radius: 8px
</style>
