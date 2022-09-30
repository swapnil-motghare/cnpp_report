<script>
import Alert from "./Alert";
export default {
  name: "ModalsMain",
  components: {
    alert: Alert,
  },
  data() {
    return {
      modalsOptions: {},
      keys: {},
    };
  },
  computed: {
    seeModals() {
      return Object.keys(this.modalsOptions).length;
    },
    componentsList() {
      return Object.keys(this.$options.components).filter(
        (comp) => !["ModalsMain"].includes(comp)
      );
    },
  },
  methods: {
    shouldIncludeComp(comp) {
      return Object.keys(this.modalsOptions).includes(comp);
    },
    readyListeners() {
      this.$emitter.on("showModal", (options) => {
        if (options && this.componentsList.includes(options.name)) {
          this.modalsOptions[options.name] = options;
          this.$forceUpdate();
        } else {
          alert(
            `Could not find and show the following modal: ${
              options && options.name
            }`
          );
        }
      });
      this.$emitter.on("hideModal", (modalName) => {
        // hides all modals if no modal name is declared
        if (!modalName) {
          this.modalsOptions = {};
        } else {
          delete this.modalsOptions[modalName];
        }
        this.$forceUpdate();
      });
    },
    onContainerClick(comp) {
      if (this.modalsOptions[comp].hardClose) return;

      this.$modals.hide(comp);
    },
  },
  mounted() {
    this.readyListeners();
  },
};
</script>

<template lang="pug">
span
  transition(
    name='fade'
  )
    .modals-main(
      v-if='Object.keys(this.modalsOptions).length'
    )
      template(
        v-for='(comp, idx) in componentsList'
      )
        .modals-container(
          v-if='shouldIncludeComp(comp)'
          @click='onContainerClick(comp)'
          )
            span(
              @click.stop=''
            )
              component(
                :is='comp'
                :options='modalsOptions[comp]'
                )
</template>

<style lang="sass">
@import '$styles/transitions.sass'
@import '$vars'
// @import '$styles/form.sass'
.modals-main
  height: 100vh
  width: 100vw
  position: fixed
  z-index: 950
  .modals-container
    height: 100%
    width: 100%
    background-color: transparentize(grey, .5)
    position: absolute
    > span
      > .modal-main
        background-color: white
        position: absolute
        left: 50%
        top: 50%
        transform: translateY(-50%) translateX(-50%)
        height: auto
        max-height: 100vh
        width: 320px
        border-radius: 10px
        box-shadow: 0 0 15px 0 transparentize(black, .7)
        border: thin grey solid
        overflow-y: auto
        > .modal-container
          > *
            padding: 16px
          > .title
            @extend .font-1-bold !optional
            font-size: 20px
            color: white
            background-color: $darkgrey
          > .content
            padding: 24px
            max-height: calc(100vh - 232px)
            overflow-y: auto
            .description
              font-size: 14px
              margin-bottom: 32px
            .content-title
              font-size: 1.6em
            .input-field
              margin-top: 5px
              $input-font-size: .9em
              > input
                margin-bottom: 8px
                height: 2.6rem
              label
                &.active
                  transform: translateY(-8px) scale(0.8)
          > .action
            background-color: $darkgrey
            color: white
            display: flex
            > .button
              @extend .pro-button !optional
              border-sizing: border-box
              padding: 8px 24px
              background-color: white
              color: black
              text-align: center
              cursor: pointer
              border: white 2px solid
              transition: all .25s
              user-select: none
              margin-right: 8px
              &.minor
                background-color: transparent
                border-color: transparent
              &.cancel
                background-color: transparent
                color: white
</style>
