<script>
import Toast from "$common/Toast";
import ModalsMain from "$modals/Main";

export default {
  name: "App",
  components: {
    toast: Toast,
    "modals-main": ModalsMain,
  },
};
</script>

<template lang="pug">
#nav
  router-link(
    to="/"
  ) Home
  router-link(
    to="/about"
  ) About
modals-main
router-view
toast
</template>

<style lang="sass">
#app
  font-family: Avenir, Helvetica, Arial, sans-serif
  -webkit-font-smoothing: antialiased
  -moz-osx-font-smoothing: grayscale
  text-align: center
  color: #2c3e50

#nav
  padding: 30px
  a
    font-weight: bold
    color: #2c3e50
    &.router-link-exact-active
      color: #42b983
</style>
