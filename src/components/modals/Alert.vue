<script>
export default {
  name: "AlertModal",
  props: {
    options: {
      required: true,
    },
  },
};
</script>

<template lang="pug">
#alert-modal.modal-main
  .modal-container
    .title {{options.title || 'ALERT'}}
    .content 
      .copy(
        v-html='options.message'
      )
    .action
      .button(
        @click='$modals.hide()'
      ) Close
</template>

<style lang="sass">
#alert-modal
  .modal-container
    .title
      @extend .font-1-bold !optional
</style>
