<template>
  <div class="popup-overlay" v-if="showPopup">
    <div class="popup">
      <p>{{ message }}</p>
      <button @click="closePopup">Close</button>
    </div>
  </div>
</template>

<script>
export default {
  props: {
    showPopup: Boolean,
    message: String,
    success: Boolean,
  },

  methods: {
    closePopup() {
      this.$emit('close')
    },
  },
}
</script>

<style>
.popup-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.5);
  display: flex;
  justify-content: center;
  align-items: center;
}

.popup {
  background-color: white;
  padding: 20px;
  border-radius: 8px;
  box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);
}
</style>
