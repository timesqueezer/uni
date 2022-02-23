<script>
  import Header from './components/header'

  import InputRecorder from './input-recorder'

  export default {
    components: {
      Header,
    },
    data() {
      return {
        loading: true,
        inputRecorder: null,
      }
    },
    async created() {
      this.inputRecorder = new InputRecorder()
      await this.inputRecorder.init()

      this.loading = false

    },
    methods: {
    }
  }
</script>

<template>
  <Header></Header>

  <main class="container flex-shrink-0 pt-4">
    <div v-if="loading">
      Loading
    </div>
    <div v-else>
      <router-view></router-view>
    </div>
  </main>

  <footer class="mt-auto bg-light py-3">
    <div class="container">
      <span>&copy; 2022 Matz Radloff</span>
    </div>
  </footer>
</template>
