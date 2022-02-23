<script>
  import Header from './components/header'

  export default {
    components: {
      Header,
    },
    data() {
      return {
      }
    },
    mounted() {
    },
    methods: {
    }
  }
</script>

<template>
  <Header></Header>

  <main class="container flex-shrink-0 pt-4">
    <router-view></router-view>
  </main>

  <footer class="mt-auto bg-light py-3">
    <div class="container">
      <span>&copy; 2022 Matz Radloff</span>
    </div>
  </footer>

</template>
