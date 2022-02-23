<script>
import axios from 'axios'


export default {
  name: 'ContentLoader',
  data() {
    return {
      content: '',
    }
  },
  created() {
    this.$watch(
      () => this.$route.params,
      () => {
        this.loadSite()
      },
      { immediate: true },
    )
  },
  methods: {
    async loadSite() {
      console.log('this.$route.path', this.$route.path)
      const path = this.$route.path === '/' ? '/home' : this.$route.path
      const response = await axios.get('/content' + path)
      this.content = response.data
    }
  }
}
</script>

<template>
  <div v-html="content"></div>
</template>
