<script>
import axios from 'axios'

import VRuntimeTemplate from 'vue3-runtime-template'


export default {
  name: 'ContentLoader',
  components: {
    VRuntimeTemplate,
  },
  data() {
    return {
      content: '',
      errorMessage: null,
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
  watch: {
    errorMessage(newData, oldData) {
      console.log('watch', { newData, oldData })
    },
  },
  methods: {
    async loadSite() {
      console.log('loadSite')
      this.errorMessage = null

      const path = this.$route.path === '/' ? '/home' : this.$route.path
      const response = await axios.get('/content' + path)
      this.content = response.data
    },
    async login(e) {
      this.errorMessage = null

      const formData = new FormData(e.target)
      const data = Object.fromEntries(formData.entries())

      console.log({data})

      const response = await axios.post(
        '/auth',
        data,
      )
      .catch((error) => {
        if (error.response?.data?.description) {
          this.errorMessage = error.response.data.description

        }

      })

      if (response?.data?.token) {
        console.log('hmmmmm', response)
        if (response.status === 200) {
          window.localStorage.setItem('fmexp_jwt_token', response.data.token)
          window.location.href = '/'

        }

      }

    },
  }
}
</script>

<template>
  <v-runtime-template :template="this.content"></v-runtime-template>
</template>
