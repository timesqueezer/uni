<script>
import axios from 'axios'

// import { RuntimeTemplateCompiler } from 'vue-runtime-template-compiler'


export default {
  name: 'ContentLoader',
  components: {
    // RuntimeTemplateCompiler,
  },
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
    },
    test() {
      console.log('hmmmm')
    },
  }
}
</script>

<template>
  <!--<RuntimeTemplateCompiler :template="html"></RuntimeTemplateCompiler>-->
  <div v-html="this.content"></div>
</template>
