import('./scss/' + FMEXP_LAYOUT + '.scss')

import * as Vue from 'vue'

import main from './main'
import router from './router'


const app = Vue.createApp(main)
app.use(router)

app.mount('#app')

app.config.errorHandler = (err) => {
  console.error(err)
}
