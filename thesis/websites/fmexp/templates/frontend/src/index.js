import './scss/custom.scss'

import { createApp } from 'vue'

import main from './main'
import router from './router'

const app = createApp(main)
app.use(router)

app.mount('#app')

app.config.errorHandler = (err) => {
  console.error(err)
}
