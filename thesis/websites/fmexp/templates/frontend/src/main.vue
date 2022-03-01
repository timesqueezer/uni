<script>
  import axios from 'axios'
  import Cookies from 'js-cookie'

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

        currentUser: null,
      }
    },
    async created() {
      this.inputRecorder = new InputRecorder()
      await this.inputRecorder.init()

      if (window.localStorage.getItem('fmexp_jwt_token')) {
        axios.defaults.headers.common.Authorization = 'JWT ' + window.localStorage.getItem('fmexp_jwt_token')
        const currentUserResponse = await axios.get('/user')
        .catch((error) => {
          // reset on error, which "should" never happen
          window.localStorage.removeItem('fmexp_jwt_token')
          delete axios.defaults.headers.common.Authorization
          Cookies.remove('user_uuid')

          location.href = '/'

        })

        if (currentUserResponse) {
          this.currentUser = currentUserResponse.data

        }

      }

      console.log('main', this.currentUser)

      this.loading = false

    },
    methods: {
      setUser(user) {
        this.currentUser = user
      }
    }
  }
</script>

<template>
  <Header :current-user="currentUser"></Header>

  <main class="container flex-shrink-0 pt-4">
    <div v-if="loading">
      Loading
    </div>
    <div v-else>
      <router-view
        :user="currentUser"
        @set-user="setUser"
      ></router-view>
    </div>
  </main>

  <footer class="mt-auto bg-light py-3">
    <div class="container">
      <span>&copy; 2022 Matz Radloff</span>
    </div>
  </footer>
</template>
