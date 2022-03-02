<script>

import Cookies from 'js-cookie'


export default {
  name: 'Header',
  props: {
    currentUser: { type: Object, required: false, default: null},
  },
  data() {
    return {
    }
  },
  mounted() {},
  methods: {
    logout() {
      window.localStorage.clear()
      Cookies.remove('user_uuid')

      location.href = '/'
    },
  },
}
</script>

<template>
  <nav class="navbar navbar-expand-lg navbar-light bg-light shadow-sm">
    <div class="container">
      <router-link class="navbar-brand" to="/" active-class="active">FM Exp 1</router-link>

      <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarsExample10" aria-controls="navbarsExample10" aria-expanded="false" aria-label="Toggle navigation">
        <span class="navbar-toggler-icon"></span>
      </button>

      <div class="collapse navbar-collapse justify-content-md-center" id="navbarsExample10">
        <ul class="navbar-nav">
          <li class="nav-item">
            <router-link class="nav-link" to="/" active-class="active">About</router-link>
          </li>
          <li class="nav-item">
            <router-link class="nav-link" to="/blog" active-class="active">Blog</router-link>
          </li>
          <li class="nav-item">
            <router-link class="nav-link" to="/contact" active-class="active">Contact / Imprint</router-link>
          </li>
        </ul>

        <div class="flex-grow-1"></div>

        <div v-if="currentUser" class="d-flex">
          <router-link class="nav-link me-3" to="/profile">
            {{ currentUser.email }}
          </router-link>
          <button class="btn btn-outline-secondary" @click="logout">
            Logout
          </button>
        </div>
        <div v-else class="d-flex">
          <router-link class="btn btn-outline-secondary me-3" to="/login" active-class="active">Login</router-link>
          <router-link class="btn btn-outline-secondary" to="/register" active-class="active">Register</router-link>
        </div>
      </div>
    </div>
  </nav>
</template>
