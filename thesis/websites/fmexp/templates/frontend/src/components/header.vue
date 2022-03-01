<script>
export default {
  name: 'Header',
  props: {
    currentUser: { type: Object, required: false, default: null},
  },
  data() {
    return {
    }
  },
  mounted() {
  }
}
</script>

<template>
  <nav class="navbar navbar-expand-lg navbar-light bg-light shadow-sm">
    <div class="container">
      <router-link class="navbar-brand" to="/">FM Exp 1</router-link>

      <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarsExample10" aria-controls="navbarsExample10" aria-expanded="false" aria-label="Toggle navigation">
        <span class="navbar-toggler-icon"></span>
      </button>

      <div class="collapse navbar-collapse justify-content-md-center" id="navbarsExample10">
        <ul class="navbar-nav">
          <li class="nav-item">
            <router-link class="nav-link" to="/blog">Blog</router-link>
          </li>
          <li class="nav-item">
            
          </li>
          <li class="nav-item">
            
          </li>
        </ul>

        <div class="flex-grow-1"></div>

        <div v-if="currentUser">
          <router-link class="nav-link" to="/profile">
            {{ currentUser.email }}
          </router-link>
        </div>
        <div v-else>
          <router-link class="btn btn-outline-secondary me-3" to="/login">Login</router-link>
          <router-link class="btn btn-outline-secondary" to="/register">Register</router-link>
        </div>
      </div>
    </div>
  </nav>
</template>
