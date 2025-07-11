<template>
  <div id="app">
    <v-app class="bg-grey-lighten-4">
      <v-app-bar color="deep-purple" elevation="0">
        <img src="./assets/vite.svg" class="pl-4" height="40" />
        <v-toolbar-title>Voltaire Data Loader</v-toolbar-title>
        <!-- <v-container> -->
        <v-row>
          <v-col cols="12" sm="6" offset-sm="3">
            <v-select v-model="selectedNetwork" :items="networkOptions" label="Select Network" outlined
              dense></v-select>
          </v-col>
        </v-row>
        <!-- </v-container> -->
      </v-app-bar>
      <div class="tab bg-deep-purple mb-4">
        <v-tabs slider-color="white" class="mt-16 mb-10" v-model="tab" align-tabs="center" bg-color="deep-purple">
          <v-tab value="single">Specific Load</v-tab>
          <v-tab value="multiple">Bulk Load</v-tab>
        </v-tabs>
      </div>
      <div class="bg-light text-center mb-4">
        <v-tooltip text="This address pays the deposits and transaction fee" location="top">
          <template v-slot:activator="{ props }">
            <span v-bind="props" class="text-h6 mb-2">{{ walletInfo.address }}</span>
          </template>
        </v-tooltip>
        <!-- Display loading message if loading is true -->
        <div v-if="loading" class="mt-1">
          <strong>Loading Balance ...</strong>
        </div>

        <!-- Display balance if loading is false -->
        <div v-else class="mt-1">
          <strong>{{ walletInfo.balance }} Ada</strong>
        </div>
      </div>

      <v-window v-model="tab">
        <v-window-item value="single">
          <SpecificLoad :selectedNetwork="selectedNetwork" />
        </v-window-item>
        <v-window-item value="multiple">
          <BulkLoad :selectedNetwork="selectedNetwork" />
        </v-window-item>
      </v-window>
    </v-app>
  </div>
</template>

<script>
import BulkLoad from './views/BulkLoad.vue'
import SpecificLoad from './views/SpecificLoad.vue'
import { getBalance } from './api'

export default {
  name: 'App',
  components: {
    BulkLoad,
    SpecificLoad,
  },
  mounted() {
    this.updateBalance()
  },
  unmounted() {
    if (this.timeoutId) {
      clearTimeout(this.timeoutId)
    }
  },
  data() {
    return {
      tab: null,
      selectedNetwork: 'Preview', // Default selection
      networkOptions: ['Preview', 'Preprod'],
      walletInfo: {
        address: null,
        balance: null,
      },
      timeoutId: null,
      fetchInterval: 7000,
      loading: true
    }
  },
  methods: {
    async updateBalance() {
      try {
        const response = await getBalance(this.selectedNetwork)
        if (response.data?.address) {
          this.walletInfo.address = response.data.address
          this.walletInfo.balance = response.data.totalValue
        }
        console.log('Wallet balance', response)
      } catch (error) {
        console.error('Error updating balance:', error)
      } finally {
        this.timeoutId = setTimeout(this.updateBalance, this.fetchInterval)
        this.loading = false;
      }
    },
  },
  watch: {
    selectedNetwork() {
      if (this.timeoutId) {
        clearTimeout(this.timeoutId)
      }
      this.loading = true;
      this.updateBalance()

    },
  },
}
</script>

<style>
.tab {
  height: 115px;
}
</style>
