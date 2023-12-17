<template>
  <div id="app">
    <v-app class="bg-grey-lighten-4">
      <v-app-bar color="deep-purple" elevation="0">
        <img src="./assets/vite.svg" class="pl-4" height="40" />
        <v-toolbar-title>Voltaire Data Loader</v-toolbar-title>
        <!-- <v-container> -->
        <v-row>
          <v-col cols="12" sm="6" offset-sm="3">
            <v-select v-model="selectedNetwork" :items="networkOptions" label="Select Network" outlined dense></v-select>
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
      <div v-if="selectedNetwork == 'Sanchonet' && walletInfo.address" class="bg-light text-center mb-4">
        <v-tooltip text="This address pays the deposits and transaction fee" location="top">
          <template v-slot:activator="{ props }">
            <span v-bind="props" class="text-h6 mb-2">{{ walletInfo.address }}</span>
          </template>
        </v-tooltip>
        <div class="mt-1">
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
    const fetchInterval = 7000
    const updateBalance = async () => {
      getBalance()
        .then((response) => {
          if (response.data?.address) {
            this.walletInfo.address = response.data.address
            this.walletInfo.balance = response.data.totalValue
          }
          console.log('Wallet balance', response)
        })
        .finally(() => {
          this.timoutId = setTimeout(updateBalance, fetchInterval)
        })
    }
    Promise.resolve(updateBalance())
  },
  unmounted() {
    if (this.timeoutId) {
      clearTimeout(this.timeoutId)
    }
  },
  data() {
    return {
      tab: null,
      selectedNetwork: 'Sanchonet', // Default selection
      networkOptions: ['Mock Database', 'Sanchonet'],
      walletInfo: {
        address: null,
        balance: null,
      },
      timeoutId: null,
    }
  },
}
</script>

<style>
.tab {
  height: 115px;
}
</style>
