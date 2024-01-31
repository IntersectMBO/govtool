<script setup>
import config from '../config'
import { prepareErrorMessage } from '../utils'
</script>

<template>
  <v-sheet width="90%" class="mx-auto pa-12">
    <v-container fluid>
      <!--For displaying the title and description-->
      <div class="mb-4">
        <div class="text-h5">Governance Action Bulk Loader</div>
        <div class="text-grey mt-1">Submit to load the required number of a given action to sanchonet.</div>
      </div>

      <!--For selecting the action type and number of proposals-->
      <div class="mt-8">
        <v-select
          v-model="selectedAction"
          label="Governance Action To Load"
          :items="actionTypes"
          variant="outlined"
          v-on:update:model-value="onActionChanged"
        ></v-select>
        <v-text-field
          label="Number of Proposals"
          v-model="noOfProposal"
          class="number-input"
          variant="outlined"
          type="number"
        ></v-text-field>
      </div>

      <!--For submitting the form-->
      <v-btn class="mt-4" size="large" color="green" rounded="sm" @click="submitBulkLoad" :disabled="noOfProposal < 1 || loading">
        <span v-if="loading">Submitting...</span>
        <span v-else>Submit</span>
      </v-btn>

      <!--For displaying snackbar messages-->
      <v-snackbar v-model="snackbar" :color="snackbarColor" :timeout="snackbarTimeout">
        {{ snackbarMessage }}
      </v-snackbar>

      <!--For displaying loading indicator-->
      <div class="mt-4">
        <v-progress-linear v-if="loading" color="green" indeterminate></v-progress-linear>
      </div>

      <!--For displaying transaction ids-->
      <div v-if="txIds && txIds.length > 0">
        <div class="mt-2" v-if="txIds.length > 1">Proposals Created in Multiple transactions</div>
        <div class="mt-2" v-else>Proposals created in Single transaction</div>
        <ul class="mt-2 ml-4">
          <li v-for="(txid, i) in txIds" :key="txid">
            {{ i + 1 }}.
            <a :href="config.vvaWebappUrl + '/governance_actions/' + txid.tx_hash + '%230'" target="_blank"
              >{{ txid.tx_hash }}
            </a>
            [ <strong>{{ txid.proposal_count }}</strong> Proposals ]
          </li>
        </ul>
      </div>
    </v-container>
  </v-sheet>
</template>

<script>
import { submitMultipleProposals } from '../api'

export default {
  data() {
    return {
      actionTypes: ['Constitution', 'Info', 'Withdrawal', 'No-Confidence', 'Update-Committee', 'Hardfork', 'Update-Parameters'],
      selectedAction: 'Constitution',
      noOfProposal: 10,

      // Related to ui like loading snackbar etc.
      loading: false,
      success: false,
      snackbar: false,
      snackbarMessage: '',
      snackbarColor: '',
      snackbarTimeout: 3000,
      txIds: [],
    }
  },
  methods: {
    onActionChanged() {
      this.txIds = null
    },
    submitBulkLoad() {
      this.loading = true

      submitMultipleProposals(this.selectedAction.toLowerCase(), this.noOfProposal)
        .then((res) => {
          console.log(res)
          this.txIds = res.data
          this.loading = false
          this.snackbarTimeout = 3000
          this.snackbarMessage = 'Transactions submitted successfully.'
          this.snackbarColor = 'success'
          this.snackbar = true
        })
        .catch((error) => {
          console.error('Error Occured', error)
          this.snackbarTimeout = 20000
          this.loading = false
          this.snackbarMessage = prepareErrorMessage(error)
          this.snackbarColor = 'error'
          this.snackbar = true
        })
    },
  },
}
</script>

<style>
.number-input {
  width: 20%;
}
</style>
