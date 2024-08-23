<script setup>
import config from '../config'
</script>
<template>
  <v-sheet width="90%" class="mx-auto pa-12">
    <v-container fluid>
      <!-- Helper title and subtext -->
      <div class="mb-4">
        <div class="text-h5">Specific Governance Action Loader</div>
        <div class="text-grey mt-1">Fill in the details according to specific action submit to sanchonet.</div>
      </div>
      <!-- Form for action loader -->
      <div class="mt-8">
        <!-- Governance Action selector -->
        <v-select
          v-model="selectedAction"
          label="Governance Action To Load"
          :items="actionTypes"
          variant="outlined"
          v-on:update:model-value="onActionChanged"
        ></v-select>

        <div v-if="selectedAction !== ''">
          <div class="mb-3 mt-3" v-if="selectedAction !== 'Info' && selectedAction !== 'Withdrawl'">
            <div><span class="text-h6 mb-2">Previous Gov Action ID</span><span class="text-caption"> (optional)</span></div>

            <v-text-field label="txHash#index" v-model="prevGovId" variant="outlined"></v-text-field>
          </div>

          <div><span class="text-h6">Proposal deposit</span><span class="text-caption"> (optional)</span></div>

          <div class="mb-3 mt-2">
            <v-text-field
              style="width: 300px"
              label="Ada amount"
              v-model="deposit"
              variant="outlined"
              type="number"
            ></v-text-field>
          </div>

          <div>
            <div class="text-h6 mb-2"></div>
            <v-tooltip text="Required" location="top">
              <template v-slot:activator="{ props }">
                <span class="text-h6 mb-2">Anchor</span>
                <span v-bind="props" class="text-red">*</span>
              </template>
            </v-tooltip>
            <v-text-field label="Anchor URL *" v-model="anchorUrl" variant="outlined"></v-text-field>
            <v-text-field label="Anchor Data Hash *" v-model="anchorDataHash" variant="outlined"></v-text-field>
          </div>
        </div>

        <!-- Constitution Section -->
        <div v-if="selectedAction == 'Constitution'">
          <v-tooltip text="Required" location="top">
            <template v-slot:activator="{ props }">
              <span class="text-h6 mb-2">Constitution</span>
              <span v-bind="props" class="text-red">*</span>
            </template>
          </v-tooltip>
          <v-text-field label="Constitution URL *" v-model="constitutionUrl" variant="outlined"></v-text-field>
          <v-text-field label="Constitution Data Hash *" v-model="constitutionDataHash" variant="outlined"></v-text-field>
        </div>

        <!-- Withdrawl section -->
        <div v-if="selectedAction == 'Withdrawal'">
          <v-tooltip text="Required" location="top">
            <template v-slot:activator="{ props }">
              <span class="text-h6 mb-2">Treasury Withdrawal</span>
              <span v-bind="props" class="text-red">*</span>
            </template>
          </v-tooltip>
          <div class="d-flex mt-2" v-for="(_, index) in treasuryWithdrawalAddresses" :key="index">
            <v-text-field
              class="stake"
              label="Withdraw Stake Address"
              v-model="treasuryWithdrawalAddresses[index]"
              variant="outlined"
            ></v-text-field>
            <v-text-field
              class="ml-6 number"
              label="Withdraw Amount (Lovelace)"
              v-model="treasuryAmounts[index]"
              variant="outlined"
              type="number"
            ></v-text-field>
            <v-btn color="red-lighten-5" variant="flat" class="mt-2 ml-6" @click="deleteTreasuryWithdrawal(index)">-</v-btn>
          </div>
          <div class="d-flex justify-center">
            <v-btn color="blue-lighten-5" variant="flat" @click="addTreasuryWithdrawal">+</v-btn>
          </div>
        </div>

        <!-- Add Committee section -->
        <div v-if="selectedAction == 'Update Committee'">
          <v-tooltip text="Required" location="top">
            <template v-slot:activator="{ props }">
              <span class="text-h6 mb-2">Quorum</span>
              <span v-bind="props" class="text-red">*</span>
            </template>
          </v-tooltip>
          <v-text-field
            class="q-number"
            label="Numerator"
            v-model="quorumNumerator"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="q-number"
            label="Denominator"
            v-model="quorumDenominator"
            variant="outlined"
            type="number"
          ></v-text-field>

          <div class="text-h6 mb-2 mt-5">Add To Committee</div>

          <div class="d-flex mt-2" v-for="(_, index) in addCommitteeFields" :key="index">
            <v-text-field
              class="stake"
              label="Stake Address / Key Hash *"
              v-model="addCommitteeFields[index]"
              variant="outlined"
            ></v-text-field>
            <v-text-field
              class="ml-6"
              label="Active Epoch *"
              v-model="addCommitteeEpochFields[index]"
              variant="outlined"
              type="number"
            ></v-text-field>
            <v-btn color="red-lighten-5" variant="flat" class="mt-2 ml-6" @click="deleteAddCommitteeField(index)">-</v-btn>
          </div>
          <div class="d-flex justify-center">
            <v-btn color="blue-lighten-5" variant="flat" @click="addAddCommitteeField">+</v-btn>
          </div>
        </div>

        <!-- Remove Committee section -->
        <div v-if="selectedAction == 'Update Committee'">
          <div class="text-h6 mb-2 mt-5">Remove From Committee</div>

          <div class="d-flex" v-for="(_, index) in removeCommitteeFields" :key="index">
            <v-text-field
              label="Stake Address / Key Hash *"
              v-model="removeCommitteeFields[index]"
              variant="outlined"
            ></v-text-field>
            <v-btn color="red-lighten-5" variant="flat" class="mt-2 ml-6" @click="deleteRemoveCommitteeField(index)">-</v-btn>
          </div>
          <div class="d-flex justify-center">
            <v-btn color="blue-lighten-5" variant="flat" @click="addRemoveCommitteeField">+</v-btn>
          </div>
        </div>

        <!-- Hardfork Section -->
        <div v-if="selectedAction == 'Hardfork'">
          <v-tooltip text="Required" location="top">
            <template v-slot:activator="{ props }">
              <span class="text-h6 mb-2">Protocol Version</span>
              <span v-bind="props" class="text-red">*</span>
            </template>
          </v-tooltip>
          <v-text-field
            class="protocolField"
            label="Major version"
            v-model="majorProtocol"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Minor version"
            v-model="minorProtocol"
            variant="outlined"
            type="number"
          ></v-text-field>
        </div>

        <!-- Update Parameters Section -->
        <div v-if="selectedAction == 'Update Parameters'">
          <v-tooltip text="Required" location="top">
            <template v-slot:activator="{ props }">
              <span class="text-h6 mb-2">Updated Parameter Fields</span>
              <span v-bind="props" class="text-red">*</span>
            </template>
          </v-tooltip>
          <v-text-field
            class="protocolField"
            label="Max Block Size"
            v-model="maxblocksize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field class="protocolField" label="Min Fee A" v-model="minFeeA" variant="outlined" type="number"></v-text-field>
          <v-text-field
            class="protocolField"
            label="Max BB Size"
            v-model="MaxBBSize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Max Tx Size"
            v-model="MaxTxSize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Max BH Size"
            v-model="MaxBHSize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Key Deposit"
            v-model="KeyDeposit"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Pool Deposit"
            v-model="PoolDeposit"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field class="protocolField" label="EMax" v-model="EMax" variant="outlined" type="number"></v-text-field>
          <v-text-field class="protocolField" label="NOpt" v-model="NOpt" variant="outlined" type="number"></v-text-field>
          <v-text-field class="protocolField" label="A0" v-model="A0" variant="outlined" type="number"></v-text-field>
          <v-text-field class="protocolField" label="Rho" v-model="Rho" variant="outlined" type="number"></v-text-field>
          <v-text-field class="protocolField" label="Tau" v-model="Tau" variant="outlined" type="number"></v-text-field>
          <v-text-field
            class="protocolField"
            label="Min Pool Cost"
            v-model="MinPoolCost"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="w-1/2 protocolField"
            label="Coins Per UTxO Byte"
            v-model="CoinsPerUTxOByte"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea
                label="Cost Models {PlutusV1:...,PlutusV2...}(Json)"
                v-model="CostModels"
                variant="outlined"
              ></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea label="Cost Models Example" v-model="CostModelsExample" readonly @focus="selectText" />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('costModels')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea label="Prices(Json)" v-model="Prices" variant="outlined"></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea label="Prices Example" v-model="PricesExample" readonly @focus="selectText" />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('prices')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea label="Max Tx Ex Units (Json)" v-model="MaxTxExUnits" variant="outlined"></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea label="Max Tx Ex Units example" v-model="MaxTxExUnitsExample" readonly @focus="selectText" />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('maxTxExUnits')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea label="Max Block Ex Units {memory:...}(Json)" v-model="MaxBlockExUnits" variant="outlined"></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea label="Max Block Ex Units Example" v-model="MaxBlockExUnitsExample" readonly @focus="selectText" />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('maxBlockExUnits')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-text-field
            class="protocolField"
            label="Max Val Size"
            v-model="MaxValSize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Collateral Percentage"
            v-model="CollateralPercentage"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Max Collateral Inputs"
            v-model="MaxCollateralInputs"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea label="Pool Voting Thresholds (Json)" v-model="PoolVotingThresholds" variant="outlined"></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea
                label="Pool Voting Thresholds Example"
                v-model="PoolVotingThresholdsExample"
                readonly
                @focus="selectText"
              />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('poolVotingThresholds')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-row align-content="space-between" justify="space-between">
            <v-col cols="12" sm="6">
              <v-textarea label="DRep Voting Thresholds (Json)" v-model="DRepVotingThresholds" variant="outlined"></v-textarea>
            </v-col>
            <v-col cols="12" sm="5">
              <v-textarea
                label="DRep Voting Thresholds Example"
                v-model="DRepVotingThresholdsExample"
                readonly
                @focus="selectText"
              />
            </v-col>
            <v-col cols="12" sm="1">
              <v-btn @click="copyExample('drepVotingThresholds')">Copy</v-btn>
            </v-col>
          </v-row>
          <v-text-field
            class="protocolField"
            label="Committee Min Size"
            v-model="CommitteeMinSize"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Committee Max Term Length"
            v-model="CommitteeMaxTermLength"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Gov Action Life Time"
            v-model="GovActionLifetime"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="Gov Action Deposit"
            v-model="GovActionDeposit"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="DRep Deposit"
            v-model="DRepDeposit"
            variant="outlined"
            type="number"
          ></v-text-field>
          <v-text-field
            class="protocolField"
            label="DRep Activity"
            v-model="DRepActivity"
            variant="outlined"
            type="number"
          ></v-text-field>
        </div>

        <!-- Submit button -->
        <v-btn class="mt-10" size="large" color="green" rounded="sm" @click="submitForm" :disabled="!selectedAction || loading">
          <span v-if="loading">Submitting...</span>
          <span v-else>Submit</span>
        </v-btn>

        <!--For displaying snackbar messages-->
        <v-snackbar v-model="snackbar" :color="snackbarColor" :timeout="snackbarTimeout" location="top">
          {{ snackbarMessage }}
        </v-snackbar>

        <!--For displaying loading indicator-->
        <div class="mt-4">
          <v-progress-linear v-if="loading" color="green" indeterminate></v-progress-linear>
        </div>

        <!--For displaying transaction id-->
        <div v-if="txid">
          <div>Transaction submitted successfully.</div>
          <div>
            Proposal ID :
            <a target="_blank" :href="config.vvaWebappUrl + '/governance_actions/' + txid + '%230'">{{ txid + '#0' }}</a>
          </div>
        </div>
      </div>
    </v-container>
  </v-sheet>
</template>

<script>
import { submitSingleProposal, submitSingleProposalMock } from '../api'
import { cborxDecoder, cborxEncoder, prepareErrorMessage } from '../utils'
import * as blake from 'blakejs'

export default {
  props: {
    selectedNetwork: String,
  },
  data() {
    return {
      // Data related to governance action form
      actionTypes: ['Constitution', 'Info', 'Withdrawal', 'No Confidence', 'Update Committee', 'Hardfork', 'Update Parameters'],
      selectedAction: '',
      prevGovId: null,
      deposit: null,
      anchorUrl: '',
      anchorDataHash: '',
      constitutionUrl: '',
      constitutionDataHash: '',
      treasuryWithdrawalAddresses: [''],
      treasuryAmounts: [''],
      noConfidence: true,
      check: true,
      removeCommitteeFields: [],
      addCommitteeFields: [],
      addCommitteeEpochFields: [],
      quorumNumerator: '',
      quorumDenominator: '',
      majorProtocol: '',
      minorProtocol: '',

      maxblocksize: null,
      minFeeA: null,
      MaxBBSize: null,
      MaxTxSize: null,
      MaxBHSize: null,
      KeyDeposit: null,
      PoolDeposit: null,
      EMax: null,
      NOpt: null,
      A0: null,
      Rho: null,
      Tau: null,
      MinPoolCost: null,
      CoinsPerUTxOByte: null,
      CostModels: null,
      CostModelsExample: `{"PlutusV1": [
                205665,
                812,
                1,
                1,
                1000,
                571,
                0,
                1,
                1000,
                24177,
                4,
                1,
                1000,
                32,
                117366,
                10475,
                4,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                100,
                100,
                23000,
                100,
                19537,
                32,
                175354,
                32,
                46417,
                4,
                221973,
                511,
                0,
                1,
                89141,
                32,
                497525,
                14068,
                4,
                2,
                196500,
                453240,
                220,
                0,
                1,
                1,
                1000,
                28662,
                4,
                2,
                245000,
                216773,
                62,
                1,
                1060367,
                12586,
                1,
                208512,
                421,
                1,
                187000,
                1000,
                52998,
                1,
                80436,
                32,
                43249,
                32,
                1000,
                32,
                80556,
                1,
                57667,
                4,
                1000,
                10,
                197145,
                156,
                1,
                197145,
                156,
                1,
                204924,
                473,
                1,
                208896,
                511,
                1,
                52467,
                32,
                64832,
                32,
                65493,
                32,
                22558,
                32,
                16563,
                32,
                76511,
                32,
                196500,
                453240,
                220,
                0,
                1,
                1,
                69522,
                11687,
                0,
                1,
                60091,
                32,
                196500,
                453240,
                220,
                0,
                1,
                1,
                196500,
                453240,
                220,
                0,
                1,
                1,
                806990,
                30482,
                4,
                1927926,
                82523,
                4,
                265318,
                0,
                4,
                0,
                85931,
                32,
                205665,
                812,
                1,
                1,
                41182,
                32,
                212342,
                32,
                31220,
                32,
                32696,
                32,
                43357,
                32,
                32247,
                32,
                38314,
                32,
                57996947,
                18975,
                10
            ],
            "PlutusV2": [
                205665,
                812,
                1,
                1,
                1000,
                571,
                0,
                1,
                1000,
                24177,
                4,
                1,
                1000,
                32,
                117366,
                10475,
                4,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                23000,
                100,
                100,
                100,
                23000,
                100,
                19537,
                32,
                175354,
                32,
                46417,
                4,
                221973,
                511,
                0,
                1,
                89141,
                32,
                497525,
                14068,
                4,
                2,
                196500,
                453240,
                220,
                0,
                1,
                1,
                1000,
                28662,
                4,
                2,
                245000,
                216773,
                62,
                1,
                1060367,
                12586,
                1,
                208512,
                421,
                1,
                187000,
                1000,
                52998,
                1,
                80436,
                32,
                43249,
                32,
                1000,
                32,
                80556,
                1,
                57667,
                4,
                1000,
                10,
                197145,
                156,
                1,
                197145,
                156,
                1,
                204924,
                473,
                1,
                208896,
                511,
                1,
                52467,
                32,
                64832,
                32,
                65493,
                32,
                22558,
                32,
                16563,
                32,
                76511,
                32,
                196500,
                453240,
                220,
                0,
                1,
                1,
                69522,
                11687,
                0,
                1,
                60091,
                32,
                196500,
                453240,
                220,
                0,
                1,
                1,
                196500,
                453240,
                220,
                0,
                1,
                1,
                1159724,
                392670,
                0,
                2,
                806990,
                30482,
                4,
                1927926,
                82523,
                4,
                265318,
                0,
                4,
                0,
                85931,
                32,
                205665,
                812,
                1,
                1,
                41182,
                32,
                212342,
                32,
                31220,
                32,
                32696,
                32,
                43357,
                32,
                32247,
                32,
                38314,
                32,
                35892428,
                10,
                57996947,
                18975,
                10,
                38887044,
                32947,
                10
            ]}`,
      Prices: null,
      PricesExample: `{
                "prMem": 5.77e-2,
                "prSteps": 7.21e-5
            }`,
      MaxTxExUnits: null,
      MaxTxExUnitsExample: `{
                "exUnitsMem": 14000000,
                "exUnitsSteps": 10000000000
            }`,
      MaxBlockExUnits: null,
      MaxBlockExUnitsExample: `{
                "exUnitsMem": 300000,
                "exUnitsSteps": 30000
            }`,
      MaxValSize: null,
      CollateralPercentage: null,
      MaxCollateralInputs: null,
      PoolVotingThresholds: null,
      PoolVotingThresholdsExample: `{
                "pvtCommitteeNoConfidence": 0.51,
                "pvtCommitteeNormal": 0.51,
                "pvtHardForkInitiation": 0.51,
                "pvtMotionNoConfidence": 0.51
            }`,
      DRepVotingThresholds: null,
      DRepVotingThresholdsExample: `{
                "dvtCommitteeNoConfidence": 0.51,
                "dvtCommitteeNormal": 0.51,
                "dvtHardForkInitiation": 0.51,
                "dvtMotionNoConfidence": 0.51,
                "dvtPPEconomicGroup": 0.51,
                "dvtPPGovGroup": 0.51,
                "dvtPPNetworkGroup": 0.51,
                "dvtPPTechnicalGroup": 0.51,
                "dvtTreasuryWithdrawal": 0.51,
                "dvtUpdateToConstitution": 0.51
            }`,
      CommitteeMinSize: null,
      CommitteeMaxTermLength: null,
      GovActionLifetime: null,
      GovActionDeposit: null,
      DRepDeposit: null,
      DRepActivity: null,

      // Related to ui like loading snackbar etc.
      loading: false,
      success: false,
      snackbar: false,
      snackbarMessage: '',
      snackbarColor: '',
      snackbarTimeout: 7000,
      txid: '',
    }
  },
  methods: {
    selectText(event) {
      event.target.select()
    },
    copyExample(key) {
      let value
      console.log(key)
      switch (key) {
        case 'CostModels':
          value = this.CostModelsExample
          break
        case 'prices':
          value = this.PricesExample
          break
        case 'drepVotingThresholds':
          value = this.DRepVotingThresholdsExample
          break
        case 'poolVotingThresholds':
          value = this.PoolVotingThresholdsExample
          break
        case 'maxTxExUnits':
          value = this.MaxTxExUnitsExample
          break
        case 'maxBlockExUnits':
          value = this.MaxBlockExUnitsExample
          break
        default:
          value = this.CostModelsExample
      }
      console.log(value)

      navigator.clipboard.writeText(value)
    },
    onActionChanged() {
      this.txid = ''
    },
    checkboxChanged() {
      console.log(this.check)
      this.check = true
    },
    addRemoveCommitteeField() {
      this.removeCommitteeFields.push('')
    },
    deleteRemoveCommitteeField(index) {
      this.removeCommitteeFields.splice(index, 1)
    },
    addAddCommitteeField() {
      this.addCommitteeFields.push('')
      this.addCommitteeEpochFields.push('')
    },
    deleteAddCommitteeField(index) {
      this.addCommitteeFields.splice(index, 1)
      this.addCommitteeEpochFields.splice(index, 1)
    },
    addTreasuryWithdrawal() {
      this.treasuryWithdrawalAddresses.push('')
      this.treasuryAmounts.push('')
    },
    deleteTreasuryWithdrawal(index) {
      this.treasuryWithdrawalAddresses.splice(index, 1)
      this.treasuryAmounts.splice(index, 1)
    },
    removeTreasuryWithdrawal(index) {
      this.treasuryWithdrawalAddresses.splice(index, 1)
      this.treasuryAmounts.splice(index, 1)
    },
    submitForm() {
      let proposal_data = {
        deposit: parseInt(this.deposit) * 1000000,
        anchor: {
          url: this.anchorUrl,
          dataHash: this.anchorDataHash,
        },
        ...(this.prevGovId != null ? { prevGovAction: this.prevGovId } : {}),
      }
      console.log(proposal_data)
      switch (this.selectedAction) {
        case 'Constitution':
          proposal_data['newconstitution'] = {
            url: this.constitutionUrl,
            dataHash: this.constitutionDataHash,
          }
          break
        case 'Info':
          break
        case 'Withdrawal':
          proposal_data['withdraw'] = this.treasuryWithdrawalAddresses.reduce((obj, key, index) => {
            obj[key] = parseInt(this.treasuryAmounts[index])
            return obj
          }, {})
          break
        case 'No Confidence':
          proposal_data['noconfidence'] = true
          break
        case 'Update Committee':
          const stakeAddrs = this.addCommitteeFields.reduce((obj, key, index) => {
            obj[key] = parseInt(this.addCommitteeEpochFields[index])
            return obj
          }, {})
          proposal_data['updatecommittee'] = {
            add: stakeAddrs,
            remove: this.removeCommitteeFields,
            qourum: {
              numerator: parseInt(this.quorumNumerator),
              denominator: parseInt(this.quorumDenominator),
            },
          }
          break
        case 'Hardfork':
          proposal_data['hardfork'] = {
            protocolVersion: {
              major: parseInt(this.majorProtocol),
              minor: parseInt(this.minorProtocol),
            },
          }
          break
        case 'Update Parameters':
          proposal_data['parameterupdate'] = {
            ...(this.maxblocksize != null ? { maxblocksize: parseInt(this.maxblocksize) } : {}),
            ...(this.minFeeA != null ? { minFeeA: parseInt(this.minFeeA) } : {}),
            ...(this.MaxBBSize != null ? { MaxBBSize: parseInt(this.MaxBBSize) } : {}),
            ...(this.MaxTxSize != null ? { MaxTxSize: parseInt(this.MaxTxSize) } : {}),
            ...(this.MaxBHSize != null ? { MaxBHSize: parseInt(this.MaxBHSize) } : {}),
            ...(this.KeyDeposit != null ? { KeyDeposit: parseInt(this.KeyDeposit) } : {}),
            ...(this.PoolDeposit != null ? { PoolDeposit: parseInt(this.PoolDeposit) } : {}),
            ...(this.EMax != null ? { EMax: parseInt(this.EMax) } : {}),
            ...(this.NOpt != null ? { NOpt: parseInt(this.NOpt) } : {}),
            ...(this.A0 != null ? { A0: parseFloat(this.A0) } : {}),
            ...(this.Rho != null ? { Rho: parseFloat(this.Rho) } : {}),
            ...(this.Tau != null ? { Tau: parseFloat(this.Tau) } : {}),
            ...(this.MinPoolCost != null ? { MinPoolCost: parseInt(this.MinPoolCost) } : {}),
            ...(this.CoinsPerUTxOByte != null ? { CoinsPerUTxOByte: parseInt(this.CoinsPerUTxOByte) } : {}),
            ...(this.CostModels != null ? { CostModels: JSON.parse(this.CostModels) } : {}),
            ...(this.Prices != null ? { Prices: JSON.parse(this.Prices) } : {}),
            ...(this.MaxTxExUnits != null ? { MaxTxExUnits: JSON.parse(this.MaxTxExUnits) } : {}),
            ...(this.MaxBlockExUnits != null ? { MaxBlockExUnits: JSON.parse(this.MaxBlockExUnits) } : {}),
            ...(this.MaxValSize != null ? { MaxValSize: parseInt(this.MaxValSize) } : {}),
            ...(this.CollateralPercentage != null ? { CollateralPercentage: parseInt(this.CollateralPercentage) } : {}),
            ...(this.MaxCollateralInputs != null ? { MaxCollateralInputs: parseInt(this.MaxCollateralInputs) } : {}),
            ...(this.PoolVotingThresholds != null ? { PoolVotingThresholds: JSON.parse(this.PoolVotingThresholds) } : {}),
            ...(this.DRepVotingThresholds != null ? { DRepVotingThresholds: JSON.parse(this.DRepVotingThresholds) } : {}),
            ...(this.CommitteeMinSize != null ? { CommitteeMinSize: parseInt(this.CommitteeMinSize) } : {}),
            ...(this.CommitteeMaxTermLength != null
              ? {
                  CommitteeMaxTermLength: parseInt(this.CommitteeMaxTermLength),
                }
              : {}),
            ...(this.GovActionLifetime != null ? { GovActionLifetime: parseInt(this.GovActionLifetime) } : {}),
            ...(this.GovActionDeposit != null ? { GovActionDeposit: parseInt(this.GovActionDeposit) } : {}),
            ...(this.DRepDeposit != null ? { DRepDeposit: parseInt(this.DRepDeposit) } : {}),
            ...(this.DRepActivity != null ? { DRepActivity: parseInt(this.DRepActivity) } : {}),
          }
          break
      }
      if (this.selectedNetwork == 'Mock Database') {
        console.log('Mock database')
        this.submitProposalToMock(this.selectedAction, proposal_data)
      } else {
        this.submitProposalViaKuber(proposal_data)
      }
    },
    submitProposalViaKuber(proposal_data) {
      this.loading = true
      submitSingleProposal(proposal_data)
        .then((res) => {
          const tx = res.data
          console.log('[Kuber] Tx: ' + JSON.stringify(tx))
          this.txid = tx.txId
          this.loading = false
          this.snackbarMessage = 'Transaction submitted successfully.'
          this.snackbarColor = 'success'
          this.snackbar = true
        })
        .catch((error) => {
          console.log(error)
          this.loading = false
          this.snackbarMessage = prepareErrorMessage(error)
          this.snackbarColor = 'error'
          this.snackbar = true
        })
    },
    submitProposalToMock(action, proposal_data) {
      this.loading = true
      submitSingleProposalMock(action, proposal_data)
        .then((res) => {
          console.log('Response: ' + res.data)
          // this.txid = tx.txId
          this.loading = false
          this.snackbarMessage = 'Transaction submitted successfully.'
          this.snackbarColor = 'success'
          this.snackbar = true
        })
        .catch((error) => {
          console.log(error)
          this.loading = false
          this.snackbarMessage = prepareErrorMessage(error)
          this.snackbarColor = 'error'
          this.snackbar = true
        })
    },
    get_transaction_details() {
      // Get the transaction from blockfrost
      specificLoaderApiGet(`/blockfrost/transaction/${tx.txId}`)
        .then((res) => {
          console.log(res.data)
        })
        .catch((error) => {
          console.log('An Error occured')
          console.log(error)
          this.loading = false
          this.snackbarMessage = 'An error occurred while loading the proposal.'
          this.snackbarColor = 'error'
          this.snackbar = true
        })
    },
  },
}
</script>

<style>
#noConfidence {
  width: 30px;
  height: 30px;
  margin-top: 4px;
}

.stake {
  width: 60%;
}

.protocolField {
  width: 25%;
}

.q-number {
  width: 25%;
}
</style>
