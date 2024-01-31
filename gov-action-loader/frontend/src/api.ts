import axios from 'axios'
import config from './config'

axios.defaults.headers.common['Content-Type'] = 'application/json'

export async function submitMultipleProposals(proposalType: string, noOfProposals: number) {
  const loadMultipleUrl = config.dataLoaderApi + '/api/load/multiple'
  const data = {
    proposal_type: proposalType,
    no_of_proposals: noOfProposals,
  }
  return await axios.post(loadMultipleUrl, data)
}

export async function getBalance() {
  const loadSingleUrl = config.dataLoaderApi + '/api/balance'
  return await axios.get(loadSingleUrl)
}
export async function submitSingleProposal(data?: any) {
  const loadSingleUrl = config.dataLoaderApi + '/api/load/single'
  return await axios.post(loadSingleUrl, data)
}

function generateRandomId(length: number) {
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
  let randomId = ''

  for (let i = 0; i < length; i++) {
    const randomIndex = Math.floor(Math.random() * characters.length)
    randomId += characters.charAt(randomIndex)
  }

  return randomId
}

export async function submitSingleProposalMock(action: String, data?: any) {
  const singleProposalendpoint = config.mockLoaderApi + '/specific/proposal/load'
  console.log('data', data)
  let payload
  switch (action) {
    case 'Constitution':
      payload = {
        details: {
          newConstitutionUrl: data['newconstitution'].url,
          newConstitutionHash: data['newconstitution'].dataHash,
        },
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(6),
        metadataHash: data['anchor'].dataHash,
        type: 'NewConstitution',
        url: data['anchor'].url,
      }
      break
    case 'Info':
      payload = {
        details: '',
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(6),
        metadataHash: data['anchor'].dataHash,
        type: 'Info',
        url: data['anchor'].url,
      }
      break
    case 'Withdrawl':
      let withdrawAddress = Object.keys(data['withdraw'])[0]

      payload = {
        details: {
          stakeAddress: withdrawAddress,
          amount: data['withdraw'][withdrawAddress],
        },
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(5),
        metadataHash: data['anchor'].dataHash,
        type: 'TreasuryWithdrawals',
        url: data['anchor'].url,
      }
      break
    case 'No Confidence':
      payload = {
        details: {
          noConfidence: true,
        },
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(5),
        metadataHash: data['anchor'].dataHash,
        type: 'No Confidence',
        url: data['anchor'].url,
      }
      break
    case 'Update Committee':
      payload = {
        details: {
          addStake: data['updatecommittee'].add,
          removeStake: data['updatecommittee'].remove,
        },
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(5),
        metadataHash: data['anchor'].dataHash,
        type: 'Update Committee',
        url: data['anchor'].url,
      }
      break
    case 'Hardfork':
      payload = {
        details: {
          majorProtocolParam: data['hardfork'].major,
          minorProtocolParam: data['hardfork'].minor,
        },
        expiryDate: '2024-12-23T03:18:41Z',
        id: generateRandomId(5),
        metadataHash: data['anchor'].dataHash,
        type: 'Hardfork',
        url: data['anchor'].url,
      }
      break
  }
  console.log(payload)
  return await axios.post(singleProposalendpoint, payload)
}
