import { AxiosError } from 'axios'
import { Decoder, Encoder } from 'cbor-x'

export const cborxEncoder = new Encoder({
  mapsAsObjects: false,
  useRecords: false,
})

export function prepareErrorMessage(error: AxiosError | any) {
  const errorMsg = error.response?.data?.detail ?? ''

  if (errorMsg == 'Something went wrong' && error.response?.status == 500) {
    return 'An error occurred while submitting the transaction. Did you submit too many transactions too quickly! '
  } else if (error.response?.status) {
    return 'Api Responded with status Code : ' + error.response?.status + ': ' + errorMsg
  } else if (error.message) {
    return 'Something Went wrong when calling the api  :' + error.message
  } else {
    return 'Unknown error when creating/submitting proposal'
  }
}

export const cborxDecoder = new Decoder({ mapsAsObjects: false })
