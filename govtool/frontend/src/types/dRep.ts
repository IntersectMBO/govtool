export type DRepDataFormValues = {
  doNotList: boolean;
  givenName: string;
  objectives: string;
  motivations: string;
  qualifications: string;
  image: string;
  paymentAddress: string;
  storeData?: boolean;
  storingURL: string;
  linkReferences?: Reference[];
  identityReferences?: Reference[];
};
