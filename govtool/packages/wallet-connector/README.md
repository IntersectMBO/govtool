# Wallet Service

The WalletService.ts file contains the implementation of a wallet service, designed to manage wallet. It includes the enableWallet function responsible for enabling the wallet connection and allows get cip95 functions.

1. Install wallet-connector package.

```sh
yarn add wallet-connector
```

2. Import service from wallet-connector.

```javascript
import { WalletService } from "@wallet-connector";
```

3. Usage - for enable your browser wallet extension.
   EXAMPLE:

```javascript
const newWalletAPI = await WalletService.enableWallet("%WALLET_NAME%");
```

# Wallet Provider

WalletProvider component which serves as a React Context Provider to facilitate wallet integration across components.

1. Install wallet-connector package

```sh
yarn add wallet-connector
```

2. Import WalletProvider from wallet-connector and wrap your app.

```javascript
import { WalletProvider } from "@wallet-connector";

<WalletProvider>
  <App />
</WalletProvider>;
```

3. Usage

```javascript
import { useWalletContext } from "@wallet-connector";

const {
  disconnectWallet,
  enableError,
  enableWallet,
  isEnableLoading,
  walletAPI,
} = useWalletContext();
```
