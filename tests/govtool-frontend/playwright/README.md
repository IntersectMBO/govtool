# 🚀 GovTool Integration Test Guide

## ✅ Prerequisites

Ensure the following tools are installed on your machine:

- **Git**
- **Node.js** & **npm**

---

## 🛠️ Setup Instructions

### 1. Clone the Repository

```bash
git clone https://github.com/IntersectMBO/govtool
```

### 2. Navigate to the Playwright Test Directory

```bash
cd govtool/tests/govtool-frontend/playwright
```

### 3. Configure Environment Variables

- Copy the example file:

  ```bash
  cp .env.example .env
  ```

- Update the `.env` file with the appropriate values for your environment.

### 4. Install Project Dependencies

```bash
npm install
```

### 5. Install Playwright Browsers

```bash
npx playwright install
```

---

## 🌐 Environment Setup

### HOST URL

| Environment | URL                                                                                                                      | Network |
| :---------- | :----------------------------------------------------------------------------------------------------------------------- | :------ |
| Development | [https://p80-z78acf3c2-zded6a792-gtw.z937eb260.rustrocks.fr](https://p80-z78acf3c2-zded6a792-gtw.z937eb260.rustrocks.fr) | Preview |
| QA          | [https://govtool.cardanoapi.io](https://govtool.cardanoapi.io)                                                           | Preview |
| Preview     | [https://preview.gov.tools](https://preview.gov.tools)                                                                   | Preview |
| Pre-Prod    | [https://pre-prod.gov.tools](https://pre-prod.gov.tools)                                                                 | Preprod |
| Mainnet     | [https://gov.tools](https://gov.tools)                                                                                   | Mainnet |

---

## 🔑 Generate API Keys

### Blockfrost API Key

- To generate a Blockfrost API key (Project ID):

  1. Follow the instructions in the [Blockfrost documentation](https://blockfrost.dev/overview/getting-started) 📚.
  2. The **Project ID** you create there serves as your **Blockfrost API Key**.
  3. Copy the **Project ID** and set it as `BLOCKFROST_API_KEY`.

  🔐 Note: Ensure you select the correct network for the **Project ID** that matches the host URL from the environment listed above.

### Kuber API Key

- To generate a Kuber API Key:
  1. Visit [Kuberide](https://kuberide.com/) 🌐.
  2. Log in using your Google or GitHub account.
  3. Navigate to **API Keys** ⚙️.
  4. Click to **Generate API Key** ✨.
  5. Copy the API key and set it as `KUBER_API_KEY`

---

## 🔧 Faucet wallet Configuration

This section guides you through generating a Cardano faucet wallet and configuring it for use. Follow the steps below to create and set the config on env

### Step 1: Generate a Faucet Wallet

Run the following command to generate a new faucet wallet:

```bash
npm run generate-faucet-wallet
```

The script will:

- Display the wallet details (payment private key, stake public key hash, and wallet address) in the terminal.

**Example Output:**

```
🎉 Wallet generated successfully!
-----------------------------------
🔑 Payment Private Key: <your-payment-private-key>
🔗 Stake Public Key Hash: <your-stake-pkh>
🏠 Wallet Address: <your-wallet-address>
-----------------------------------

📋 Please copy the following to your environment variables:
1. Set FAUCET_PAYMENT_PRIVATE=<your-payment-private-key>
2. Set FAUCET_STAKE_PKH=<your-stake-pkh>
3. Set FAUCET_ADDRESS=<your-wallet-address>

🎈 All set! Please ensure this wallet is funded with a sufficient balance
```

### Step 2: Configure Environment Variables

Securely store the generated wallet details in your environment variables. Add the following to your `.env` file or environment configuration:

```env
FAUCET_PAYMENT_PRIVATE=<your-payment-private-key>
FAUCET_STAKE_PKH=<your-stake-pkh>
FAUCET_ADDRESS=<your-wallet-address>
```

⚠️ **Security Note**: Store your wallet details in a secure location for future use. The payment private key is sensitive and must be protected to prevent unauthorized access to your funds.

### Step 3: Fund the Wallet

Ensure your wallet has enough funds for your test runs. The required balance depends on the specific tests you plan to execute (see test-specific details below).

To fund your wallet on the **Preview** or **Preprod** network:

1. Use the Cardano Testnet Faucet:  
   [https://docs.cardano.org/cardano-testnets/tools/faucet](https://docs.cardano.org/cardano-testnets/tools/faucet) 🌐  
   **Note**: There is a daily limit of **10,000 ADA** per wallet.

2. If the funded amount is insufficient, transfer additional ADA from another wallet. 💸

To check your wallet balance:

Visit:

```
https://${network}.cardanoscan.io/address/<your-wallet-address>
```

- Replace `${network}` with the appropriate network (e.g., `preprod` or `preview`).
- Replace `<your-wallet-address>` with your wallet address. 🔍

**Example**:

- For a preview wallet: `https://preview.cardanoscan.io/address/<your-wallet-address>`
- Monitor the balance to ensure it meets the requirements for individual or all test runs.

---

## 🧪 Running Tests

### 🔑 Generate Test Wallets

Before each test run, generate test wallets required for wallet-dependent tests:

```bash
npm run generate-wallets
```

---

### 🔁 Run All Tests

- **Pre-requisite**: Ensure the faucet address holds at least **412,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless
```

---

### ▶️ Run Specific Test Suites

Each test suite can be run in **UI** or **Headless** mode.

---

#### 1. **Delegation Pillar**

- **Pre-requisite**: Ensure the faucet address holds at least **12,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:delegation-pillar
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:delegation-pillar
```

---

#### 2. **Voting Pillar**

- **Pre-requisite**: Ensure the faucet address holds at least **12,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:voting-pillar
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:voting-pillar
```

---

#### 3. **Outcomes**

#### 🖥️ UI Mode

```bash
npm run test:outcomes
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:outcomes
```

---

#### 4. **Proposal Pillar**

_Includes both Proposal Discussion and Budget Discussion_

- **Pre-requisite**: Ensure the faucet address holds at least **403,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:proposal-pillar
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:proposal-pillar
```

---

#### 5. **Proposal Discussion**

- **Pre-requisite**: Ensure the faucet address holds at least **401,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:proposal-discussion
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:proposal-discussion
```

---

#### 6. **Proposal Budget**

- **Pre-requisite**: Ensure the faucet address holds at least **1,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:proposal-budget
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:proposal-budget
```

---

#### 7. **Wallet Connect**

#### 🖥️ UI Mode

```bash
npm run test:wallet-connect
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:wallet-connect
```

---

#### 8. **Usersnap Integration**

#### 🖥️ UI Mode

```bash
npm run test:usersnap
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:usersnap
```

---

#### 9. **Miscellaneous Tests**

#### 🖥️ UI Mode

```bash
npm run test:misc
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:misc
```

---

## 📊 Visualize Allure Report

To generate and view an Allure test report:

```bash
npm run allure:serve
```

---

## 📚 Additional Resources

For contribution guidelines and development tips, refer to:

📄 **[DEVELOPERS_GUIDE.md](./DEVELOPERS_GUIDE.md)**
