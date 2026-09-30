# 🚀 GovTool Integration Test Guide

For CIP-179 user stories and deterministic tests without funded wallets or API keys, see [CIP-179 linked surveys](docs/cip179-user-stories.md). Run them with `npm run test:cip179` after installing frontend and test dependencies.

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

## 🔧 Wallet Configuration

Two secrets drive every wallet the tests use:

- `TEST_WALLET_MNEMONIC`: a 24-word mnemonic. Each test wallet is a CIP-1852 account derived from it by name, fresh for every run.
- `FAUCET_ADDRESS`, `FAUCET_PAYMENT_PRIVATE`, `FAUCET_STAKE_PRIVATE`: the faucet wallet, which funds the test wallets as tests need them.

No wallet keys are written to disk, and no wallets need to be generated before a run. To reuse a past run's wallets, set `HD_RUN_ID` to the id recorded in `lib/_mock/hdRun.json`.

### Step 1: Generate the Secrets

```bash
npm run generate-test-mnemonic
npm run generate-faucet-wallet
```

Each script prints the lines to add to your `.env` file:

```env
TEST_WALLET_MNEMONIC="<24 words>"
FAUCET_ADDRESS=<your-wallet-address>
FAUCET_PAYMENT_PRIVATE=<your-payment-private-key>
FAUCET_STAKE_PRIVATE=<your-stake-private-key>
```

⚠️ **Security Note**: Keep these values secret. Anyone holding the mnemonic or the faucet keys can spend the test funds.

### Step 2: Fund the Faucet Wallet

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

### 🔁 Run All Tests

- **Pre-requisite**: Ensure the faucet address holds at least **512,000 ADA**.

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

- **Pre-requisite**: Ensure the faucet address holds at least **501,000 ADA**.

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

#### 8. **Chatwoot Feedback Integration**

#### 🖥️ UI Mode

```bash
npm run test:chatwoot
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:chatwoot
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
