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

| Environment | URL                                                                                                                      |
| :---------- | :----------------------------------------------------------------------------------------------------------------------- |
| Development | [https://p80-z78acf3c2-zded6a792-gtw.z937eb260.rustrocks.fr](https://p80-z78acf3c2-zded6a792-gtw.z937eb260.rustrocks.fr) |
| QA          | [https://govtool.cardanoapi.io](https://govtool.cardanoapi.io)                                                           |
| Preview     | [https://preview.gov.tools](https://preview.gov.tools)                                                                   |
| Pre-Prod    | [https://pre-prod.gov.tools](https://pre-prod.gov.tools)                                                                 |
| Mainnet     | [https://gov.tools](https://gov.tools)                                                                                   |

---

## 🔑 Generate API Keys

### Blockfrost API Key

- To generate a Blockfrost API key (Project ID):
  1. Follow the instructions in the [Blockfrost documentation](https://blockfrost.dev/overview/getting-started).
  2. The **Project ID** you create there serves as your **Blockfrost API Key**.

### Kuber API Key

- To generate a Kuber API Key:
  1. Visit [Kuberide](https://kuberide.com/).
  2. Log in using your Google or GitHub account.
  3. Navigate to **API Keys**.
  4. Click to **Generate API Key**.

---

## 🧪 Running Tests

### 🔑 Generate Test Wallets

Before each test run, generate fresh test wallets to avoid conflicts:

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

#### 1. **Delegation Pillars**

- **Pre-requisite**: Ensure the faucet address holds at least **12,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:delegation-pillars
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:delegation-pillars
```

---

#### 2. **Voting Pillars**

- **Pre-requisite**: Ensure the faucet address holds at least **12,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:voting-pillars
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:voting-pillars
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

#### 4. **Proposal Pillars**

_Includes both Proposal Discussion and Budget Discussion_

- **Pre-requisite**: Ensure the faucet address holds at least **403,000 ADA**.

#### 🖥️ UI Mode

```bash
npm run test:proposal-pillars
```

#### 🧪 Headless Mode (No UI)

```bash
npm run test:headless:proposal-pillars
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

📄 **[DEVELOPERS_GUIDE.md](/DEVELOPERS_GUIDE.md)**
