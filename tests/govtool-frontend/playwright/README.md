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
cd tests/govtool-frontend/playwright
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

## 🧪 Running Tests

### 🔑 Generate Test Wallets

Before each test run, generate fresh test wallets to avoid conflicts:

```bash
npm run generate-wallets
```

---

### 🔁 Run All Tests
- **Requirements**: Faucet API key and  Valid proposal faucet private keys

- **Headless mode** (no UI):

  ```bash
  npm run test
  ```

- **UI mode**:

  ```bash
  npm run test:headless
  ```

---

### ▶️ Run Specific Test Suites

Each test suite can be run in **UI** or **Headless** mode.

---

#### 1. **Delegation Pillars**
- **Requirements**: Faucet API key  
- **UI Mode**:
  ```bash
  npm run test:delegation-pillars
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:delegation-pillars
  ```

---

#### 2. **Voting Pillars**
- **Requirements**: Faucet API key  
- **UI Mode**:
  ```bash
  npm run test:voting-pillars
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:voting-pillars
  ```

---

#### 3. **Outcomes**
- **UI Mode**:
  ```bash
  npm run test:outcomes
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:outcomes
  ```

---

#### 4. **Proposal Pillars**  
_Includes both Proposal Discussion and Budget Discussion_  
- **Requirements**: Valid proposal faucet private keys  
- **UI Mode**:
  ```bash
  npm run test:proposal-pillars
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:proposal-pillars
  ```

---

#### 5. **Proposal Discussion**
- **Requirements**: Valid proposal faucet private keys  
- **UI Mode**:
  ```bash
  npm run test:proposal-discussion
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:proposal-discussion
  ```

---

#### 6. **Proposal Budget**
- **Requirements**: Valid proposal faucet private keys  
- **UI Mode**:
  ```bash
  npm run test:proposal-budget
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:proposal-budget
  ```

---

#### 7. **Wallet Connect**
- **UI Mode**:
  ```bash
  npm run test:wallet-connect
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:wallet-connect
  ```

---

#### 8. **Usersnap Integration**
- **UI Mode**:
  ```bash
  npm run test:usersnap
  ```
- **Headless Mode**:
  ```bash
  npm run test:headless:usersnap
  ```

---

#### 9. **Miscellaneous Tests**
- **UI Mode**:
  ```bash
  npm run test:misc
  ```
- **Headless Mode**:
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

📄 **`DEVELOPERS_GUIDE.md`**
