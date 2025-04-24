# **Developer's Guide**

This document serves as a guide for new developers participating in the project. It provides an overview of the project’s directory structure and describes the purpose of each component.

---

## **Directory Structure**

### `lib/`
Contains the utility logic, helpers, mock datas and shared services, including:
- _mock (mock data)
- constants
- datafactory
- fixtures
- forms
- Pages
- helpers
- Services

---

## **Test Directory: `tests/`**

This directory contains all automated test cases, organized by feature area:

### Feature-Based Test Suites

1. **`1-wallet-connect/`**  
   Wallet connection test cases.

2. **`2-delegation/`**  
   Delegation functionality and visibility tests for delegated wallets.

3. **`3-drep-registration/`**  
   dRep registration, editing functionality, validation, and visibility test cases.

4. **`4-proposal-visibility/`**  
   Voting visibility tests for voting pillars.

5. **`5-proposal-functionality/`**  
   Voting functionality tests and validation of voting metadata.

6. **`6-miscellaneous/`**  
   Miscellaneous tests, such as documentation URL checks.

7. **`7-proposal-submission/`**  
   Proposal submission form validation, visibility, draft handling, and submission tests.

8. **`8-proposal-discussion/`**  
   Tests related to proposal discussions adjacent to the submission form.

9. **`9-outcomes/`**  
   Tests related to proposal outcome visibility and validations.

10. **`10-user-snap/`**  
   Usersnap-related test cases.

11. **`11-proposal-budget/`**  
   Tests for proposal budget functionality and associated forms.

12. **`12-proposal-budget-submission/`**  
   Proposal budget submission form validation, draft, visibility, and submission tests.

---

### Authentication & Setup Scripts

13. **`auth.setup.ts`**  
   Sets up authentication for users and adaHolder wallets.

14. **`proposal-budget.auth.setup.ts`**  
   Authentication setup for proposal budget wallets.

15. **`proposal-discussion.auth.setup.ts`**  
   Authentication setup for proposal discussion wallets.

16. **`dRep.auth.setup.ts`**  
   Authentication setup for dRep wallets.

17. **`dRep.setup.ts`**  
   Setup for temporary and static dRep wallet registration.

18. **`proposal-budget.dRep.setup.ts`**  
   dRep wallet registration setup for proposal budget tests.

19. **`proposal.setup.ts`**  
   Setup for proposal submission wallets.

20. **`wallet.bootstrap.ts`**  
   Initializes adaHolder and dRep wallets with test funds for spendable ADA functionality.

---

### Teardown Scripts

21. **`delegation.teardown.ts`**  
   Resets delegation status (abstains delegation from delegated wallets).

22. **`faucet.setup.ts`**  
   Funds the test faucet wallet.

23. **`faucet.teardown.ts`**  
   Refunds all unspendable ADA from test wallets back to the proposal faucet.

24. **`dRep.teardown.ts`**  
   Handles deregistration of dRep wallets registered during tests or setup.
