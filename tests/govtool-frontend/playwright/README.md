# GitHub Pages Deployment SSH Deploy Key Generation Guide

This guide walks you through the process of generating an SSH deploy key for GitHub Pages deployment and securely integrating it into your workflow.

## Generating the SSH Deploy Key

Execute the following command in your terminal to generate the SSH deploy key:

```bash
ssh-keygen -t rsa -b 4096 -C "$(git config user.email)" -f gh-pages -N ""
```

This command will generate two files:

- `gh-pages.pub`: Public key
- `gh-pages`: Private key

## Integration Steps

1. **Repository Settings:**

   - Navigate to your repository settings.

2. **Adding Public Key to Deploy Keys: Add to** [`https://github.com/cardanoapi/govtool-test-reports`](https://github.com/cardanoapi/govtool-test-reports)

   - Go to Deploy Keys and add your public key. Make sure to grant it write access.

3. **Adding Private Key to Secrets:**
   - Go to Secrets and add your private key as `DEPLOY_KEY`.
